{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}

import qualified Auth
import Control.Concurrent (MVar, ThreadId, forkIO, killThread, modifyMVar, modifyMVar_, newEmptyMVar, newMVar, putMVar, takeMVar, withMVar)
import Control.Exception (Exception, bracket, throwIO, try)
import Control.Monad.Cont (forever, void)
import Data.Aeson (FromJSON (..), ToJSON (..), Value, decode, encode, object, withObject, (.:), (.=))
import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy as L
import Data.Map (fromList)
import Data.Text (Text)
import Data.Time.Calendar (Day (ModifiedJulianDay))
import Data.Time.Clock (UTCTime (..), secondsToDiffTime)
import qualified Network.AMQP as Q
import Network.WebSockets (Connection, ControlMessage (..), DataMessage (Text), Message (..), runClient)
import Network.WebSockets.Connection (receive, receiveDataMessage, sendTextData)
import qualified Notifier
import RabbitMQ
import System.Timeout (timeout)
import Test.Hspec (Spec, around, describe, hspec, it, shouldBe, shouldSatisfy)
import Types

type Seconds = Int

toMicroseconds :: Seconds -> Int
toMicroseconds = (*) 1000000

-- * Test configuration variables

setupTimeout :: Seconds
setupTimeout = 5

testWsHost :: String
testWsHost = "127.0.0.1"

testWsPort :: Int
testWsPort = 8090

-- * The actual tests

main :: IO ()
main = hspec spec

spec :: Spec
spec = do
  around withWsAndServer $ do
    describe "Authentication flow" $ do
      it "Closes the connection after three failed attempts" $ \conn -> do
        -- Two incorrect tokens is okay:
        sendTextData conn ("i-am-wrong-token" :: BS.ByteString)
        sendTextData conn ("i-am-wrong-token" :: BS.ByteString)
        assertNoMessage conn

        -- But the third one triggers a Close control frame:
        sendTextData conn ("i-am-wrong-token" :: BS.ByteString)
        assertCloseMessage conn

      it "Keeps the connection open upon successful authentication" $ \conn -> do
        sendTextData conn ("i-am-wrong-token" :: BS.ByteString)
        sendTextData conn ("i-am-wrong-token" :: BS.ByteString)
        sendTextData conn (token user1)
        sendTextData conn ("some-client-data" :: BS.ByteString)
        assertNoMessage conn

  around withThreeWsAndQC $ do
    describe "Notification routing" $ do
      it "Routes notifications to the correct WebSockets" $ \(w1, w2, w3, queueConsumer) -> do
        -- User 1 registers on WS #1 and WS #2, user 2 registers on WS #3, user 3 doesn't register
        sendTextData w1 (token user1)
        sendTextData w2 (token user1)
        sendTextData w3 (token user2)

        w1Msgs <- newMVar []
        w2Msgs <- newMVar []
        w3Msgs <- newMVar []

        forkIO $ recordMessages w1Msgs w1
        forkIO $ recordMessages w2Msgs w2
        forkIO $ recordMessages w3Msgs w3

        -- We send a message to each user. User 1 should receive it on both its websockets.
        queueConsumer (Q.Message {Q.msgBody = messageUser (user user1) "Hello, user 1"})
        queueConsumer (Q.Message {Q.msgBody = messageUser (user user2) "Hello, user 2"})
        queueConsumer (Q.Message {Q.msgBody = messageUser (user user3) "Hello, user 3"})

        takeMVar w1Msgs >>= \msgs -> print msgs >> (msgs `shouldSatisfy` onlyContainsMessage "Hello, user 1")
        takeMVar w2Msgs >>= \msgs -> print msgs >> (msgs `shouldSatisfy` onlyContainsMessage "Hello, user 1")
        takeMVar w3Msgs >>= \msgs -> print msgs >> (msgs `shouldSatisfy` onlyContainsMessage "Hello, user 2")

-- * Test data

data TestUser = TestUser {user :: User, token :: L.ByteString}

userListVerifier :: [TestUser] -> Auth.TokenVerifier
userListVerifier knownUsers attemptedToken = case filter (matchesToken attemptedToken) knownUsers of
  (u : _) -> pure $ Right (user u)
  _ -> pure $ Left "Unknown token"
  where
    matchesToken t u = token u == t

user1 :: TestUser
user1 =
  TestUser
    { token = "user-1-token",
      user =
        User
          { userId = "user-1-id",
            userCreatedAt = UTCTime (ModifiedJulianDay 0) (secondsToDiffTime 0),
            authType = Insecure,
            thirdPartyId = Just "third-party-user-1"
          }
    }

user2 :: TestUser
user2 =
  TestUser
    { token = "user-2-token",
      user =
        User
          { userId = "user-2-id",
            userCreatedAt = UTCTime (ModifiedJulianDay 4) (secondsToDiffTime 0),
            authType = Insecure,
            thirdPartyId = Just "third-party-user-2"
          }
    }

user3 :: TestUser
user3 =
  TestUser
    { token = "user-3-token",
      user =
        User
          { userId = "user-3-id",
            userCreatedAt = UTCTime (ModifiedJulianDay 15) (secondsToDiffTime 0),
            authType = Insecure,
            thirdPartyId = Just "third-party-user-3"
          }
    }

-- * Helper functions for setting up the notifier

newtype SetupFailure = SetupTimeout Int

instance Show SetupFailure where
  show (SetupTimeout s) = "Notifier not ready within " ++ show s ++ " seconds"

instance Exception SetupFailure

tryConnection :: IO (Either IOError ())
tryConnection = try $ runClient testWsHost testWsPort "/" (const (return ()))

waitForConnectionReady :: IO ()
waitForConnectionReady = do
  tryConnection >>= \case
    (Right _) -> return ()
    _ -> waitForConnectionReady

startServer :: IO (ThreadId, QueueConsumer)
startServer = do
  queueConsumer <- newEmptyMVar
  serverThread <- forkIO $ Notifier.run (userListVerifier [user1, user2, user3]) (putMVar queueConsumer)

  putStrLn $ "Waiting for up to " ++ show setupTimeout ++ " seconds for WS connection to be ready"
  connReady <- timeout (toMicroseconds setupTimeout) waitForConnectionReady

  putStrLn $ "Waiting for up to " ++ show setupTimeout ++ " seconds for queue consumer to be ready"
  queueConsumerReady <- timeout (toMicroseconds setupTimeout) (takeMVar queueConsumer)
  case (connReady, queueConsumerReady) of
    (Just (), Just consumer) -> return (serverThread, consumer)
    _ -> throwIO $ SetupTimeout setupTimeout

killServer :: (ThreadId, QueueConsumer) -> IO ()
killServer (threadId, _) = killThread threadId

withWs :: (Connection -> IO a) -> IO a
withWs = runClient testWsHost testWsPort "/"

withWsAndServer :: (Connection -> IO a) -> IO a
withWsAndServer t = bracket startServer killServer (\_ -> withWs t)

withThreeWsAndQC :: ((Connection, Connection, Connection, QueueConsumer) -> IO a) -> IO a
withThreeWsAndQC f = bracket startServer killServer (\(_, qc) -> withWs $ \w1 -> withWs $ \w2 -> withWs $ \w3 -> f (w1, w2, w3, qc))

-- * Helper functions for recording and asserting on WebSocket messages

-- Notifier deals with JSON payloads, this type is here to make the test integrate smoothly with that:
newtype TestMessage = TestMessage {msg :: String} deriving (Eq, Show)

instance FromJSON TestMessage where
  parseJSON = withObject "TestMessage" $ \v ->
    TestMessage <$> v .: "msg"

instance ToJSON TestMessage where
  toJSON (TestMessage msg) = object ["msg" .= msg]

-- Construct the RMQ payload for sending a notification to a user
messageUser :: User -> String -> L.ByteString
messageUser target payload = encode $ MessageToUser (userId target) (toJSON (TestMessage payload))

-- Put all data messages received on the given connection onto the mutable list
recordMessages :: MVar [DataMessage] -> Connection -> IO ()
recordMessages msgStore conn = forever $ do
  receiveDataMessage conn >>= \msg -> modifyMVar_ msgStore (\store -> return $ msg : store)

onlyContainsMessage :: String -> [DataMessage] -> Bool
onlyContainsMessage desiredText [msg] = isMessage desiredText msg
onlyContainsMessage _ _ = False

isMessage :: String -> DataMessage -> Bool
isMessage desired (Text msg _) = decode msg == Just (TestMessage desired)
isMessage _ _ = False

assertNoMessage :: Connection -> IO ()
assertNoMessage conn = timeout (toMicroseconds 1) (receive conn) >>= \res -> res `shouldBe` Nothing

assertCloseMessage :: Connection -> IO ()
assertCloseMessage conn = timeout (toMicroseconds 1) (receive conn) >>= \msg -> msg `shouldSatisfy` isClose
  where
    isClose (Just (ControlMessage (Close _ _))) = True
    isClose _ = False
