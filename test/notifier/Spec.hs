{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}

import qualified Auth
import Control.Concurrent (forkIO)
import Control.Exception (Exception, throwIO, try)
import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy as L
import Data.Text (Text)
import Data.Time.Calendar (Day (ModifiedJulianDay))
import Data.Time.Clock (UTCTime (..), secondsToDiffTime)
import Network.WebSockets (Connection, ControlMessage (..), Message (..), runClient)
import Network.WebSockets.Connection (receive, sendTextData)
import qualified Notifier
import System.Timeout (timeout)
import Test.Hspec (Spec, around, describe, hspec, it, shouldBe, shouldSatisfy)
import Types

type Seconds = Int

toMicroseconds :: Seconds -> Int
toMicroseconds = (*) 1000000

setupTimeout :: Seconds
setupTimeout = 5

testWsHost :: String
testWsHost = "127.0.0.1"

testWsPort :: Int
testWsPort = 8090

testUserNotQueue :: Text
testUserNotQueue = "test-user-nots"

testDataQueue :: Text
testDataQueue = "test-data"

testQc :: QueueConfig
testQc =
  QueueConfig
    { hostname = "127.0.0.1",
      vhost = "/",
      username = "huemetricswebsite",
      password = "huemetricspassword",
      notiQueueName = testUserNotQueue,
      dataQueueName = testDataQueue
    }

data TestUser = TestUser {user :: User, token :: L.ByteString}

userListVerifier :: [TestUser] -> Auth.TokenVerifier
userListVerifier knownUsers attemptedToken = case filter (matchesToken attemptedToken) knownUsers of
  (u : _) -> pure $ Right (user u)
  _ -> pure $ Left "Unknown token"

matchesToken :: L.ByteString -> TestUser -> Bool
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

main :: IO ()
main = do
  _ <- forkIO $ Notifier.run testQc (userListVerifier [user1, user2, user3])

  putStrLn $ "Waiting for up to " ++ show setupTimeout ++ " seconds for WS connection to be ready"
  connReady <- timeout (toMicroseconds setupTimeout) waitForConnectionReady
  case connReady of
    Just () -> hspec spec
    _ -> throwIO $ ConnNotReady setupTimeout

spec :: Spec
spec = do
  around withWs $ do
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

tryConnection :: IO (Either IOError ())
tryConnection = try $ runClient testWsHost testWsPort "/" (const (return ()))

waitForConnectionReady :: IO ()
waitForConnectionReady = do
  tryConnection >>= \case
    (Right _) -> return ()
    _ -> waitForConnectionReady

withWs :: (Connection -> IO a) -> IO a
withWs = runClient testWsHost testWsPort "/"

isClose :: Maybe Message -> Bool
isClose (Just (ControlMessage (Close _ _))) = True
isClose _ = False

assertNoMessage :: Connection -> IO ()
assertNoMessage conn = timeout (toMicroseconds 1) (receive conn) >>= \res -> res `shouldBe` Nothing

assertCloseMessage :: Connection -> IO ()
assertCloseMessage conn = timeout (toMicroseconds 1) (receive conn) >>= \msg -> msg `shouldSatisfy` isClose

newtype SetupFailure = ConnNotReady Int

instance Show SetupFailure where
  show (ConnNotReady s) = "Connection not ready within " ++ show s ++ " seconds"

instance Exception SetupFailure
