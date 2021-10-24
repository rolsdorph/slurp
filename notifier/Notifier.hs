{-# LANGUAGE OverloadedStrings #-}

module Notifier where

import qualified Auth
import Control.Concurrent.MVar
import Control.Exception
import Control.Monad
import Data.Aeson
import qualified Data.ByteString.Lazy as L
import qualified Data.List as List
import Data.UUID.V4
import GHC.IO.Handle.FD
import qualified Network.AMQP as Q
import Control.Monad.Reader (ReaderT, ask, liftIO, runReaderT)
import Network.WebSockets
import RabbitMQ
import System.Log.Handler.Simple
import System.Log.Logger (infoM, warningM, updateGlobalLogger, rootLoggerName, removeHandler, setLevel, Priority (DEBUG), addHandler, emergencyM)
import Types
import Secrets (readUserNotificationQueueConfig)
import qualified Database.HDBC.Sqlite3 as DB
import Control.Concurrent.Async (async, wait)
import Control.Concurrent.STM.TSem (TSem, signalTSem)
import Control.Concurrent.STM (atomically)

loggerName :: String
loggerName = "Notifier"

type ConnId = String

data UserConnection = UserConnection
  { connId :: ConnId,
    user :: User,
    connection :: WSConnection
  }
  deriving (Show)

newtype WSConnection = WSConnection {getConnection :: Connection}

instance Show WSConnection where
  show _ = "A Connection"

type UserConnections = [UserConnection]

emptyConnectionList :: [UserConnection]
emptyConnectionList = []

-- Starts tracking a new connection
addConnection :: UserConnection -> UserConnections -> UserConnections
addConnection newConnection connections = newConnection : connections

-- Stops tracking a connection
removeConnection :: UserConnection -> UserConnections -> UserConnections
removeConnection userConn = List.filter (\c -> connId c /= removeId)
  where
    removeId = connId userConn

configureLogging :: IO ()
configureLogging = do
  updateGlobalLogger rootLoggerName removeHandler
  updateGlobalLogger rootLoggerName $ setLevel DEBUG
  stdOutHandler <- verboseStreamHandler stdout DEBUG
  updateGlobalLogger rootLoggerName $ addHandler stdOutHandler

main' :: TSem -> DB.Connection -> IO ()
main' ready conn = do
  maybeQueueConfig <- readUserNotificationQueueConfig
  case maybeQueueConfig of
    (Just queueConfig) -> runReaderT (app ready conn) queueConfig
    _ -> emergencyM Notifier.loggerName "Notification queue config not found, refusing to start"

app :: TSem -> DB.Connection -> ReaderT QueueConfig IO ()
app ready conn = do
  queueConfig <- ask
  consumerRegistry <- liftIO $ createConsumerRegistry queueConfig
  notifierThread <- liftIO . async $ Notifier.run ((`runReaderT` conn) <$> Auth.verifyToken) consumerRegistry
  liftIO . atomically $ signalTSem ready
  liftIO $ wait notifierThread

run :: Auth.TokenVerifier -> ConsumerRegistry -> IO ()
run verifyToken consumerRegistry = do
  configureLogging

  infoM loggerName "Listening at 127.0.0.1:8090"

  connections <- newMVar emptyConnectionList

  -- Listen for events to forward
  forwardEvents connections consumerRegistry

  -- Listen for WebSocket events
  runServer "127.0.0.1" 8090 (wsApp connections verifyToken)

forwardEvents :: MVar UserConnections -> ConsumerRegistry -> IO ()
forwardEvents connectionsVar registerConsumer = do
  registerConsumer $ \msg -> do
    let parsedMsg = eitherDecode $ Q.msgBody msg
    case parsedMsg of
      (Right (MessageToUser target payload')) -> do
        connections <- readMVar connectionsVar

        infoM loggerName $ "Notifying " ++ target

        sendToUserId target connections (encode payload')
      (Left err) -> warningM loggerName $ "Ignoring malformed message " ++ err

sendToUserId :: UserId -> UserConnections -> L.ByteString -> IO ()
sendToUserId target connections message =
  forM_
    (userIdConnections target connections)
    (\c -> sendDataMessage (getConnection $ connection c) (Text message Nothing))

userIdConnections :: String -> UserConnections -> UserConnections
userIdConnections target = List.filter (\u -> userId (user u) == target)

wsApp :: MVar UserConnections -> Auth.TokenVerifier -> ServerApp
wsApp connectionVar verifyToken pendingConnection = do
  -- Javascript WS API doesn't support specifying headers... we have to accept everything
  conn <- acceptRequest pendingConnection

  -- Wait for a valid authentication message from the client
  authResult <- waitForAuth 3 conn verifyToken

  case authResult of
    (Left err) -> do
      infoM loggerName $ "Authentication unsuccessful: " ++ err
      sendClose conn ("Authentication attempts exhausted" :: L.ByteString)
    (Right authenticatedUser) -> do
      infoM loggerName "Authentication successful :)"

      connectionId <- show <$> nextRandom
      let userConn = UserConnection connectionId authenticatedUser (WSConnection conn)

      -- Start tracking the connection
      modifyMVar_ connectionVar $
        \connections -> pure $ addConnection userConn connections

      -- Listen until disconnect, then remove the connection
      removeOnDisconnect connectionVar userConn

-- Gives the connection the given number of attempts to send a WebSocket data message that
-- contains a valid auth token
type AuthAttempts = Int

waitForAuth :: AuthAttempts -> Connection -> Auth.TokenVerifier -> IO (Either String User)
waitForAuth attemptsLeft conn verifyToken
  | attemptsLeft <= 0 = return $ Left "Authentication attempts exhausted"
  | otherwise = do
    msg <- receiveDataMessage conn
    authRes <- attemptAuth verifyToken msg
    case authRes of
      (Right authenticatedUser) -> pure $ Right authenticatedUser
      _ -> waitForAuth (attemptsLeft - 1) conn verifyToken

attemptAuth :: Auth.TokenVerifier -> DataMessage -> IO (Either String User)
attemptAuth verifyToken (Text token _) = verifyToken token
attemptAuth _ _ = pure $ Left "Unexpected auth payload"

-- Listens to the given socket, removing it from the connection list upon disconnect
removeOnDisconnect :: MVar UserConnections -> UserConnection -> IO ()
removeOnDisconnect connectionsVar userConn =
  finally (waitForDisconnect (connection userConn)) $ do
    infoM loggerName "User disconnected"

    modifyMVar_ connectionsVar $ \currentConnections ->
      pure $ removeConnection userConn currentConnections

-- Keeps the connection open until an exception (i.e. a disconnect) occurs
waitForDisconnect :: WSConnection -> IO ()
waitForDisconnect conn = forever $ receiveDataMessage (getConnection conn)
