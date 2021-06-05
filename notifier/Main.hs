{-# LANGUAGE OverloadedStrings #-}

module Main where

import           Control.Exception
import           Control.Monad
import           Control.Concurrent.MVar
import           Data.Aeson
import qualified Data.ByteString.Lazy          as L
import qualified Data.List                     as List
import           Data.UUID.V4
import qualified Network.AMQP                  as Q
import           Network.WebSockets
import           GHC.IO.Handle.FD
import           System.Log.Logger
import           System.Log.Handler.Simple

import qualified Auth
import           Secrets
import           Types

loggerName = "Notifier"

type ConnId = String
data UserConnection = UserConnection {
    connId :: ConnId,
    user :: User,
    connection :: Connection }
                    deriving Show

instance Show Connection where
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
    where removeId = connId userConn

main :: IO ()
main = do
    updateGlobalLogger rootLoggerName removeHandler
    updateGlobalLogger rootLoggerName $ setLevel DEBUG
    stdOutHandler <- verboseStreamHandler stdout DEBUG
    updateGlobalLogger rootLoggerName $ addHandler stdOutHandler

    maybeQueueConfig <- readUserNotificationQueueConfig
    case maybeQueueConfig of
        (Just queueConfig) -> run queueConfig Auth.verifyToken
        _ -> emergencyM loggerName "Notification queue config not found, refusing to start"

run :: QueueConfig -> Auth.TokenVerifier -> IO ()
run queueConfig verifyToken = do
    infoM loggerName "Listening at 127.0.0.1:8090"

    connections <- newMVar emptyConnectionList

    -- Listen for events to forward
    forwardEvents connections queueConfig

    -- Listen for WebSocket events
    runServer "127.0.0.1" 8090 (app connections verifyToken)

forwardEvents :: MVar UserConnections -> QueueConfig -> IO ()
forwardEvents connectionsVar queueConfig = do
    conn <- Q.openConnection (hostname queueConfig)
                             (vhost queueConfig)
                             (username queueConfig)
                             (password queueConfig)
    chan <- Q.openChannel conn
    Q.declareQueue chan $ Q.newQueue { Q.queueName = notiQueueName queueConfig }

    Q.consumeMsgs chan (notiQueueName queueConfig) Q.NoAck $ \(msg, envelope) -> do
        let parsedMsg = eitherDecode $ Q.msgBody msg
        case parsedMsg of
            (Right (MessageToUser targetUserId payload)) -> do
                connections <- readMVar connectionsVar

                infoM loggerName $ "Notifying " ++ targetUserId

                sendToUserId targetUserId connections (encode payload)
            (Left err) -> warningM loggerName $ "Ignoring malformed message " ++ err

    return ()

sendToUserId :: String -> UserConnections -> L.ByteString -> IO ()
sendToUserId targetUserId connections message = forM_
    (userIdConnections targetUserId connections)
    (\c -> sendDataMessage (connection c) (Text message Nothing))

userIdConnections :: String -> UserConnections -> UserConnections
userIdConnections target = List.filter (\u -> userId (user u) == target)

app :: MVar UserConnections -> Auth.TokenVerifier -> ServerApp
app connectionVar verifyToken pendingConnection = do
    -- Javascript WS API doesn't support specifying headers... we have to accept everything
    connection <- acceptRequest pendingConnection

    -- Wait for a valid authentication message from the client
    authResult <- waitForAuth 3 connection verifyToken

    case authResult of
        (Left  err ) -> infoM loggerName $ "Authentication unsuccessful: " ++ err
        (Right user) -> do
            infoM loggerName "Authentication successful :)"

            connId <- show <$> nextRandom
            let userConn = UserConnection connId user connection

            -- Start tracking the connection
            modifyMVar_ connectionVar
                $ \connections -> pure $ addConnection userConn connections

            -- Listen until disconnect, then remove the connection
            removeOnDisconnect connectionVar userConn

-- Gives the connection the given number of attempts to send a WebSocket data message that
-- contains a valid auth token
type AuthAttempts = Int
waitForAuth :: AuthAttempts -> Connection -> Auth.TokenVerifier -> IO (Either String User)
waitForAuth attemptsLeft conn verifyToken
    | attemptsLeft <= 0 = return $ Left "Authentication attempts exhausted"
    | otherwise = do
        msg     <- receiveDataMessage conn
        authRes <- attemptAuth verifyToken msg
        case authRes of
            (Right user) -> pure $ Right user
            _            -> waitForAuth (attemptsLeft - 1) conn verifyToken

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
waitForDisconnect :: Connection -> IO ()
waitForDisconnect connection = forever $ receiveDataMessage connection

