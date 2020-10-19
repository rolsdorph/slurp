{-# LANGUAGE OverloadedStrings #-}

module Main where

import           Control.Exception
import           Control.Monad
import           Control.Concurrent.Async
import           Control.Concurrent             ( threadDelay )
import           Control.Concurrent.MVar
import           Data.Aeson
import qualified Data.ByteString               as B
import qualified Data.ByteString.Lazy          as L
import qualified Data.CaseInsensitive          as CI
import qualified Data.List                     as List
import           Data.UUID.V4
import qualified Network.AMQP                  as Q
import           Network.WebSockets

import           Auth
import           Secrets
import           Types

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
    maybeQueueConfig <- readUserNotificationQueueConfig
    case maybeQueueConfig of
        (Just queueConfig) -> do
            print "127.0.0.1:8090"

            connections <- newMVar emptyConnectionList

            -- Listen for events to forward
            forwardEvents connections queueConfig

            -- Listen for WebSocket events
            runServer "127.0.0.1" 8090 (app connections)
        _ -> putStrLn "Notification queue config not found, refusing to start"

forwardEvents :: MVar UserConnections -> QueueConfig -> IO ()
forwardEvents connectionsVar queueConfig = do
    conn <- Q.openConnection (hostname queueConfig)
                             (vhost queueConfig)
                             (username queueConfig)
                             (password queueConfig)
    chan <- Q.openChannel conn
    Q.declareQueue chan $ Q.newQueue { Q.queueName = (queueName queueConfig) }

    Q.consumeMsgs chan (queueName queueConfig) Q.NoAck $ \(msg, envelope) -> do
        let parsedMsg = eitherDecode $ Q.msgBody msg
        case parsedMsg of
            (Right (MessageToUser targetUserId payload)) -> do
                connections <- readMVar connectionsVar

                print $ "Notifying " ++ targetUserId

                sendToUserId targetUserId connections (encode payload)
            (Left err) -> print $ "Ignoring malformed message " ++ err

    return ()

sendToUserId :: String -> UserConnections -> L.ByteString -> IO ()
sendToUserId targetUserId connections message = forM_
    (userIdConnections targetUserId connections)
    (\c -> sendDataMessage (connection c) (Text message Nothing))

userIdConnections :: String -> UserConnections -> UserConnections
userIdConnections target = List.filter (\u -> userId (user u) == target)

app :: MVar UserConnections -> ServerApp
app connectionVar pendingConnection = do
    -- Javascript WS API doesn't support specifying headers... we have to accept everything
    connection <- acceptRequest pendingConnection

    -- Wait for a valid authentication message from the client
    authResult <- waitForAuth 3 connection

    case authResult of
        (Left  err ) -> print $ "Authentication unsuccessful: " ++ err
        (Right user) -> do
            print "Authentication successful :)"

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
waitForAuth :: AuthAttempts -> Connection -> IO (Either String User)
waitForAuth attemptsLeft conn
    | attemptsLeft <= 0 = return $ Left "Authentication attempts exhausted"
    | otherwise = do
        msg     <- receiveDataMessage conn
        authRes <- attemptAuth msg
        case authRes of
            (Just user) -> pure $ Right user
            _           -> waitForAuth (attemptsLeft - 1) conn

attemptAuth :: DataMessage -> IO (Maybe User)
attemptAuth (Text token _) = verifyToken token

-- Listens to the given socket, removing it from the connection list upon disconnect
removeOnDisconnect :: MVar UserConnections -> UserConnection -> IO ()
removeOnDisconnect connectionsVar userConn =
    finally (waitForDisconnect (connection userConn)) $ do
        print "User disconnected"

        modifyMVar_ connectionsVar $ \currentConnections ->
            pure $ removeConnection userConn currentConnections

-- Keeps the connection open until an exception (i.e. a disconnect) occurs
waitForDisconnect :: Connection -> IO ()
waitForDisconnect connection = forever $ receiveDataMessage connection

