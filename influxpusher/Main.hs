{-# LANGUAGE OverloadedStrings #-}

module Main where

import           Control.Concurrent.Async
import           Control.Concurrent.MVar
import           Control.Monad
import           Data.Aeson
import qualified Network.AMQP                  as Q
import           GHC.IO.Handle.FD
import           System.Log.Logger
import           System.Log.Handler.Simple

import           Secrets
import           Types

loggerName = "InfluxPusher"

main :: IO ()
main = do
    updateGlobalLogger rootLoggerName removeHandler
    updateGlobalLogger rootLoggerName $ setLevel DEBUG
    stdOutHandler <- verboseStreamHandler stdout DEBUG
    updateGlobalLogger rootLoggerName $ addHandler stdOutHandler

    maybeQueueConfig <- readUserNotificationQueueConfig
    case maybeQueueConfig of
        (Just queueConfig) -> do
            sourceDataMVar <- newEmptyMVar
            infoM loggerName "Waiting for data collection events"

            -- Listen for data, put it on the MVar
            receiveEvents queueConfig sourceDataMVar

            -- Push data from the MVar onto all sinks for the data owner
            forever $ forwardToInflux sourceDataMVar

        _ -> emergencyM loggerName "Notification queue config not found, refusing to start"

receiveEvents :: QueueConfig -> MVar SourceData -> IO ()
receiveEvents queueConfig dataVar = do
    conn <- Q.openConnection (hostname queueConfig)
                             (vhost queueConfig)
                             (username queueConfig)
                             (password queueConfig)
    chan <- Q.openChannel conn
    Q.declareQueue chan $ Q.newQueue { Q.queueName = dataQueueName queueConfig }

    Q.consumeMsgs chan (dataQueueName queueConfig) Q.NoAck $ \(msg, envelope) -> do
        let parsedMsg = eitherDecode $ Q.msgBody msg
        case parsedMsg of
             (Right s@(SourceData _ _)) -> putMVar dataVar s
             (Left err) -> warningM loggerName $ "Ignoring malformed message " ++ err

    return ()


forwardToInflux :: MVar SourceData -> IO ()
forwardToInflux dataVar = do
    res <- takeMVar dataVar
    infoM loggerName "Pushing to Influx!"
