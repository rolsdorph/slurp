{-# LANGUAGE OverloadedStrings #-}

module Main where

import           Control.Concurrent.Async
import           Control.Concurrent.MVar
import           Control.Monad
import           Data.Aeson
import qualified Data.Text                     as T
import           Data.Time.Clock
import qualified Database.InfluxDB.Types       as I
import qualified Network.AMQP                  as Q
import           GHC.IO.Handle.FD
import           System.Log.Logger
import           System.Log.Handler.Simple

import           HomeDB
import           InfluxDB
import           Secrets
import           Types
import           InfluxPublish                  ( publish )
import           UserNotification

loggerName = "InfluxPusher"
influxDbName = "home" -- TODO: Move to the database

main :: IO ()
main = do
    updateGlobalLogger rootLoggerName removeHandler
    updateGlobalLogger rootLoggerName $ setLevel DEBUG
    stdOutHandler <- verboseStreamHandler stdout DEBUG
    updateGlobalLogger rootLoggerName $ addHandler stdOutHandler

    maybeQueueConfig <- readUserNotificationQueueConfig
    case maybeQueueConfig of
        (Just queueConfig) -> do
            -- Inter-thread communication
            userNotificationVar <- newEmptyMVar
            sourceDataMVar      <- newEmptyMVar

            -- AMQP config
            queueConnection     <- Q.openConnection (hostname queueConfig)
                                                    (vhost queueConfig)
                                                    (username queueConfig)
                                                    (password queueConfig)
            queueChannel <- Q.openChannel queueConnection

            Q.declareQueue queueChannel
                $ Q.newQueue { Q.queueName = notiQueueName queueConfig }
            Q.declareQueue queueChannel
                $ Q.newQueue { Q.queueName = dataQueueName queueConfig }

            -- Listen for user notifications, forward them to the queue
            userNotificationJob <- async $ publishNotifications
                (userNotificationVar :: MVar MessageToUser)
                queueChannel
                (notiQueueName queueConfig)

            -- Listen for data, put it on the MVar
            receiveEvents queueConfig sourceDataMVar

            infoM loggerName "Waiting for data collection events"

            -- Push data from the MVar onto all sinks for the data owner
            forever $ forwardToInflux sourceDataMVar userNotificationVar

        _ -> emergencyM
            loggerName
            "Notification queue config not found, refusing to start"

-- Waits for incoming AMQP events, puts valid ones on the given MVar
receiveEvents :: QueueConfig -> MVar SourceData -> IO ()
receiveEvents queueConfig dataVar = do
    conn <- Q.openConnection (hostname queueConfig)
                             (vhost queueConfig)
                             (username queueConfig)
                             (password queueConfig)
    chan <- Q.openChannel conn
    Q.declareQueue chan $ Q.newQueue { Q.queueName = dataQueueName queueConfig }

    Q.consumeMsgs chan (dataQueueName queueConfig) Q.NoAck
        $ \(msg, envelope) -> do
              let parsedMsg = eitherDecode $ Q.msgBody msg
              case parsedMsg of
                  (Right s@(SourceData _ _ _ _)) -> putMVar dataVar s
                  (Left err) ->
                      warningM loggerName $ "Ignoring malformed message " ++ err

    return ()

-- Waits for new source data, pushes it onto all relevant user sinks
forwardToInflux :: MVar SourceData -> MVar MessageToUser -> IO ()
forwardToInflux dataVar userNotificationVar = do
    sourceData <- takeMVar dataVar
    let userNotifier = notify userNotificationVar $ sourceOwnerId sourceData

    sinks <- getUserInfluxSinks $ sourceOwnerId sourceData
    forM_ sinks $ pushToSink sourceData userNotifier

-- Pushes the given data points to the given Influx sink
pushToSink :: SourceData -> UserNotifier -> InfluxSink -> IO ()
pushToSink dataToPublish userNotifier sink = do
    publish (T.pack $ influxHost sink)
            (influxPort sink)
            (T.pack $ influxUsername sink)
            (T.pack $ influxPassword sink)
            influxDbName
            (I.Measurement $ T.pack (datakey dataToPublish))
            (datapoints dataToPublish)

    sinkPayload <- buildSinkPayload sink
    userNotifier sinkPayload
    infoM loggerName "Pushed to a sink"

-- Builds a user notification event payload for a fetch of the given sink
buildSinkPayload :: InfluxSink -> IO Value
buildSinkPayload sink = do
    curTime <- getCurrentTime
    pure $ object
        [ "type" .= ("SinkFed" :: String)
        , "time" .= curTime
        , "sinkId" .= influxUuid sink
        ]
