{-# LANGUAGE OverloadedStrings #-}

module Main where

import           Control.Monad
import           Control.Concurrent             ( threadDelay )
import           Control.Concurrent.Async
import           Control.Concurrent.MVar
import           Data.Foldable
import           Data.Aeson
import           Data.Aeson.Types
import           Data.List
import qualified Data.Text                     as T
import           Data.Time.Clock
import qualified Network.AMQP                  as Q
import           Text.Printf
import           GHC.IO.Handle.FD
import           System.Log.Logger
import           System.Log.Handler.Simple

import           HomeDB
import           InfluxDB
import           UserDB
import           Collector
import           InfluxPublish                  ( publish )
import           Secrets
import           Types

hueBridgeApi = "api.meethue.com"
loggerName = "Collector"
influxDbName = "home"
influxMeasurement = "light"

main :: IO ()
main = do
    updateGlobalLogger rootLoggerName removeHandler
    updateGlobalLogger rootLoggerName $ setLevel DEBUG
    stdOutHandler <- verboseStreamHandler stdout DEBUG
    updateGlobalLogger rootLoggerName $ addHandler stdOutHandler

    maybeQueueConfig <- readUserNotificationQueueConfig

    case maybeQueueConfig of
        (Just queueConfig) -> do
            userNotificationVar <- newEmptyMVar
            dataEventsVar <- newEmptyMVar

            queueConnection <- Q.openConnection (hostname queueConfig)
                                                (vhost queueConfig)
                                                (username queueConfig)
                                                (password queueConfig)
            queueChannel    <- Q.openChannel queueConnection

            Q.declareQueue queueChannel $ Q.newQueue { Q.queueName = notiQueueName queueConfig }
            Q.declareQueue queueChannel $ Q.newQueue { Q.queueName = dataQueueName queueConfig }

            publishJob      <- async $ publishAll userNotificationVar dataEventsVar

            userNotificationJob <- async $ publishNotifications
                (userNotificationVar :: MVar MessageToUser)
                queueChannel
                (notiQueueName queueConfig)

            dataEventsJob <- async $ publishNotifications
                (dataEventsVar :: MVar SourceData)
                queueChannel
                (dataQueueName queueConfig)

            wait publishJob
        _ -> emergencyM loggerName "User notification queue config missing, not starting"

-- Waits for messages and pushes them onto the given channel
publishNotifications :: ToJSON a => MVar a -> Q.Channel -> T.Text -> IO ()
publishNotifications messageVar chan routingKey = forever $ do
    msg <- takeMVar messageVar
    Q.publishMsg chan "" routingKey $ Q.newMsg { Q.msgBody = encode msg } -- "" = default exchange

-- Publishes data from all sources to all sinks, for all users
publishAll :: MVar MessageToUser -> MVar SourceData -> IO ()
publishAll notificationVar dataVar = forever $ do
    infoM loggerName "Fetching users..."
    users <- getAllUsers

    forM_ users $ publishForUser notificationVar dataVar

    infoM loggerName "All done, soon looping again!"

    threadDelay (1000 * 1000 * 10) -- 10s

-- Publishes data from all user homes to all user sinks
publishForUser :: MVar MessageToUser -> MVar SourceData -> User -> IO ()
publishForUser notificationVar dataVar user = do
    infoM loggerName "Fetching verified user homes..."
    homes <- getUserHomes (userId user)

    infoM loggerName "Fetching all user sinks..."
    sinks <- getUserInfluxSinks (userId user)

    let userNotifyer = notify notificationVar user
    let dataQueuePusher = putMVar dataVar

    infoM loggerName "Collecting metrics..."
    forM_ homes (collectHome userNotifyer dataQueuePusher sinks)

    infoM loggerName "Done!"

notify :: MVar MessageToUser -> User -> Value -> IO ()
notify notificationVar user payload =
    putMVar notificationVar $ MessageToUser (userId user) payload

buildHomePayload :: Home -> IO Value
buildHomePayload home = do
    curTime <- getCurrentTime
    pure
        $ object
              [ "type" .= ("SourceCollected" :: String)
              , "time" .= curTime
              , "homeId" .= uuid home
              ]

buildSinkPayload :: InfluxSink -> IO Value
buildSinkPayload sink = do
    curTime <- getCurrentTime
    pure
        $ object
              [ "type" .= ("SinkFed" :: String)
              , "time" .= curTime
              , "influxId" .= influxUuid sink
              ]

type UserNotifier = Value -> IO ()
type DataQueuePusher = SourceData -> IO ()

-- Publishes data from the given home to each of the given sinks
collectHome :: UserNotifier -> DataQueuePusher -> [InfluxSink] -> Home -> IO ()
collectHome notifyUser notifyDataQueue sinks home = forM_ sinks $ \sink -> do
    let maybeToken    = accessToken home
    let maybeUsername = hueUsername home
    let maybeHomeId = uuid home
    case (maybeToken, maybeUsername, maybeHomeId) of
        (Just t, Just u, Just homeId) -> do
            -- Get the data
            lights <- collect hueBridgeApi u (Just t)
            infoM loggerName "Collected light data"

            -- Notify the user that we've collected it
            homePayload <- buildHomePayload home
            notifyUser homePayload

            -- Stick the data on the data queue
            notifyDataQueue $ SourceData {sourceId = homeId, datapoints = map toDataPoint lights}

            publish (T.pack $ influxHost sink)
                    (influxPort sink)
                    (T.pack $ influxUsername sink)
                    (T.pack $ influxPassword sink)
                    influxDbName
                    influxMeasurement
                    (map toDataPoint lights)

            sinkPayload <- buildSinkPayload sink
            notifyUser sinkPayload

            infoM loggerName "Published light data"
        _ -> warningM loggerName "Invalid home data, can't update home"
