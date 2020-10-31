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
import           UserDB
import           Collector
import           Secrets
import           Types
import           UserNotification

hueBridgeApi = "api.meethue.com"
loggerName = "Collector"

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

-- Publishes data from all user sources to the data queue
publishAll :: MVar MessageToUser -> MVar SourceData -> IO ()
publishAll notificationVar dataVar = forever $ do
    infoM loggerName "Fetching users..."
    users <- getAllUsers

    forM_ users $ publishForUser notificationVar dataVar

    infoM loggerName "All done, soon looping again!"

    threadDelay (1000 * 1000 * 10) -- 10s

-- Publishes data from all user homes to the data queue
publishForUser :: MVar MessageToUser -> MVar SourceData -> User -> IO ()
publishForUser notificationVar dataVar user = do
    infoM loggerName "Fetching verified user homes..."
    homes <- getUserHomes (userId user)

    let userNotifyer = notify notificationVar $ userId user
    let dataQueuePusher = putMVar dataVar

    infoM loggerName "Collecting metrics..."
    forM_ homes (collectHome userNotifyer dataQueuePusher)

    infoM loggerName "Done!"

buildHomePayload :: Home -> IO Value
buildHomePayload home = do
    curTime <- getCurrentTime
    pure
        $ object
              [ "type" .= ("SourceCollected" :: String)
              , "time" .= curTime
              , "homeId" .= uuid home
              ]

type DataQueuePusher = SourceData -> IO ()

-- Publishes data from the given home to the data queue
collectHome :: UserNotifier -> DataQueuePusher -> Home -> IO ()
collectHome notifyUser notifyDataQueue home = do
    let maybeToken    = accessToken home
    let maybeUsername = hueUsername home
    let maybeHomeId = uuid home
    let maybeDatakey = homeDataKey home
    case (maybeToken, maybeUsername, maybeHomeId, maybeDatakey) of
        (Just t, Just u, Just homeId, Just d) -> do
            -- Get the data
            lights <- collect hueBridgeApi u (Just t)
            infoM loggerName "Collected light data"

            -- Notify the user that we've collected it
            homePayload <- buildHomePayload home
            notifyUser homePayload

            -- Stick the data on the data queue
            notifyDataQueue $ SourceData {sourceId = homeId, datakey = d, datapoints = map toDataPoint lights}

            infoM loggerName "Published light data"
        _ -> warningM loggerName "Invalid home data, can't update home"
