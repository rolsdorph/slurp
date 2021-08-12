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

import           SimpleSourceDB
import           HomeDB
import           UserDB
import           Collector
import           Secrets
import           Types
import           UserNotification
import qualified SimpleSource as SS
import Control.Monad.Except (runExceptT)

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
    forM_ users $ publishSSForUser notificationVar dataVar

    infoM loggerName "All done, soon looping again!"

    threadDelay (1000 * 1000 * 10) -- 10s

-- Publishes data from all user simple sources to the data queue
publishSSForUser :: MVar MessageToUser -> MVar SourceData -> User -> IO ()
publishSSForUser notificationVar dataVar user = do
    maybeSources <- runExceptT (getUserSimpleSources $ userId user)

    case maybeSources of
      (Left err) -> errorM loggerName (show err)
      (Right sources) -> do
          let userNotifyer = notify notificationVar $ userId user
          let dataQueuePusher = putMVar dataVar

          infoM loggerName "Collecting simple sources..."
          forM_ sources (collectSimpleSource userNotifyer dataQueuePusher)

          infoM loggerName "Collected simple sources!"

collectSimpleSource
    :: UserNotifier -> DataQueuePusher -> SimpleShallowJsonSource -> IO ()
collectSimpleSource notifyUser notifyDataQueue source = do
    sourceData <- SS.collect (errorM loggerName) source

    case sourceData of
         (Left err) -> errorM loggerName $ "Failed to collect data: " <> err
         (Right sd) -> do
            -- Notify the user that we've collected it
            sourcePayload <- buildSimpleSourcePayload source
            notifyUser sourcePayload

            -- Stick the data on the data queue
            notifyDataQueue sd

            infoM loggerName "Published simple data"


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
              , "sourceId" .= uuid home
              ]

buildSimpleSourcePayload :: SimpleShallowJsonSource -> IO Value
buildSimpleSourcePayload source = do
    curTime <- getCurrentTime
    pure
        $ object
              [ "type" .= ("SourceCollected" :: String)
              , "time" .= curTime
              , "sourceId" .= genericSourceId source
              ]

type DataQueuePusher = SourceData -> IO ()

-- Publishes data from the given home to the data queue
collectHome :: UserNotifier -> DataQueuePusher -> Home -> IO ()
collectHome notifyUser notifyDataQueue home = do
    let maybeToken    = accessToken home
    let maybeUsername = hueUsername home
    case (maybeToken, maybeUsername) of
        (Just token, Just username) -> do
            -- Get the data
            lights <- collect hueBridgeApi username (Just token)
            infoM loggerName "Collected light data"

            -- Notify the user that we've collected it
            homePayload <- buildHomePayload home
            notifyUser homePayload

            -- Stick the data on the data queue
            notifyDataQueue $ SourceData { sourceId      = uuid home
                                         , sourceOwnerId = ownerId home
                                         , datakey       = homeDataKey home
                                         , datapoints = map toDataPoint lights
                                         }

            infoM loggerName "Published light data"
        _ -> warningM loggerName
                      "Username or token missing, not able to collect home"
