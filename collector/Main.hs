{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

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
import           HueHome
import           Secrets
import           Types
import           UserNotification
import qualified SimpleSource as SS
import Control.Monad.Except (runExceptT)
import Network.HTTP.Req (runReq, defaultHttpConfig, req, header, GET (..), NoReqBody (..), lbsResponse, responseBody, HttpException)
import Control.Exception (catch)
import Control.Monad.Reader (ReaderT, asks, liftIO, runReaderT)

hueBridgeApi = "api.meethue.com"
loggerName = "Collector"

newtype CollectorStack a = CollectorStack { runCollector :: IO a }
  deriving (Functor, Applicative, Monad)

instance HasHttp CollectorStack where
  simpleGet url options = CollectorStack $ do
    catch
      ( runReq defaultHttpConfig $ do
          res <-
            req
              GET
              url
              NoReqBody
              lbsResponse
              options
          return . Right $ responseBody res
      )
      (\ex -> return . Left $ "Failed to collect simple source: " ++ show (ex :: HttpException))

instance HasLogger CollectorStack where
  infoLog  = CollectorStack . infoM loggerName
  errorLog = CollectorStack . errorM loggerName

main :: IO ()
main = do
  updateGlobalLogger rootLoggerName removeHandler
  updateGlobalLogger rootLoggerName $ setLevel DEBUG
  stdOutHandler <- verboseStreamHandler stdout DEBUG
  updateGlobalLogger rootLoggerName $ addHandler stdOutHandler

  maybeConfig <- readUserNotificationQueueConfig
  case maybeConfig of
    (Just config) -> runReaderT app config
    Nothing -> emergencyM loggerName "User notification queue missing, not starting"

app :: ReaderT QueueConfig IO ()
app = do
    host' <- asks hostname
    vhost' <- asks vhost
    username' <- asks username
    password' <- asks password
    notificationsQueue <- asks notiQueueName
    dataQueue <- asks dataQueueName

    userNotificationVar <- liftIO newEmptyMVar
    dataEventsVar <- liftIO newEmptyMVar

    queueConnection <- liftIO $ Q.openConnection host' vhost' username' password'
    queueChannel    <- liftIO $ Q.openChannel queueConnection

    liftIO $ Q.declareQueue queueChannel $ Q.newQueue { Q.queueName = notificationsQueue }
    liftIO $ Q.declareQueue queueChannel $ Q.newQueue { Q.queueName = dataQueue }

    publishJob      <- liftIO . async $ publishAll userNotificationVar dataEventsVar

    userNotificationJob <- liftIO . async $ publishNotifications
        (userNotificationVar :: MVar MessageToUser)
        queueChannel
        notificationsQueue

    dataEventsJob <- liftIO . async $ publishNotifications
        (dataEventsVar :: MVar SourceData)
        queueChannel
        dataQueue

    liftIO $ wait publishJob

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
    sourceData <- runCollector $ SS.collect source

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
            maybeLights <- runCollector $ collect hueBridgeApi username (Just token)
            case maybeLights of
              (Right lights) -> do
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
              (Left err) -> errorM loggerName err
        _ -> warningM loggerName
                      "Username or token missing, not able to collect home"
