{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE FlexibleContexts #-}

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
import qualified Data.ByteString.Lazy          as LB
import qualified Network.AMQP                  as Q
import           Text.Printf
import           GHC.IO.Handle.FD
import           System.Log.Logger
import           System.Log.Handler.Simple

import qualified SimpleSourceDB
import qualified HomeDB
import qualified UserDB
import           HueHome
import           Secrets
import           Types
import           UserNotification
import qualified SimpleSource as SS
import Control.Monad.Except (runExceptT, MonadIO, ExceptT)
import Network.HTTP.Req (runReq, defaultHttpConfig, req, header, GET (..), NoReqBody (..), lbsResponse, responseBody, HttpException)
import Control.Exception (catch)
import Control.Monad.Reader (ReaderT, asks, liftIO, runReaderT, MonadReader, ask)

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

data Env = Env {
    envGetAllUsers :: IO [User]
  , envGetUserSs :: String -> ExceptT LB.ByteString IO [SimpleShallowJsonSource]
  , envGetUserHomes :: String -> IO [Home]
  , envCollectHome :: Home -> IO (Either String SourceData)
  , envCollectSs :: SimpleShallowJsonSource -> IO (Either String SourceData)
  , envLogInfo :: String -> IO ()
  , envLogError :: String -> IO ()
  , envPushData :: SourceData -> IO ()
  , envNotify :: UserId -> Value -> IO ()
  , envPublishNotification :: MessageToUser -> IO ()
  , envPublishData :: SourceData -> IO ()
}

class HasUsers a where
  getAllUsers :: a -> IO [User]

class HasSimpleSources a where
  getUserSimpleSources :: a -> (String -> ExceptT LB.ByteString IO [SimpleShallowJsonSource])

class HasHomes a where
  getUserHomes :: a -> (String -> IO [Home])

class HasSimpleSourceCollector a where
  getSsCollector :: a -> (SimpleShallowJsonSource -> IO (Either String SourceData))

class HasHueHomeCollector a where
  getHomeCollector :: a -> (Home -> IO (Either String SourceData))

class HasIOLogger a where
  getInfoLog :: a -> String -> IO ()
  getErrorLog :: a -> String -> IO ()

logInfo :: (MonadIO m, MonadReader env m, HasIOLogger env) => String -> m ()
logInfo s = do
  logger <- asks getInfoLog
  liftIO $ logger s

logError :: (MonadIO m, MonadReader env m, HasIOLogger env) => String -> m ()
logError s = do
  logger <- asks getErrorLog
  liftIO $ logger s

class CanPushData a where
  getDataPusher :: a -> DataQueuePusher

class CanNotifyUsers a where
  getUserNotifier :: a -> (UserId -> Value -> IO())

instance HasUsers Env where
  getAllUsers = envGetAllUsers

instance HasSimpleSources Env where
  getUserSimpleSources = envGetUserSs

instance HasHomes Env where
  getUserHomes = envGetUserHomes

instance HasSimpleSourceCollector Env where
  getSsCollector = envCollectSs

instance HasHueHomeCollector Env where
  getHomeCollector = envCollectHome

instance HasIOLogger Env where
  getInfoLog = envLogInfo
  getErrorLog = envLogError

instance CanPushData Env where
  getDataPusher = envPushData

instance CanNotifyUsers Env where
  getUserNotifier = envNotify

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

    let env = Env {
        envGetAllUsers = UserDB.getAllUsers
      , envGetUserSs = SimpleSourceDB.getUserSimpleSources
      , envGetUserHomes = HomeDB.getUserHomes
      , envCollectHome = runCollector . HueHome.collect
      , envCollectSs = runCollector . SS.collect
      , envLogInfo = infoM loggerName
      , envLogError = errorM loggerName
      , envPushData = putMVar dataEventsVar
      , envNotify = notify userNotificationVar
      , envPublishNotification = rmqPushFunction queueChannel notificationsQueue
      , envPublishData = rmqPushFunction queueChannel dataQueue
    }

    publishJob      <- liftIO . async $ runReaderT publishAll env

    userNotificationJob <- liftIO . async $ publishNotifications
        (userNotificationVar :: MVar MessageToUser)
        (envPublishNotification env)

    dataEventsJob <- liftIO . async $ publishNotifications
        (dataEventsVar :: MVar SourceData)
        (envPublishData env)

    liftIO $ wait publishJob

-- Publishes data from all user sources to the data queue
publishAll :: (MonadIO m, MonadReader Env m) => m ()
publishAll = forever $ do
    userGetter <- asks getAllUsers

    logInfo "Fetching users..."
    users <- liftIO userGetter

    forM_ users publishForUser
    forM_ users publishSSForUser

    logInfo "All done, soon looping again!"

    liftIO $ threadDelay (1000 * 1000 * 10) -- 10s

-- Publishes data from all user simple sources to the data queue
publishSSForUser :: (MonadIO m,
                     MonadReader env m,
                     HasIOLogger env,
                     CanNotifyUsers env,
                     CanPushData env,
                     HasSimpleSources env,
                     HasSimpleSourceCollector env) => User -> m ()
publishSSForUser user = do
    env <- ask
    maybeSources <- liftIO . runExceptT $ getUserSimpleSources env (userId user)

    case maybeSources of
      (Left err) -> logError (show err)
      (Right sources) -> do
          logInfo "Collecting simple sources..."
          forM_ sources collectSimpleSource

          logInfo "Collected simple sources!"

collectSimpleSource
    :: (MonadIO m,
        MonadReader env m,
        HasIOLogger env,
        CanNotifyUsers env,
        CanPushData env,
        HasSimpleSourceCollector env) => SimpleShallowJsonSource -> m ()
collectSimpleSource source = do
    env <- ask
    sourceData <- liftIO $ getSsCollector env source

    case sourceData of
         (Left err) -> logError $ "Failed to collect data: " <> err
         (Right sd) -> do
            -- Notify the user that we've collected it
            sourcePayload <- liftIO $ buildSimpleSourcePayload source
            liftIO $ getUserNotifier env (sourceOwnerId sd) sourcePayload

            -- Stick the data on the data queue
            liftIO $ getDataPusher env sd

            logInfo "Published simple data"


-- Publishes data from all user homes to the data queue
publishForUser :: (MonadIO m
                  , MonadReader env m
                  , HasIOLogger env
                  , HasHomes env
                  , CanNotifyUsers env
                  , CanPushData env
                  , HasHueHomeCollector env) =>
                  User -> m ()
publishForUser user = do
    env <- ask

    logInfo "Fetching verified user homes..."
    homes <- liftIO $ getUserHomes env (userId user)

    logInfo "Collecting metrics..."
    forM_ homes collectHome

    logInfo "Done!"

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
collectHome :: (MonadIO m
               , MonadReader env m
               , HasIOLogger env
               , CanNotifyUsers env
               , CanPushData env
               , HasHueHomeCollector env) =>
                Home -> m ()
collectHome home = do
  env <- ask

  -- Get the data
  maybeSourceData <- liftIO $ getHomeCollector env home
  case maybeSourceData of
    (Right sourceData) -> do
      logInfo "Collected light data"

      -- Notify the user that we've collected it
      homePayload <- liftIO $ buildHomePayload home
      liftIO $ getUserNotifier env (ownerId home) homePayload

      -- Stick the data on the data queue
      liftIO $ getDataPusher env sourceData

      logInfo "Published light data"
    (Left err) -> logError err
