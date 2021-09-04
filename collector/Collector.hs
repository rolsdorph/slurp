{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings #-}

module Collector where

import Control.Concurrent (threadDelay)
import Control.Concurrent.Async
import Control.Monad
import Control.Monad.Except (ExceptT, MonadIO, runExceptT)
import Control.Monad.Reader (MonadReader, ReaderT, ask, asks, liftIO, runReaderT)
import Data.Aeson
import qualified Data.ByteString.Lazy as LB
import Data.Time.Clock
import Types
import UserNotification

data Env = Env
  { envGetAllUsers :: IO [User],
    envGetUserSs :: String -> ExceptT LB.ByteString IO [SimpleShallowJsonSource],
    envGetUserHomes :: String -> IO [Home],
    envCollectHome :: Home -> IO (Either String SourceData),
    envCollectSs :: SimpleShallowJsonSource -> IO (Either String SourceData),
    envLogInfo :: String -> IO (),
    envLogError :: String -> IO (),
    envPushData :: SourceData -> IO (),
    envNotify :: UserId -> Value -> IO (),
    envReadEvent :: IO SourceData,
    envReadNotification :: IO MessageToUser,
    envPublishNotification :: MessageToUser -> IO (),
    envPublishData :: SourceData -> IO ()
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
  getUserNotifier :: a -> (UserId -> Value -> IO ())

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

app :: (MonadIO m, MonadReader Env m) => m ()
app = do
  env <- ask
  publishJob <- liftIO . async $ runReaderT publishAll env

  userNotificationJob <-
    liftIO . async $
      publishNotifications
        (envReadNotification env)
        (envPublishNotification env)

  dataEventsJob <-
    liftIO . async $
      publishNotifications
        (envReadEvent env)
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
publishSSForUser ::
  ( MonadIO m,
    MonadReader env m,
    HasIOLogger env,
    CanNotifyUsers env,
    CanPushData env,
    HasSimpleSources env,
    HasSimpleSourceCollector env
  ) =>
  User ->
  m ()
publishSSForUser user = do
  env <- ask
  maybeSources <- liftIO . runExceptT $ getUserSimpleSources env (userId user)

  case maybeSources of
    (Left err) -> logError (show err)
    (Right sources) -> do
      logInfo "Collecting simple sources..."
      forM_ sources collectSimpleSource

      logInfo "Collected simple sources!"

collectSimpleSource ::
  ( MonadIO m,
    MonadReader env m,
    HasIOLogger env,
    CanNotifyUsers env,
    CanPushData env,
    HasSimpleSourceCollector env
  ) =>
  SimpleShallowJsonSource ->
  m ()
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
publishForUser ::
  ( MonadIO m,
    MonadReader env m,
    HasIOLogger env,
    HasHomes env,
    CanNotifyUsers env,
    CanPushData env,
    HasHueHomeCollector env
  ) =>
  User ->
  m ()
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
  pure $
    object
      [ "type" .= ("SourceCollected" :: String),
        "time" .= curTime,
        "sourceId" .= uuid home
      ]

buildSimpleSourcePayload :: SimpleShallowJsonSource -> IO Value
buildSimpleSourcePayload source = do
  curTime <- getCurrentTime
  pure $
    object
      [ "type" .= ("SourceCollected" :: String),
        "time" .= curTime,
        "sourceId" .= genericSourceId source
      ]

type DataQueuePusher = SourceData -> IO ()

-- Publishes data from the given home to the data queue
collectHome ::
  ( MonadIO m,
    MonadReader env m,
    HasIOLogger env,
    CanNotifyUsers env,
    CanPushData env,
    HasHueHomeCollector env
  ) =>
  Home ->
  m ()
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
