{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings #-}

module Collector where

import Control.Concurrent (newEmptyMVar, takeMVar, threadDelay, MVar, putMVar)
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
    envPublishNotification :: MessageToUser -> IO (),
    envPublishData :: SourceData -> IO ()
  }

type UserNotifier = UserId -> Value -> IO ()

type DataPusher = SourceData -> IO ()

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

app :: (MonadIO m, MonadReader Env m) => m ()
app = do
  env <- ask

  userNotificationVar <- liftIO newEmptyMVar
  dataEventsVar <- liftIO newEmptyMVar

  publishJob <- liftIO . async $ runReaderT (publishAll userNotificationVar dataEventsVar) env

  userNotificationJob <-
    liftIO . async $
      publishNotifications
        (takeMVar userNotificationVar)
        (envPublishNotification env)

  dataEventsJob <-
    liftIO . async $
      publishNotifications
        (takeMVar dataEventsVar)
        (envPublishData env)

  liftIO $ wait publishJob

-- Publishes data from all user sources to the data queue
publishAll :: (MonadIO m, MonadReader Env m) => MVar MessageToUser -> MVar SourceData -> m ()
publishAll notificationVar dataVar = forever $ do
  userGetter <- asks getAllUsers

  logInfo "Fetching users..."
  users <- liftIO userGetter

  let notifyUser = notify notificationVar
  let pushData = putMVar dataVar

  forM_ users (publishForUser notifyUser pushData)
  forM_ users (publishSSForUser notifyUser pushData)

  logInfo "All done, soon looping again!"

  liftIO $ threadDelay (1000 * 1000 * 10) -- 10s

-- Publishes data from all user simple sources to the data queue
publishSSForUser ::
  ( MonadIO m,
    MonadReader env m,
    HasIOLogger env,
    HasSimpleSources env,
    HasSimpleSourceCollector env
  ) =>
  UserNotifier ->
  DataPusher ->
  User ->
  m ()
publishSSForUser notifier dataPusher user = do
  env <- ask
  maybeSources <- liftIO . runExceptT $ getUserSimpleSources env (userId user)

  case maybeSources of
    (Left err) -> logError (show err)
    (Right sources) -> do
      logInfo "Collecting simple sources..."
      forM_ sources (collectSimpleSource notifier dataPusher)

      logInfo "Collected simple sources!"

collectSimpleSource ::
  ( MonadIO m,
    MonadReader env m,
    HasIOLogger env,
    HasSimpleSourceCollector env
  ) =>
  UserNotifier ->
  DataPusher ->
  SimpleShallowJsonSource ->
  m ()
collectSimpleSource userNotifier dataPusher source = do
  env <- ask
  sourceData <- liftIO $ getSsCollector env source

  case sourceData of
    (Left err) -> logError $ "Failed to collect data: " <> err
    (Right sd) -> do
      -- Notify the user that we've collected it
      sourcePayload <- liftIO $ buildSimpleSourcePayload source
      liftIO $ userNotifier (sourceOwnerId sd) sourcePayload

      -- Stick the data on the data queue
      liftIO $ dataPusher sd

      logInfo "Published simple data"

-- Publishes data from all user homes to the data queue
publishForUser ::
  ( MonadIO m,
    MonadReader env m,
    HasIOLogger env,
    HasHomes env,
    HasHueHomeCollector env
  ) =>
  UserNotifier ->
  DataPusher ->
  User ->
  m ()
publishForUser notifier dataPusher user = do
  env <- ask

  logInfo "Fetching verified user homes..."
  homes <- liftIO $ getUserHomes env (userId user)

  logInfo "Collecting metrics..."
  forM_ homes (collectHome notifier dataPusher)

  logInfo "Done!"

-- Publishes data from the given home to the data queue
collectHome ::
  ( MonadIO m,
    MonadReader env m,
    HasIOLogger env,
    HasHueHomeCollector env
  ) =>
  UserNotifier ->
  DataPusher ->
  Home ->
  m ()
collectHome notifier dataPusher home = do
  env <- ask

  -- Get the data
  maybeSourceData <- liftIO $ getHomeCollector env home
  case maybeSourceData of
    (Right sourceData) -> do
      logInfo "Collected light data"

      -- Notify the user that we've collected it
      homePayload <- liftIO $ buildHomePayload home
      liftIO $ notifier (ownerId home) homePayload

      -- Stick the data on the data queue
      liftIO $ dataPusher sourceData

      logInfo "Published light data"
    (Left err) -> logError err

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
