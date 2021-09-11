{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}

module InfluxPusher where

import Control.Concurrent (MVar, newEmptyMVar, putMVar, takeMVar)
import Control.Concurrent.Async (async)
import Control.Monad (forM_, forever)
import Control.Monad.Reader (MonadIO, MonadReader, ask, asks, liftIO)
import Data.Aeson (Value, eitherDecode, object, (.=))
import Data.Time (getCurrentTime)
import InfluxPublish (InfluxPushResult)
import qualified InfluxPublish as Influx
import qualified Network.AMQP as Q
import RabbitMQ (ConsumerRegistry)
import Types
import UserNotification (notify, publishNotifications, rmqPushFunction)

data Env = Env
  { envGetUserSinks :: UserId -> IO [InfluxSink],
    envInfluxPush :: SourceData -> InfluxSink -> IO InfluxPushResult,
    envLogInfo :: String -> IO (),
    envLogWarn :: String -> IO (),
    envLogError :: String -> IO (),
    envPublishNotification :: MessageToUser -> IO (),
    envConsumerRegistry :: ConsumerRegistry
  }

class HasSinks a where
  getUserSinks :: a -> UserId -> IO [InfluxSink]

class HasInfluxPush a where
  getInfluxPush :: a -> SourceData -> InfluxSink -> IO InfluxPushResult

class HasIOLogger a where
  getInfoLog :: a -> String -> IO ()
  getWarnLog :: a -> String -> IO ()
  getErrorLog :: a -> String -> IO ()

instance HasSinks Env where
  getUserSinks = envGetUserSinks

instance HasInfluxPush Env where
  getInfluxPush = envInfluxPush

instance HasIOLogger Env where
  getInfoLog = envLogInfo
  getWarnLog = envLogInfo
  getErrorLog = envLogError

logInfo :: (MonadIO m, MonadReader env m, HasIOLogger env) => String -> m ()
logInfo s = do
  logger <- asks getInfoLog
  liftIO $ logger s

logWarn :: (MonadIO m, MonadReader env m, HasIOLogger env) => String -> m ()
logWarn s = do
  logger <- asks getWarnLog
  liftIO $ logger s

logError :: (MonadIO m, MonadReader env m, HasIOLogger env) => String -> m ()
logError s = do
  logger <- asks getErrorLog
  liftIO $ logger s

app :: (MonadIO m, MonadReader Env m) => m ()
app = do
  env <- ask

  -- Inter-thread communication
  userNotificationVar <- liftIO newEmptyMVar
  sourceDataMVar <- liftIO newEmptyMVar

  -- Listen for user notifications, forward them to the queue
  userNotificationJob <-
    liftIO . async $
      publishNotifications
        (takeMVar userNotificationVar)
        (envPublishNotification env)

  -- Listen for data, put it on the MVar
  receiveEvents (envConsumerRegistry env) sourceDataMVar

  logInfo "Waiting for data collection events"

  -- Push data from the MVar onto all sinks for the data owner
  forever $ forwardToInflux sourceDataMVar userNotificationVar

-- Pushes the given data points to the given Influx sink
pushToSink ::
  (MonadIO m, MonadReader env m, HasInfluxPush env, HasIOLogger env) =>
  SourceData ->
  (Value -> IO ()) ->
  InfluxSink ->
  m ()
pushToSink dataToPublish userNotifier sink = do
  influxPush <- asks getInfluxPush
  res <- liftIO $ influxPush dataToPublish sink

  case res of
    Influx.Success -> do
      sinkPayload <- liftIO $ buildSinkPayload sink
      liftIO $ userNotifier sinkPayload
      logInfo "Pushed to a sink"
    Influx.Error err -> logError $ "Failed to push to sink: " ++ err

-- Waits for incoming AMQP events, puts valid ones on the given MVar
receiveEvents :: (MonadIO m, MonadReader env m, HasIOLogger env) => ConsumerRegistry -> MVar SourceData -> m ()
receiveEvents registerConsumer dataVar = do
  warnLog <- asks getWarnLog

  liftIO $
    registerConsumer $ \msg -> do
      let parsedMsg = eitherDecode (Q.msgBody msg)
      case parsedMsg of
        (Right s@(SourceData _ _ _ _)) -> liftIO $ putMVar dataVar s
        (Left err) -> warnLog $ "Ignoring malformed message " ++ err

      return ()

-- Waits for new source data, pushes it onto all relevant user sinks
forwardToInflux ::
  (MonadIO m, MonadReader env m, HasIOLogger env, HasSinks env, HasInfluxPush env) =>
  MVar SourceData ->
  MVar MessageToUser ->
  m ()
forwardToInflux dataVar userNotificationVar = do
  env <- ask

  sourceData <- liftIO $ takeMVar dataVar
  let userNotifier = notify userNotificationVar $ sourceOwnerId sourceData

  sinks <- liftIO $ getUserSinks env (sourceOwnerId sourceData)
  forM_ sinks $ pushToSink sourceData userNotifier

-- Builds a user notification event payload for a fetch of the given sink
buildSinkPayload :: InfluxSink -> IO Value
buildSinkPayload sink = do
  curTime <- getCurrentTime
  pure $
    object
      [ "type" .= ("SinkFed" :: String),
        "time" .= curTime,
        "sinkId" .= influxUuid sink
      ]
