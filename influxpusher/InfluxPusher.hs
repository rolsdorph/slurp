{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}

module InfluxPusher (run, app, Env(..)) where

import Control.Concurrent (MVar, newEmptyMVar, putMVar, takeMVar)
import Control.Concurrent.Async (async)
import Control.Monad (forM_, forever)
import Control.Monad.Reader (MonadIO, MonadReader, ask, asks, liftIO, runReaderT)
import Data.Aeson (Value, eitherDecode, object, (.=))
import Data.Time (getCurrentTime)
import InfluxPublish (InfluxPushResult)
import qualified InfluxPublish as Influx
import qualified Network.AMQP as Q
import RabbitMQ (ConsumerRegistry, createConsumerRegistry)
import Types
import UserNotification (notify, publishNotifications, rmqPushFunction)
import qualified Database.InfluxDB.Types       as I
import qualified Data.Text                     as T
import           InfluxDB
import           System.Log.Logger
import           System.Log.Handler.Simple
import           GHC.IO.Handle.FD
import Secrets (readUserNotificationQueueConfig)
import           Database.HDBC.Sqlite3 (Connection)
import Control.Concurrent.STM.TSem (TSem, signalTSem)
import Control.Concurrent.STM (atomically)

loggerName :: String
loggerName = "InfluxPusher"

influxDbName :: I.Database
influxDbName = "home" -- TODO: Move to the database

data Env = Env
  { envGetUserSinks :: UserId -> IO [InfluxSink],
    envInfluxPush :: SourceData -> InfluxDefinition -> IO InfluxPushResult,
    envLogInfo :: String -> IO (),
    envLogWarn :: String -> IO (),
    envLogError :: String -> IO (),
    envPublishNotification :: MessageToUser -> IO (),
    envConsumerRegistry :: ConsumerRegistry,
    envSignalReady :: IO ()
  }

class HasSinks a where
  getUserSinks :: a -> UserId -> IO [InfluxSink]

class HasInfluxPush a where
  getInfluxPush :: a -> SourceData -> InfluxDefinition -> IO InfluxPushResult

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

run :: TSem -> Connection -> IO ()
run ready conn = do
    updateGlobalLogger rootLoggerName removeHandler
    updateGlobalLogger rootLoggerName $ setLevel DEBUG
    stdOutHandler <- verboseStreamHandler stdout DEBUG
    updateGlobalLogger rootLoggerName $ addHandler stdOutHandler

    maybeQueueConfig <- readUserNotificationQueueConfig
    case maybeQueueConfig of
        (Just queueConfig) -> do
            queueConnection     <- Q.openConnection (hostname queueConfig)
                                                    (vhost queueConfig)
                                                    (username queueConfig)
                                                    (password queueConfig)
            queueChannel <- Q.openChannel queueConnection

            _ <- Q.declareQueue queueChannel
                $ Q.newQueue { Q.queueName = notiQueueName queueConfig }
            _ <- Q.declareQueue queueChannel
                $ Q.newQueue { Q.queueName = dataQueueName queueConfig }

            consumerRegistry <- createConsumerRegistry queueConfig dataQueueName

            let env = Env {
              envLogInfo = infoM loggerName,
              envLogWarn = warningM loggerName,
              envLogError = errorM loggerName,
              envGetUserSinks = (`runReaderT` conn) <$> InfluxDB.getUserInfluxSinks,
              envInfluxPush = doPush,
              envPublishNotification = rmqPushFunction queueChannel (notiQueueName queueConfig),
              envConsumerRegistry = consumerRegistry,
              envSignalReady = atomically $ signalTSem ready
            }

            runReaderT app env
        _ -> emergencyM
            loggerName
            "Notification queue config not found, refusing to start"

doPush :: SourceData -> InfluxDefinition -> IO InfluxPushResult
doPush dataToPublish sink =
    Influx.publish
      (T.pack $ influxHost sink)
      (influxPort sink)
      (influxTLS sink)
      (T.pack $ influxUsername sink)
      (T.pack $ influxPassword sink)
      influxDbName
      (I.Measurement $ T.pack (datakey dataToPublish))
      (datapoints dataToPublish)
      
app :: (MonadIO m, MonadReader Env m) => m ()
app = do
  env <- ask

  -- Inter-thread communication
  userNotificationVar <- liftIO newEmptyMVar
  sourceDataMVar <- liftIO newEmptyMVar

  -- Listen for user notifications, forward them to the queue
  _ <-
    liftIO . async $
      publishNotifications
        (takeMVar userNotificationVar)
        (envPublishNotification env)

  -- Listen for data, put it on the MVar
  receiveEvents (envConsumerRegistry env) sourceDataMVar

  logInfo "Waiting for data collection events"

  liftIO $ envSignalReady env

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
  res <- liftIO $ influxPush dataToPublish (influxDefinition sink)

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
        (Right s@SourceData {}) -> liftIO $ putMVar dataVar s
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
