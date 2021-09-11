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
import           InfluxPusher
import           Secrets
import           Types
import qualified InfluxPublish as Influx
import           UserNotification
import InfluxPublish (InfluxPushResult)
import RabbitMQ (ConsumerRegistry, createConsumerRegistry)
import Control.Monad.Reader (MonadReader, MonadIO, runReaderT)

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
            queueConnection     <- Q.openConnection (hostname queueConfig)
                                                    (vhost queueConfig)
                                                    (username queueConfig)
                                                    (password queueConfig)
            queueChannel <- Q.openChannel queueConnection

            Q.declareQueue queueChannel
                $ Q.newQueue { Q.queueName = notiQueueName queueConfig }
            Q.declareQueue queueChannel
                $ Q.newQueue { Q.queueName = dataQueueName queueConfig }

            consumerRegistry <- createConsumerRegistry queueConfig

            let env = Env {
              envLogInfo = infoM loggerName,
              envLogWarn = warningM loggerName,
              envLogError = errorM loggerName,
              envGetUserSinks = InfluxDB.getUserInfluxSinks,
              envInfluxPush = doPush,
              envPublishNotification = rmqPushFunction queueChannel (notiQueueName queueConfig),
              envConsumerRegistry = consumerRegistry
            }

            runReaderT app env
        _ -> emergencyM
            loggerName
            "Notification queue config not found, refusing to start"

doPush :: SourceData -> InfluxSink -> IO InfluxPushResult
doPush dataToPublish sink =
    Influx.publish
      (T.pack $ influxHost sink)
      (influxPort sink)
      (T.pack $ influxUsername sink)
      (T.pack $ influxPassword sink)
      influxDbName
      (I.Measurement $ T.pack (datakey dataToPublish))
      (datapoints dataToPublish)
