{-# LANGUAGE OverloadedStrings #-}

module Main where

import qualified Auth
import Control.Monad.Reader (ReaderT, ask, liftIO, runReaderT)
import Database.HDBC.Sqlite3 (connectSqlite3)
import qualified Notifier
import RabbitMQ (createConsumerRegistry)
import Secrets
import System.Log.Logger (emergencyM)
import qualified TokenDB
import Types (QueueConfig)

main :: IO ()
main = do
  maybeQueueConfig <- readUserNotificationQueueConfig
  case maybeQueueConfig of
    (Just queueConfig) -> runReaderT app queueConfig
    _ -> emergencyM Notifier.loggerName "Notification queue config not found, refusing to start"

app :: ReaderT QueueConfig IO ()
app = do
  queueConfig <- ask
  consumerRegistry <- liftIO $ createConsumerRegistry queueConfig
  conn <- liftIO $ connectSqlite3 TokenDB.dbName
  liftIO $ Notifier.run ((`runReaderT` conn) <$> Auth.verifyToken) consumerRegistry