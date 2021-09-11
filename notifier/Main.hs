{-# LANGUAGE OverloadedStrings #-}

module Main where

import qualified Auth
import Control.Monad.Reader (ReaderT, ask, liftIO, runReaderT)
import qualified Notifier
import RabbitMQ (createConsumerRegistry)
import Secrets
import System.Log.Logger (emergencyM)
import Types (QueueConfig)

main :: IO ()
main = do
  maybeQueueConfig <- readUserNotificationQueueConfig
  case maybeQueueConfig of
    (Just queueConfig) -> runReaderT app queueConfig
    _ -> emergencyM Notifier.loggerName "Notification queue config not found, refusing to start"

app :: ReaderT QueueConfig IO ()
app = ask >>= liftIO . createConsumerRegistry >>= liftIO . Notifier.run Auth.verifyToken
