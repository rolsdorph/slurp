{-# LANGUAGE OverloadedStrings #-}

module Main where

import qualified Auth
import qualified Notifier
import Secrets
import System.Log.Logger (emergencyM)

main :: IO ()
main = do
  maybeQueueConfig <- readUserNotificationQueueConfig
  case maybeQueueConfig of
    (Just queueConfig) -> Notifier.run queueConfig Auth.verifyToken
    _ -> emergencyM Notifier.loggerName "Notification queue config not found, refusing to start"
