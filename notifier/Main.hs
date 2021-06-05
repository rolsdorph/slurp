{-# LANGUAGE OverloadedStrings #-}

module Main where

import qualified Auth
import GHC.IO.Handle.FD
import qualified Notifier
import Secrets
import System.Log.Handler.Simple
import System.Log.Logger

main :: IO ()
main = do
  updateGlobalLogger rootLoggerName removeHandler
  updateGlobalLogger rootLoggerName $ setLevel DEBUG
  stdOutHandler <- verboseStreamHandler stdout DEBUG
  updateGlobalLogger rootLoggerName $ addHandler stdOutHandler

  maybeQueueConfig <- readUserNotificationQueueConfig
  case maybeQueueConfig of
    (Just queueConfig) -> Notifier.run queueConfig Auth.verifyToken
    _ -> emergencyM Notifier.loggerName "Notification queue config not found, refusing to start"
