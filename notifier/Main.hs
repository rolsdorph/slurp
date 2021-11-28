{-# LANGUAGE OverloadedStrings #-}

module Main where

import qualified Notifier
import Control.Concurrent.STM (atomically)
import Control.Concurrent.STM.TSem (newTSem)
import System.Log.Logger (updateGlobalLogger, rootLoggerName, removeHandler, setLevel, Priority (DEBUG), addHandler, emergencyM)
import System.Log.Handler.Simple (verboseStreamHandler)
import System.IO (stdout)
import DBUtil (connectPsql)

main :: IO ()
main = do
  updateGlobalLogger rootLoggerName removeHandler
  updateGlobalLogger rootLoggerName $ setLevel DEBUG
  stdOutHandler <- verboseStreamHandler stdout DEBUG
  updateGlobalLogger rootLoggerName $ addHandler stdOutHandler

  pgConn <- connectPsql
  case pgConn of
    (Just conn) -> do
      readyVar <- atomically $ newTSem 0
      Notifier.main' readyVar conn
    Nothing -> emergencyM Notifier.loggerName "PG connection info not found, not starting"