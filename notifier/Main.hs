{-# LANGUAGE OverloadedStrings #-}

module Main where

import Database.HDBC.Sqlite3 (connectSqlite3)
import qualified Notifier
import Control.Concurrent.STM (atomically)
import Control.Concurrent.STM.TSem (newTSem)
import System.Log.Logger (updateGlobalLogger, rootLoggerName, removeHandler, setLevel, Priority (DEBUG), addHandler, emergencyM)
import System.Log.Handler.Simple (verboseStreamHandler)
import System.IO (stdout)
import Secrets (readDbPath)

main :: IO ()
main = do
  updateGlobalLogger rootLoggerName removeHandler
  updateGlobalLogger rootLoggerName $ setLevel DEBUG
  stdOutHandler <- verboseStreamHandler stdout DEBUG
  updateGlobalLogger rootLoggerName $ addHandler stdOutHandler

  dbPath <- readDbPath
  case dbPath of
    (Just path) -> do
        conn <- connectSqlite3 path
        readyVar <- atomically $ newTSem 0
        Notifier.main' readyVar conn
    Nothing -> emergencyM Notifier.loggerName "Database path not found, not starting"