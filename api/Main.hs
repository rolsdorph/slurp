module Main where

import Api (loggerName, run)
import Control.Concurrent.STM (atomically)
import Control.Concurrent.STM.TSem (newTSem)
import Database.HDBC.Sqlite3 (connectSqlite3)
import Secrets (readDbPath)
import System.Log.Logger
import System.Log.Handler.Simple
import System.IO (stdout)

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
      run readyVar conn
    Nothing -> emergencyM loggerName "Database path not found, not starting"
