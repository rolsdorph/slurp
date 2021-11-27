module Main where

import Api (loggerName, run)
import Control.Concurrent.STM (atomically)
import Control.Concurrent.STM.TSem (newTSem)
import Database.HDBC.PostgreSQL (connectPostgreSQL)
import Secrets (readPgConnInfo)
import System.Log.Logger
import System.Log.Handler.Simple
import System.IO (stdout)

main :: IO ()
main = do
  updateGlobalLogger rootLoggerName removeHandler
  updateGlobalLogger rootLoggerName $ setLevel DEBUG
  stdOutHandler <- verboseStreamHandler stdout DEBUG
  updateGlobalLogger rootLoggerName $ addHandler stdOutHandler

  pgConnInfo <- readPgConnInfo
  case pgConnInfo of
    (Just connInfo) -> do
      conn <- connectPostgreSQL connInfo
      readyVar <- atomically $ newTSem 0
      run readyVar conn
    Nothing -> emergencyM loggerName "PG connection info not found, not starting"
