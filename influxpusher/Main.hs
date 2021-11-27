module Main where
  
import InfluxPusher (loggerName, run)

import Database.HDBC.PostgreSQL (connectPostgreSQL)
import Control.Concurrent.STM (atomically)
import Control.Concurrent.STM.TSem (newTSem)
import System.Log.Logger (updateGlobalLogger, rootLoggerName, removeHandler, setLevel, Priority (DEBUG), addHandler, emergencyM)
import System.Log.Handler.Simple (verboseStreamHandler)
import System.IO (stdout)
import Secrets (readPgConnInfo)

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
