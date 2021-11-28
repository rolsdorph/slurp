module Main where

import Api (loggerName, run)
import Control.Concurrent.STM (atomically)
import Control.Concurrent.STM.TSem (newTSem)
import System.Log.Logger
import System.Log.Handler.Simple
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
      run readyVar conn
    Nothing -> emergencyM loggerName "PG connection info not found, not starting"
