module Main where

import Control.Monad.Reader (runReaderT)
import System.Log.Logger
import System.Log.Handler.Simple
import System.IO (stdout)

import DBUtil (connectPsql)

import UserDB
import HomeDB
import TokenDB
import InfluxDB
import SimpleSourceDB

loggerName :: String
loggerName = "DB tool"

main :: IO ()
main = do
  updateGlobalLogger rootLoggerName removeHandler
  updateGlobalLogger rootLoggerName $ setLevel DEBUG
  stdOutHandler <- verboseStreamHandler stdout DEBUG
  updateGlobalLogger rootLoggerName $ addHandler stdOutHandler

  pgConn <- connectPsql
  case pgConn of
    (Just conn) -> do
      -- Create necessary tables if they don't exist
      runReaderT UserDB.setupDb conn
      runReaderT HomeDB.setupDb conn
      runReaderT TokenDB.setupDb conn
      runReaderT InfluxDB.setupDb conn
      runReaderT SimpleSourceDB.setupDb conn
    Nothing -> emergencyM loggerName "PG connection info not found, not running migrations"
