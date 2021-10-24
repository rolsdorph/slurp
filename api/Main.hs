module Main where

import Api (run)
import Control.Concurrent.STM (atomically)
import Control.Concurrent.STM.TSem (newTSem)
import Database.HDBC.Sqlite3 (connectSqlite3)
import qualified InfluxDB

main :: IO ()
main = do
  conn <- connectSqlite3 InfluxDB.dbName
  readyVar <- atomically $ newTSem 0
  run readyVar conn
