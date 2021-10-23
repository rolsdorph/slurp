module Main where

import Api (run)
import Database.HDBC.Sqlite3 (connectSqlite3)
import qualified InfluxDB

main :: IO ()
main = do
  conn <- connectSqlite3 InfluxDB.dbName
  run conn
