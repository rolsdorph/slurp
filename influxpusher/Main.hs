module Main where
  
import InfluxPusher (run)
import qualified InfluxDB

import           Database.HDBC.Sqlite3 (connectSqlite3)

main :: IO ()
main = do
    conn <- connectSqlite3 InfluxDB.dbName
    run conn
