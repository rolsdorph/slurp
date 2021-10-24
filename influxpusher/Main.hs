module Main where
  
import InfluxPusher (run)
import qualified InfluxDB

import           Database.HDBC.Sqlite3 (connectSqlite3)
import Control.Concurrent.STM (atomically)
import Control.Concurrent.STM.TSem (newTSem)

main :: IO ()
main = do
    conn <- connectSqlite3 InfluxDB.dbName
    readyVar <- atomically $ newTSem 0
    run readyVar conn
