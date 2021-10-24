{-# LANGUAGE OverloadedStrings #-}

module Main where

import Database.HDBC.Sqlite3 (connectSqlite3)
import qualified Notifier
import qualified TokenDB
import Control.Concurrent.STM (atomically)
import Control.Concurrent.STM.TSem (newTSem)

main :: IO ()
main = do
  conn <- connectSqlite3 TokenDB.dbName
  readyVar <- atomically $ newTSem 0
  Notifier.main' readyVar conn