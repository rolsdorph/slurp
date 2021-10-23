{-# LANGUAGE OverloadedStrings #-}

module Main where

import Database.HDBC.Sqlite3 (connectSqlite3)
import qualified Notifier
import qualified TokenDB

main :: IO ()
main = do
  conn <- connectSqlite3 TokenDB.dbName
  Notifier.main' conn