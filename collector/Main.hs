{-# LANGUAGE OverloadedStrings #-}

module Main where

import           Database.HDBC.Sqlite3 (connectSqlite3)

import qualified InfluxDB

import Collector


main :: IO ()
main = do
  conn <- connectSqlite3 InfluxDB.dbName
  run conn
