{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}

module TokenDB where

import           Types
import           DBUtil

import           Data.Maybe
import           Data.Time.Clock
import           Database.HDBC
import           Database.HDBC.Sqlite3
import           Data.UUID.V4
import           Data.ByteString.Lazy as L

dbName = "/home/mads/dev/minimal-hue-metrics-website/testdb.sqlite3"
tokenTableName = "tokens" :: String

createStmt =
    "CREATE TABLE IF NOT EXISTS "
        ++ tokenTableName
        ++ " (\
       \ userId text NOT NULL, \
       \ token text NOT NULL, \
       \ createdAt datetime NULL)"

setupDb = do
    conn <- connectSqlite3 dbName
    run conn createStmt []
    commit conn
    disconnect conn

-- Creates a token for the given user ID
createToken :: String -> IO (Maybe String)
createToken uuid = do
    token       <- show <$> nextRandom
    now         <- getCurrentTime

    conn        <- connectSqlite3 dbName
    numInserted <- run
        conn
        (  "INSERT INTO "
        ++ tokenTableName
        ++ "(userId, token, createdAt) VALUES (?, ?, ?)"
        )
        [toSql uuid, toSql token, toSql now]
    commit conn
    disconnect conn

    case numInserted of
        1 -> pure $ Just token
        _ -> pure Nothing

-- Fetches the user ID associated with a given token
getTokenUserId :: L.ByteString -> IO (Maybe L.ByteString)
getTokenUserId token = do
    conn <- connectSqlite3 dbName
    stmt <- prepare conn
                    ("SELECT * FROM " ++ tokenTableName ++ " WHERE token = ?")
    numRows  <- execute stmt [toSql token]
    firstHit <- fetchRowAL stmt
    disconnect conn

    case firstHit of
        (Just row) -> return $ valFrom "userId" row
        _          -> return Nothing

-- Deletes all tokens for the given user ID
deleteUserTokens :: String -> IO ()
deleteUserTokens uuid = do
    conn        <- connectSqlite3 dbName
    numInserted <- run
        conn
        ("DELETE FROM " ++ tokenTableName ++ " WHERE userId = ?")
        [toSql uuid]
    commit conn
    disconnect conn

-- Deletes the given token
deleteToken :: String -> String -> IO ()
deleteToken uuid token = do
    conn        <- connectSqlite3 dbName
    numInserted <- run
        conn
        ("DELETE FROM " ++ tokenTableName ++ " WHERE userId = ? AND token = ?")
        [toSql uuid, toSql token]
    commit conn
    disconnect conn
