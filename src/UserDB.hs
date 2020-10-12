{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}

module UserDB where

import           Types
import           DBUtil

import           Data.Maybe
import           Data.Convertible.Base
import           Data.List
import           Data.Time.Clock
import           Database.HDBC
import           Database.HDBC.Sqlite3
import           Data.UUID.V4
import qualified Data.ByteString.Lazy          as L
import qualified Data.ByteString.Char8         as C

dbName = "/home/mads/dev/minimal-hue-metrics-website/testdb.sqlite3"
userTableName = "users" :: String

createStmt =
    "CREATE TABLE IF NOT EXISTS "
        ++ userTableName
        ++ " (\
       \ uuid text NOT NULL, \
       \ createdAt datetime NULL,\
       \ authType text DEFAULT 'Google',\
       \ googleUuid text NULL)"

setupDb = do
    conn <- connectSqlite3 dbName
    run conn createStmt []
    commit conn
    disconnect conn

getUser :: L.ByteString -> IO (Maybe User)
getUser userId = do
    conn <- connectSqlite3 dbName
    stmt <- prepare conn
                    ("SELECT * FROM " ++ userTableName ++ " WHERE uuid = ?")
    numRows  <- execute stmt [toSql userId]
    firstHit <- fetchRowAL stmt
    disconnect conn

    case firstHit of
        (Just row) -> return $ parseUserRow row
        _          -> return Nothing

-- Fetches all users
getAllUsers :: IO [User]
getAllUsers = do
    conn  <- connectSqlite3 dbName
    stmt  <- prepare conn ("SELECT * FROM " ++ userTableName)
    res   <- execute stmt []
    users <- fetchAllRowsAL stmt
    pure $ mapMaybe parseUserRow users

-- Fetches the user with the given Google UUID, creating a new user if it doesn't exist
fetchOrCreateGoogleUser :: L.ByteString -> IO (Maybe User)
fetchOrCreateGoogleUser googleId = do
    conn <- connectSqlite3 dbName
    stmt <- prepare
        conn
        ("SELECT * FROM " ++ userTableName ++ " WHERE googleUuid = ?")
    numRows  <- execute stmt [toSql googleId]
    firstHit <- fetchRowAL stmt
    disconnect conn

    case firstHit of
        (Just row) -> do
           print $ "Found user with Google UUID " <> googleId
           return $ parseUserRow row
        _          -> createGoogleUser googleId

-- Creates a new user with the given Google UUID
createGoogleUser :: L.ByteString -> IO (Maybe User)
createGoogleUser googleId = do
    print $ "Creating user with Google UUID " <> googleId

    uuid <- show <$> nextRandom
    now  <- getCurrentTime

    let newUser = User uuid now Google (Just $ (C.unpack . L.toStrict) googleId)

    conn        <- connectSqlite3 dbName
    numInserted <- run
        conn
        ("INSERT INTO "
        ++ userTableName
        ++ "(uuid, createdAt, authType, googleUuid) VALUES (?, ?, ?, ?)")
        [toSql uuid, toSql now, toSql Google, toSql googleId]
    commit conn
    disconnect conn

    case numInserted of
        1 -> pure $ Just newUser
        _ -> pure Nothing

parseUserRow :: [(String, SqlValue)] -> Maybe User
parseUserRow vals =
    User
        <$> valFrom "uuid"      vals
        <*> valFrom "createdAt" vals
        <*> (authFromString <$> valFrom "authType" vals)
        <*> pure (valFrom "googleUuid" vals)
