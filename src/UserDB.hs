{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}

module UserDB where

import           Types
import           DBUtil
import           Util

import           Data.Convertible.Base
import           Data.List
import           Data.Time.Clock
import           Database.HDBC
import           Database.HDBC.Sqlite3
import           Data.UUID.V4
import qualified Data.ByteString.Lazy          as L
import qualified Data.ByteString.Char8         as C
import           System.Log.Logger

userDbLoggerName = "UserDB" -- TODO: Just don't export this variable
dbName = "/home/mads/dev/minimal-hue-metrics-website/testdb.sqlite3"
userTableName = "users" :: String

createStmt =
    "CREATE TABLE IF NOT EXISTS "
        ++ userTableName
        ++ " (\
       \ uuid text NOT NULL, \
       \ createdAt datetime NULL,\
       \ authType text DEFAULT 'Insecure',\
       \ thirdPartyId text NULL)"

setupDb = do
    conn <- connectSqlite3 dbName
    run conn createStmt []
    commit conn
    disconnect conn

getUser :: L.ByteString -> IO (Either String User)
getUser userId = do
    conn <- connectSqlite3 dbName
    stmt <- prepare conn
                    ("SELECT * FROM " ++ userTableName ++ " WHERE uuid = ?")
    numRows  <- execute stmt [toSql userId]
    firstHit <- fetchRowAL stmt
    disconnect conn

    case firstHit of
        (Just row) -> return $ parseUserRow row
        _          -> return $ Left "User not found"

-- Fetches all users
getAllUsers :: IO [User]
getAllUsers = do
    conn  <- connectSqlite3 dbName
    stmt  <- prepare conn ("SELECT * FROM " ++ userTableName)
    res   <- execute stmt []
    users <- fetchAllRowsAL stmt
    pure $ mapEither parseUserRow users

-- Fetches the user with the given insecure id, creating a new user if it doesn't exist
fetchOrCreateInsecureUser :: L.ByteString -> IO (Either String User)
fetchOrCreateInsecureUser insecureId = do
    conn <- connectSqlite3 dbName
    stmt <- prepare
        conn
        ("SELECT * FROM " ++ userTableName ++ " WHERE thirdPartyId = ?")
    numRows  <- execute stmt [toSql insecureId]
    firstHit <- fetchRowAL stmt
    disconnect conn

    case firstHit of
        (Just row) -> do
           infoM userDbLoggerName $ "Found user with insecure UUID " ++ show insecureId
           return $ parseUserRow row
        _          -> createInsecureUser insecureId

-- Fetches the user with the given Google UUID, creating a new user if it doesn't exist
fetchOrCreateGoogleUser :: L.ByteString -> IO (Either String User)
fetchOrCreateGoogleUser googleId = do
    conn <- connectSqlite3 dbName
    stmt <- prepare
        conn
        ("SELECT * FROM " ++ userTableName ++ " WHERE thirdPartyId = ?")
    numRows  <- execute stmt [toSql googleId]
    firstHit <- fetchRowAL stmt
    disconnect conn

    case firstHit of
        (Just row) -> do
           infoM userDbLoggerName $ "Found user with Google UUID " ++ show googleId
           return $ parseUserRow row
        _          -> createGoogleUser googleId

-- Creates a new user with the given Google UUID
createGoogleUser :: L.ByteString -> IO (Either String User)
createGoogleUser googleId = do
    infoM userDbLoggerName $ "Creating user with Google UUID " ++ show googleId

    uuid <- show <$> nextRandom
    now  <- getCurrentTime

    let newUser = User uuid now Google (Just $ (C.unpack . L.toStrict) googleId)

    conn        <- connectSqlite3 dbName
    numInserted <- run
        conn
        ("INSERT INTO "
        ++ userTableName
        ++ "(uuid, createdAt, authType, thirdPartyId) VALUES (?, ?, ?, ?)")
        [toSql uuid, toSql now, toSql Google, toSql googleId]
    commit conn
    disconnect conn

    case numInserted of
        1 -> pure $ Right newUser
        _ -> pure $ Left "Failed to create user"

-- Creates a new user with the given insecure UUID
createInsecureUser :: L.ByteString -> IO (Either String User)
createInsecureUser insecureId = do
    infoM userDbLoggerName $ "Creating user with insecure UUID " ++ show insecureId

    uuid <- show <$> nextRandom
    now  <- getCurrentTime

    let newUser = User uuid now Insecure (Just $ (C.unpack . L.toStrict) insecureId)

    conn        <- connectSqlite3 dbName
    numInserted <- run
        conn
        ("INSERT INTO "
        ++ userTableName
        ++ "(uuid, createdAt, authType, thirdPartyId) VALUES (?, ?, ?, ?)")
        [toSql uuid, toSql now, toSql Insecure, toSql insecureId]
    commit conn
    disconnect conn

    case numInserted of
        1 -> pure $ Right newUser
        _ -> pure $ Left "Failed to create user"

parseUserRow :: [(String, SqlValue)] -> Either String User
parseUserRow vals =
    User <$> eitherValFrom "uuid"      vals <*> eitherValFrom "createdAt" vals
        <*> (authFromString <$> eitherValFrom "authType" vals)
        <*> eitherValFrom "thirdPartyId" vals
