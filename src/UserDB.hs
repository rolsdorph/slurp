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
import Control.Monad.Reader (ask, liftIO)

userDbLoggerName = "UserDB" -- TODO: Just don't export this variable
userTableName = "users" :: String

createStmt =
    "CREATE TABLE IF NOT EXISTS "
        ++ userTableName
        ++ " (\
       \ uuid text NOT NULL, \
       \ createdAt datetime NULL,\
       \ authType text DEFAULT 'Insecure',\
       \ thirdPartyId text NULL)"

setupDb :: HasConnection ()
setupDb = do
    conn <- ask
    liftIO $ run conn createStmt []
    liftIO $ commit conn

getUser :: L.ByteString -> HasConnection (Either String User)
getUser userId = do
    conn <- ask
    stmt <- liftIO $ prepare conn
                    ("SELECT * FROM " ++ userTableName ++ " WHERE uuid = ?")
    numRows  <- liftIO $ execute stmt [toSql userId]
    firstHit <- liftIO $ fetchRowAL stmt

    case firstHit of
        (Just row) -> return $ parseUserRow row
        _          -> return $ Left "User not found"

-- Fetches all users
getAllUsers :: HasConnection [User]
getAllUsers = do
    conn  <- ask
    stmt  <- liftIO $ prepare conn ("SELECT * FROM " ++ userTableName)
    res   <- liftIO $ execute stmt []
    users <- liftIO $ fetchAllRowsAL stmt
    return $ mapEither parseUserRow users

-- Fetches the user with the given insecure id, creating a new user if it doesn't exist
fetchOrCreateInsecureUser :: L.ByteString -> HasConnection (Either String User)
fetchOrCreateInsecureUser insecureId = do
    conn <- ask
    stmt <- liftIO $ prepare
        conn
        ("SELECT * FROM " ++ userTableName ++ " WHERE thirdPartyId = ?")
    numRows  <- liftIO $ execute stmt [toSql insecureId]
    firstHit <- liftIO $ fetchRowAL stmt

    case firstHit of
        (Just row) -> do
           liftIO . infoM userDbLoggerName $ "Found user with insecure UUID " ++ show insecureId
           return $ parseUserRow row
        _          -> createInsecureUser insecureId

-- Fetches the user with the given Google UUID, creating a new user if it doesn't exist
fetchOrCreateGoogleUser :: L.ByteString -> HasConnection (Either String User)
fetchOrCreateGoogleUser googleId = do
    conn <- ask
    stmt <- liftIO $ prepare
        conn
        ("SELECT * FROM " ++ userTableName ++ " WHERE thirdPartyId = ?")
    numRows  <- liftIO $ execute stmt [toSql googleId]
    firstHit <- liftIO $ fetchRowAL stmt

    case firstHit of
        (Just row) -> do
           liftIO . infoM userDbLoggerName $ "Found user with Google UUID " ++ show googleId
           return $ parseUserRow row
        _          -> createGoogleUser googleId

-- Creates a new user with the given Google UUID
createGoogleUser :: L.ByteString -> HasConnection (Either String User)
createGoogleUser googleId = do
    conn <- ask
    liftIO . infoM userDbLoggerName $ "Creating user with Google UUID " ++ show googleId

    uuid <- show <$> liftIO nextRandom
    now  <- liftIO getCurrentTime

    let newUser = User uuid now Google (Just $ (C.unpack . L.toStrict) googleId)

    numInserted <- liftIO $ run
        conn
        ("INSERT INTO "
        ++ userTableName
        ++ "(uuid, createdAt, authType, thirdPartyId) VALUES (?, ?, ?, ?)")
        [toSql uuid, toSql now, toSql Google, toSql googleId]
    liftIO $ commit conn

    case numInserted of
        1 -> return $ Right newUser
        _ -> return $ Left "Failed to create user"

-- Creates a new user with the given insecure UUID
createInsecureUser :: L.ByteString -> HasConnection (Either String User)
createInsecureUser insecureId = do
    conn <- ask
    liftIO . infoM userDbLoggerName $ "Creating user with insecure UUID " ++ show insecureId

    uuid <- show <$> liftIO nextRandom
    now  <- liftIO getCurrentTime

    let newUser = User uuid now Insecure (Just $ (C.unpack . L.toStrict) insecureId)

    numInserted <- liftIO $ run
        conn
        ("INSERT INTO "
        ++ userTableName
        ++ "(uuid, createdAt, authType, thirdPartyId) VALUES (?, ?, ?, ?)")
        [toSql uuid, toSql now, toSql Insecure, toSql insecureId]
    liftIO $ commit conn

    case numInserted of
        1 -> return $ Right newUser
        _ -> return $ Left "Failed to create user"

parseUserRow :: [(String, SqlValue)] -> Either String User
parseUserRow vals =
    User <$> eitherValFrom "uuid"      vals <*> eitherValFrom "createdAt" vals
        <*> (authFromString <$> eitherValFrom "authType" vals)
        <*> eitherValFrom "thirdPartyId" vals
