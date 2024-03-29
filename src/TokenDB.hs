{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}

module TokenDB where

import           DBUtil

import           Data.Time.Clock
import           Database.HDBC
import           Data.UUID.V4
import           Data.ByteString.Lazy as L
import Control.Monad.Reader (liftIO, ask)

tokenTableName :: String
tokenTableName = "tokens"

createStmt :: String
createStmt =
    "CREATE TABLE IF NOT EXISTS "
        ++ tokenTableName
        ++ " (\
       \ user_id text NOT NULL, \
       \ token text NOT NULL, \
       \ created_at timestamp with time zone NULL)"

setupDb :: HasConnection ()
setupDb = do
    conn <- ask
    _ <- liftIO $ run conn createStmt []
    liftIO $ commit conn

-- Creates a token for the given user ID
createToken :: String -> HasConnection (Maybe String)
createToken uuid = do
    conn        <- ask

    token       <- show <$> liftIO nextRandom
    now         <- liftIO getCurrentTime

    numInserted <- liftIO $ run
        conn
        (  "INSERT INTO "
        ++ tokenTableName
        ++ "(user_id, token, created_at) VALUES (?, ?, ?)"
        )
        [toSql uuid, toSql token, toSql now]
    liftIO $ commit conn

    case numInserted of
        1 -> return $ Just token
        _ -> return Nothing

-- Fetches the user ID associated with a given token
getTokenUserId :: L.ByteString -> HasConnection (Maybe L.ByteString)
getTokenUserId token = do
    conn <- ask
    stmt <- liftIO $ prepare conn
                    ("SELECT * FROM " ++ tokenTableName ++ " WHERE token = ?")
    _        <- liftIO $ execute stmt [toSql token]
    firstHit <- liftIO $ fetchRowAL stmt

    case firstHit of
        (Just row) -> return $ valFrom "user_id" row
        _          -> return Nothing

-- Deletes all tokens for the given user ID
deleteUserTokens :: String -> HasConnection ()
deleteUserTokens uuid = do
    conn        <- ask
    _ <- liftIO $ run
        conn
        ("DELETE FROM " ++ tokenTableName ++ " WHERE user_id = ?")
        [toSql uuid]
    liftIO $ commit conn

-- Deletes the given token
deleteToken :: String -> String -> HasConnection ()
deleteToken uuid token = do
    conn        <- ask
    _ <- liftIO $ run
        conn
        ("DELETE FROM " ++ tokenTableName ++ " WHERE user_id = ? AND token = ?")
        [toSql uuid, toSql token]
    liftIO $ commit conn
