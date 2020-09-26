{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}

module HomeDB
    where

import Types

import           Data.Convertible.Base
import           Data.List
import           Data.Time.Clock
import           Database.HDBC
import           Database.HDBC.Sqlite3
import           Data.UUID.V4
import qualified Data.ByteString.Lazy          as L

dbName = "/home/mads/dev/minimal-hue-metrics-website/testdb.sqlite3"
homeTableName = "homes" :: String

createStmt =
    "CREATE TABLE IF NOT EXISTS "
        ++ homeTableName
        ++ " (\
       \ uuid text NOT NULL, \
       \ state text DEFAULT 'Pending',\
       \ oauthState text NULL,\
       \ accessToken text NULL,\
       \ accessExpiry datetime NULL,\
       \ refreshToken text NULL,\
       \ refreshExpiry datetime NULL,\
       \ createdAt datetime NULL,\
       \ influxHost text NOT NULL,\
       \ influxPort text NOT NULL,\
       \ influxTLS boolean NOT NULL)"

setupDb = do
    conn <- connectSqlite3 dbName
    run conn createStmt []
    commit conn
    disconnect conn

-- Stores the updated home
updateHome :: Home -> IO (Either L.ByteString Home)
updateHome newHome = do
    conn        <- connectSqlite3 dbName
    numUpdated <- run
        conn
        ("UPDATE "
        ++ homeTableName
        ++ " SET state=?, accessToken=?, accessExpiry=?, refreshToken=?, refreshExpiry=?"
        ++ " WHERE uuid=?"
        ) [toSql $ state newHome
        , toSql $ accessToken newHome
        , toSql $ accessExpiry newHome
        , toSql $ accessToken newHome
        , toSql $ accessExpiry newHome
        , toSql $ uuid newHome
        ]
    commit conn
    disconnect conn

    case numUpdated of
        1 -> return (Right newHome)
        _ -> return (Left "Failed to store home.")

-- Stores a Home in the database
storeHome :: Home -> IO (Either L.ByteString Home)
storeHome home = do
    -- Randomness for UUID and state
    uuid        <- show <$> nextRandom
    oauthState  <- show <$> nextRandom

    -- Actual insert
    conn        <- connectSqlite3 dbName
    numInserted <- run
        conn
        ("INSERT INTO "
        ++ homeTableName
        ++ "(uuid, createdAt, oauthState, influxHost, influxPort, influxTLS) \
                                    \ VALUES (?, ?, ?, ?, ?, ?)"
        )
        [ toSql $ uuid
        , toSql $ createdAt home
        , toSql $ oauthState
        , toSql $ influxHost home
        , toSql $ influxPort home
        , toSql $ influxTLS home
        ]
    commit conn
    disconnect conn

    case numInserted of
        1 -> return (Right (home { uuid = Just uuid, oauthState = Just oauthState }))
        _ -> return (Left "Failed to store home.")

-- Retrieves the Home associated with the given OAuth verification state
getOauthPendingHome :: String -> IO (Maybe Home)
getOauthPendingHome state = do
    print state
    conn <- connectSqlite3 dbName
    stmt <- prepare conn
                    ("SELECT * FROM " ++ homeTableName ++ " WHERE state = 'Pending' AND oauthState = ?")
    numRows  <- execute stmt [toSql state]
    firstHit <- fetchRowAL stmt

    case firstHit of
        (Just row) -> return $ parseHomeRow row
        _          -> return Nothing

-- Retrieves a Home from the database
getHome :: Maybe String -> IO (Maybe Home)
getHome (Just uuid) = do
    conn <- connectSqlite3 dbName
    stmt <- prepare conn
                    ("SELECT * FROM " ++ homeTableName ++ " WHERE uuid = ?")
    numRows  <- execute stmt [toSql uuid]
    firstHit <- fetchRowAL stmt

    case firstHit of
        (Just row) -> return $ parseHomeRow row
        _          -> return Nothing
getHome Nothing = return Nothing

parseHomeRow :: [(String, SqlValue)] -> Maybe Home
parseHomeRow vals = do
    Home
        <$> (valFrom "uuid" vals)
        <*> (valFrom "influxHost" vals)
        <*> (valFrom "influxPort" vals)
        <*> (valFrom "influxTLS" vals)
        <*> (valFrom "createdAt" vals)
        <*> (fromString <$> valFrom "state" vals)
        <*> pure (valFrom "oauthState" vals)
        <*> pure (valFrom "accessToken" vals)
        <*> pure (valFrom "refreshToken" vals)
        <*> pure (valFrom "accessExpiry" vals)
        <*> pure (valFrom "refreshExpiry" vals)

valFrom
    :: Convertible SqlValue (Maybe a)
    => String
    -> [(String, SqlValue)]
    -> Maybe a
valFrom colName allVals = case maybeSqlVal of
    (Just sqlVal) -> fromSql sqlVal
    _             -> Nothing
    where maybeSqlVal = snd <$> (find (\v -> fst v == colName) allVals)
