module InfluxDB
    where

import           Database.HDBC
import           Database.HDBC.Sqlite3
import           Data.Maybe
import           Data.UUID.V4
import           DBUtil
import           Types
import           Util

dbName = "/home/mads/dev/minimal-hue-metrics-website/testdb.sqlite3"
influxTableName = "influxes" :: String

createStmt =
    "CREATE TABLE IF NOT EXISTS "
        ++ influxTableName
        ++ " (\
       \ uuid text NOT NULL, \
       \ ownerId text NOT NULL, \
       \ createdAt datetime NULL,\
       \ influxHost text NOT NULL,\
       \ influxPort text NOT NULL,\
       \ influxTLS boolean NOT NULL,\
       \ influxUsername text NOT NULL,\
       \ influxPassword text NOT NULL)"

setupDb = do
    conn <- connectSqlite3 dbName
    run conn createStmt []
    commit conn
    disconnect conn

-- Stores a InfluxSink in the database
storeInfluxSink :: InfluxSink -> IO (Maybe InfluxSink)
storeInfluxSink influx = do
    uuid        <- show <$> nextRandom

    conn        <- connectSqlite3 dbName
    numInserted <- run
        conn
        ("INSERT INTO "
        ++ influxTableName
        ++ "(uuid, ownerId, createdAt, influxHost, influxPort, influxTLS, influxUsername, influxPassword) \
                                    \ VALUES (?, ?, ?, ?, ?, ?, ?, ?)"
        )
        [ toSql uuid
        , toSql $ influxOwnerId influx
        , toSql $ influxCreatedAt influx
        , toSql $ influxHost influx
        , toSql $ influxPort influx
        , toSql $ influxTLS influx
        , toSql $ influxUsername influx
        , toSql $ influxPassword influx
        ]
    commit conn
    disconnect conn

    case numInserted of
        1 -> return $ Just (influx { influxUuid = Just uuid })
        _ -> return Nothing

-- Retrieves all InfluxSinks for the given ownerId
getUserInfluxSinks :: String -> IO [InfluxSink]
getUserInfluxSinks ownerId = do
    conn <- connectSqlite3 dbName
    stmt <- prepare conn ("SELECT * FROM " ++ influxTableName ++ " WHERE ownerId = ?")
    res <- execute stmt [toSql ownerId]
    sinks <- fetchAllRowsAL stmt
    pure $ mapMaybe parseInfluxSinkRow sinks

parseInfluxSinkRow :: [(String, SqlValue)] -> Maybe InfluxSink
parseInfluxSinkRow vals =
    InfluxSink
        <$> valFrom "uuid" vals
        <*> pure (valFrom "ownerId" vals)
        <*> valFrom "influxHost" vals
        <*> valFrom "influxPort" vals
        <*> valFrom "influxTLS" vals
        <*> valFrom "influxUsername" vals
        <*> valFrom "influxPassword" vals
        <*> valFrom "createdAt" vals
