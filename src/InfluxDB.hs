{-# LANGUAGE OverloadedStrings #-}

module InfluxDB
    where

import qualified Data.ByteString.Lazy as BL
import           Database.HDBC
import           Data.Maybe
import           Data.UUID.V4
import           DBUtil
import           Types
import           Util
import Control.Monad.Except (ExceptT, liftIO, throwError)
import Control.Monad.Reader (MonadReader, ReaderT, ask)

influxTableName = "influxes" :: String

createStmt =
    "CREATE TABLE IF NOT EXISTS "
        ++ influxTableName
        ++ " (\
       \ uuid text NOT NULL, \
       \ owner_id text NOT NULL, \
       \ created_at timestamp with time zone NULL,\
       \ influx_host text NOT NULL,\
       \ influx_port text NOT NULL,\
       \ influx_tls boolean NOT NULL,\
       \ influx_username text NOT NULL,\
       \ influx_password text NOT NULL,\
       \ influx_db_name text NOT NULL)"

setupDb :: HasConnection ()
setupDb = do
    conn <- ask
    liftIO $ run conn createStmt []
    liftIO $ commit conn

-- Stores a InfluxSink in the database
storeInfluxSink :: InfluxDefinition -> ExceptT BL.ByteString HasConnection InfluxSink
storeInfluxSink influx = do
    conn        <- ask

    uuid        <- liftIO $ show <$> nextRandom

    numInserted <- liftIO $ run
        conn
        ("INSERT INTO "
        ++ influxTableName
        ++ "(uuid, owner_id, created_at, influx_host, influx_port, influx_tls, influx_username, influx_password, influx_db_name) \
                                    \ VALUES (?, ?, ?, ?, ?, ?, ?, ?, ?)"
        )
        [ toSql uuid
        , toSql $ influxOwnerId influx
        , toSql $ influxCreatedAt influx
        , toSql $ influxHost influx
        , toSql $ influxPort influx
        , toSql $ influxTLS influx
        , toSql $ influxUsername influx
        , toSql $ influxPassword influx
        , toSql $ influxDbName influx
        ]
    liftIO $ commit conn

    case numInserted of
        1 -> return InfluxSink { influxUuid = uuid, influxDefinition = influx }
        _ -> throwError "Failed to store source."

-- Retrieves all InfluxSinks for the given ownerId
getUserInfluxSinks :: String -> HasConnection [InfluxSink]
getUserInfluxSinks ownerId = do
    conn <- ask

    stmt <- liftIO $ prepare conn ("SELECT * FROM " ++ influxTableName ++ " WHERE owner_id = ?")
    res <- liftIO $ execute stmt [toSql ownerId]
    sinks <- liftIO $ fetchAllRowsAL stmt
    return $ mapMaybe parseInfluxSinkRow sinks


parseInfluxSinkRow :: [(String, SqlValue)] -> Maybe InfluxSink
parseInfluxSinkRow vals =
  InfluxSink
    <$> valFrom "uuid" vals
    <*> ( InfluxDefinition <$> valFrom "owner_id" vals
            <*> valFrom "influx_host" vals
            <*> valFrom "influx_port" vals
            <*> valFrom "influx_tls" vals
            <*> valFrom "influx_username" vals
            <*> valFrom "influx_password" vals
            <*> valFrom "influx_db_name" vals
            <*> valFrom "created_at" vals
        )
