module SimpleSourceDB where

import           Control.Monad
import           Data.Aeson
import           Database.HDBC
import           Database.HDBC.Sqlite3
import           Data.Maybe
import           Data.UUID.V4
import           DBUtil
import           Types
import           Util


dbName = "/home/mads/dev/minimal-hue-metrics-website/testdb.sqlite3"
sourceTableName = "simplejsonsources" :: String

createStmt =
    "CREATE TABLE IF NOT EXISTS "
        ++ sourceTableName
        ++ " (\
       \ uuid text NOT NULL, \
       \ datakey text NOT NULL, \
       \ ownerId text NOT NULL, \
       \ createdAt datetime NULL,\
       \ url text NOT NULL, \
       \ authHeader text NULL, \
       \ tagMappings text NOT NULL, \
       \ fieldMappings text NOT NULL)"

setupDb = do
    conn <- connectSqlite3 dbName
    run conn createStmt []
    commit conn
    disconnect conn

storeSimpleSource
    :: SimpleShallowJsonSource -> IO (Maybe SimpleShallowJsonSource)
storeSimpleSource source = do
    uuid        <- show <$> nextRandom

    conn        <- connectSqlite3 dbName
    numInserted <- run
        conn
        ("INSERT INTO "
        ++ sourceTableName
        ++ "(uuid, datakey, ownerId, createdAt, url, authHeader, tagMappings, fieldMappings) \
                                    \ VALUES (?, ?, ?, ?, ?, ?, ?, ?)"
        )
        [ toSql uuid
        , toSql $ genericDataKey source
        , toSql $ shallowOwnerId source
        , toSql $ shallowCreatedAt source
        , toSql $ url source
        , toSql $ authHeader source
        , toSql $ encode (tagMappings source)
        , toSql $ encode (fieldMappings source)
        ]
    commit conn
    disconnect conn

    case numInserted of
        1 -> return $ Just (source { genericSourceId = Just uuid })
        _ -> return Nothing

getUserSimpleSources :: String -> IO [SimpleShallowJsonSource]
getUserSimpleSources ownerId = do
    conn <- connectSqlite3 dbName
    stmt <- prepare conn ("SELECT * FROM " ++ sourceTableName ++ " WHERE ownerId = ?")
    res <- execute stmt [toSql ownerId]
    sources <- fetchAllRowsAL stmt
    pure $ mapMaybe parseSimpleSourceRow sources

parseSimpleSourceRow :: [(String, SqlValue)] -> Maybe SimpleShallowJsonSource
parseSimpleSourceRow vals = do
    let maybeTags   = (decode <=< valFrom "tagMappings") vals
    let maybeFields = (decode <=< valFrom "fieldMappings") vals

    case (maybeTags, maybeFields) of
        (Just tags, Just fields) ->
            SimpleShallowJsonSource
                <$> valFrom "uuid"       vals
                <*> valFrom "datakey"    vals
                <*> valFrom "ownerId"    vals
                <*> valFrom "createdAt"  vals
                <*> valFrom "url"        vals
                <*> valFrom "authHeader" vals
                <*> tags
                <*> fields
        _ -> Nothing
