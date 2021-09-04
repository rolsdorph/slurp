{-# LANGUAGE OverloadedStrings #-}

module SimpleSourceDB where

import           Control.Monad
import           Data.Aeson
import qualified Data.ByteString.Lazy as BL
import           Database.HDBC
import           Database.HDBC.Sqlite3
import           Data.Maybe
import           Data.UUID.V4
import           DBUtil
import           Types
import           Util
import Control.Monad.Except (ExceptT, liftIO, throwError)


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

class Monad m => MonadSimpleSource m where
  -- TODO: Can we generalize this rather than explicitly returning ExceptT?
  storeSimpleSource :: SimpleSourceDefinition -> ExceptT BL.ByteString m SimpleShallowJsonSource
  getUserSimpleSources :: String -> ExceptT BL.ByteString m [SimpleShallowJsonSource]

instance MonadSimpleSource IO where
  storeSimpleSource source = do
      uuid        <- liftIO $ show <$> nextRandom

      conn        <- liftIO $ connectSqlite3 dbName
      numInserted <- liftIO $ run
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
      liftIO $ commit conn
      liftIO $ disconnect conn

      case numInserted of
          1 -> return (SimpleShallowJsonSource { genericSourceId = uuid, ssDefinition = source })
          _ -> throwError "Failed to store source"

  getUserSimpleSources ownerId = do
      conn <- liftIO $ connectSqlite3 dbName
      stmt <- liftIO $ prepare conn ("SELECT * FROM " ++ sourceTableName ++ " WHERE ownerId = ?")
      res <- liftIO $ execute stmt [toSql ownerId]
      sources <- liftIO $ fetchAllRowsAL stmt
      return $ mapMaybe parseSimpleSourceRow sources


parseSimpleSourceRow :: [(String, SqlValue)] -> Maybe SimpleShallowJsonSource
parseSimpleSourceRow vals = do
  let maybeTags = (decode <=< valFrom "tagMappings") vals
  let maybeFields = (decode <=< valFrom "fieldMappings") vals

  case (maybeTags, maybeFields) of
    (Just tags, Just fields) ->
      SimpleShallowJsonSource
        <$> valFrom "uuid" vals
        <*> ( SimpleSourceDefinition <$> valFrom "datakey" vals
                <*> valFrom "ownerId" vals
                <*> valFrom "createdAt" vals
                <*> valFrom "url" vals
                <*> valFrom "authHeader" vals
                <*> tags
                <*> fields
            )
    _ -> Nothing
