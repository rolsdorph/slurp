{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE FlexibleInstances #-}

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
import Control.Monad.Reader (ask)


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

setupDb :: HasConnection ()
setupDb = do
    conn <- ask
    liftIO $ run conn createStmt []
    liftIO $ commit conn

class Monad m => MonadSimpleSource m where
  -- TODO: Can we generalize this rather than explicitly returning ExceptT?
  storeSimpleSource :: SimpleSourceDefinition -> ExceptT BL.ByteString m SimpleShallowJsonSource
  getUserSimpleSources :: String -> ExceptT BL.ByteString m [SimpleShallowJsonSource]

instance MonadSimpleSource HasConnection where
  storeSimpleSource source = do
      conn        <- ask
      uuid        <- liftIO $ show <$> nextRandom

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

      case numInserted of
          1 -> return (SimpleShallowJsonSource { genericSourceId = uuid, ssDefinition = source })
          _ -> throwError "Failed to store source"

  getUserSimpleSources ownerId = do
      conn <- ask
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
