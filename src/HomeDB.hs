{-# LANGUAGE OverloadedStrings #-}

module HomeDB
    where

import Types
import DBUtil

import           Data.Maybe
import           Data.List
import           Data.Time.Clock
import           Database.HDBC
import           Database.HDBC.Sqlite3
import           Data.UUID.V4
import qualified Data.ByteString.Lazy          as L
import Control.Monad.Except (ExceptT, liftIO, throwError)
import Control.Monad.Reader (ReaderT, ask)

dbName = "/home/mads/dev/minimal-hue-metrics-website/testdb.sqlite3"
homeTableName = "homes" :: String

createStmt =
    "CREATE TABLE IF NOT EXISTS "
        ++ homeTableName
        ++ " (\
       \ uuid text NOT NULL, \
       \ datakey text NOT NULL, \
       \ ownerId text NOT NULL, \
       \ state text DEFAULT 'Pending',\
       \ oauthState text NULL,\
       \ accessToken text NULL,\
       \ accessExpiry datetime NULL,\
       \ refreshToken text NULL,\
       \ refreshExpiry datetime NULL,\
       \ createdAt datetime NULL,\
       \ hueUsername text NULL)"

setupDb :: HasConnection ()
setupDb = do
    conn <- ask
    liftIO $ run conn createStmt []
    liftIO $ commit conn

-- Stores the updated home
updateHome :: Home -> HasConnection (Maybe Home)
updateHome newHome = do
    conn        <- ask
    numUpdated <- liftIO $ run
        conn
        ("UPDATE "
        ++ homeTableName
        ++ " SET state=?, accessToken=?, accessExpiry=?, refreshToken=?, refreshExpiry=?, hueUsername=?"
        ++ " WHERE uuid=?"
        ) [toSql $ state newHome
        , toSql $ accessToken newHome
        , toSql $ accessExpiry newHome
        , toSql $ accessToken newHome
        , toSql $ accessExpiry newHome
        , toSql $ hueUsername newHome
        , toSql $ uuid newHome
        ]
    liftIO $ commit conn
    liftIO $ disconnect conn

    case numUpdated of
        1 -> return $ Just newHome
        _ -> return Nothing

-- Stores a Home in the database
storeHome :: Home -> ExceptT L.ByteString HasConnection Home
storeHome (PreCreationHome datakey ownerId createdAt verificationState) = do
  conn <- ask
  
  -- Randomness for UUID and state
  uuid <- liftIO $ show <$> nextRandom
  oauthState <- liftIO $ show <$> nextRandom

  -- Actual insert
  numInserted <-
    liftIO $
      run
        conn
        ( "INSERT INTO "
            ++ homeTableName
            ++ "(uuid, datakey, ownerId, createdAt, oauthState, state) \
               \ VALUES (?, ?, ?, ?, ?, ?)"
        )
        [ toSql uuid,
          toSql datakey,
          toSql ownerId,
          toSql createdAt,
          toSql $ Just oauthState,
          toSql verificationState
        ]
  liftIO $ commit conn
  liftIO $ disconnect conn

  case numInserted of
    1 ->
      return
        Home
          { uuid = uuid,
            homeDataKey = datakey,
            ownerId = ownerId,
            createdAt = createdAt,
            state = verificationState,
            oauthState = Just oauthState,
            accessToken = Nothing,
            refreshToken = Nothing,
            accessExpiry = Nothing,
            refreshExpiry = Nothing,
            hueUsername = Nothing
          }
    _ -> throwError "Failed to create home"

-- Retrieves all Homes for the given ownerId
getUserHomes :: String -> HasConnection [Home]
getUserHomes ownerId = do
    conn <- ask
    stmt <- liftIO $ prepare conn ("SELECT * FROM " ++ homeTableName ++ " WHERE ownerId = ?")
    res <- liftIO $ execute stmt [toSql ownerId]
    homes <- liftIO $ fetchAllRowsAL stmt
    return $ mapMaybe parseHomeRow homes

-- Retrieves all Homes that are in the Verified  state
getVerifiedHomes :: HasConnection [Home]
getVerifiedHomes = do
    conn <- ask
    stmt <- liftIO $ prepare conn ("SELECT * FROM " ++ homeTableName ++ " WHERE state = 'Verified'")
    res <- liftIO $ execute stmt []
    homes <- liftIO $ fetchAllRowsAL stmt
    return $ mapMaybe parseHomeRow homes

-- Retrieves the Home associated with the given OAuth verification state
getOauthPendingHome :: String -> HasConnection (Maybe Home)
getOauthPendingHome state = do
    conn <- ask
    stmt <- liftIO $ prepare conn
                    ("SELECT * FROM " ++ homeTableName ++ " WHERE state = 'Pending' AND oauthState = ?")
    numRows  <- liftIO $ execute stmt [toSql state]
    firstHit <- liftIO $ fetchRowAL stmt

    case firstHit of
        (Just row) -> return $ parseHomeRow row
        _          -> return Nothing

-- Retrieves a Home from the database
getHome :: Maybe String -> HasConnection (Maybe Home)
getHome (Just uuid) = do
    conn <- ask
    stmt <- liftIO $ prepare conn
                    ("SELECT * FROM " ++ homeTableName ++ " WHERE uuid = ?")
    numRows  <- liftIO $ execute stmt [toSql uuid]
    firstHit <- liftIO $ fetchRowAL stmt

    case firstHit of
        (Just row) -> return $ parseHomeRow row
        _          -> return Nothing
getHome Nothing = return Nothing

parseHomeRow :: [(String, SqlValue)] -> Maybe Home
parseHomeRow vals =
    Home
        <$> valFrom "uuid" vals
        <*> valFrom "datakey" vals
        <*> valFrom "ownerId" vals
        <*> valFrom "createdAt" vals
        <*> (fromString <$> valFrom "state" vals)
        <*> valFrom "oauthState" vals
        <*> pure (valFrom "accessToken" vals)
        <*> pure (valFrom "refreshToken" vals)
        <*> pure (valFrom "accessExpiry" vals)
        <*> pure (valFrom "refreshExpiry" vals)
        <*> pure (valFrom "hueUsername" vals)
