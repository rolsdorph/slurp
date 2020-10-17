{-# LANGUAGE OverloadedStrings #-}

module Auth where

import           TokenDB
import           UserDB
import           Util
import           Types

import qualified Data.ByteString               as B
import qualified Data.ByteString.Lazy          as L
import qualified Data.ByteString.Search        as SS
import qualified Network.HTTP.Types            as HTTP
import           Data.Aeson

extractBearerToken :: HTTP.Header -> L.ByteString
extractBearerToken header = SS.replace "Bearer " ("" :: B.ByteString) (snd header)

-- Generates a token for the given user ID
login :: String -> IO (Either L.ByteString String)
login uuid = do
    maybeToken <- createToken uuid
    case maybeToken of
        (Just token) -> pure $ Right token
        _            -> pure $ Left "Failed to create token"

-- Extracts the user ID associated with the given token
verifyToken :: L.ByteString -> IO (Maybe User)
verifyToken token = do
    maybeUserId <- getTokenUserId token
    case maybeUserId of
        (Just uuid) -> getUser uuid
        _ -> pure Nothing

logoutUser :: String -> IO ()
logoutUser = deleteUserTokens
