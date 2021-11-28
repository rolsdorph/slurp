{-# LANGUAGE OverloadedStrings #-}

module Auth where

import           DBUtil (HasConnection)
import           TokenDB
import           UserDB
import           Types

import qualified Data.ByteString               as B
import qualified Data.ByteString.Lazy          as L
import qualified Data.ByteString.Search        as SS
import qualified Network.HTTP.Types            as HTTP

type TokenVerifier = L.ByteString -> IO (Either String User)

extractBearerToken :: HTTP.Header -> L.ByteString
extractBearerToken header = SS.replace "Bearer " ("" :: B.ByteString) (snd header)

-- Generates a token for the given user ID
login :: String -> HasConnection (Either L.ByteString String)
login uuid = do
    maybeToken <- createToken uuid
    case maybeToken of
        (Just token) -> return $ Right token
        _            -> return $ Left "Failed to create token"

-- Extracts the user ID associated with the given token
verifyToken :: L.ByteString -> HasConnection (Either String User)
verifyToken token = do
    maybeUserId <- getTokenUserId token
    case maybeUserId of
        (Just uuid) -> getUser uuid
        _ -> return $ Left "Unknown token"

logoutUser :: String -> HasConnection ()
logoutUser = deleteUserTokens
