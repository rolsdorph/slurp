{-# LANGUAGE OverloadedStrings #-}

module GoogleLogin where

import Control.Lens
import Control.Monad.Except
import Control.Lens.Operators
import           Crypto.JOSE.JWS
import           Crypto.JWT
import           Data.Aeson
import qualified Data.ByteString.Lazy          as L
import qualified Data.ByteString.UTF8          as U
import qualified Data.ByteString.Char8         as C
import qualified Data.Text                     as T

import Util

googleIssuer1 = "accounts.google.com"
googleIssuer2 = "https://accounts.google.com"

-- TODO: Learn lens
extractUserId :: ClaimsSet -> Either L.ByteString L.ByteString
extractUserId claims = case maybeSub of
    (Just (Just x)) -> pure $ L.fromStrict (C.pack (T.unpack x))
    _               -> Left "JWT subject not found"
    where maybeSub = preview string <$> (claims ^. claimSub)

googleValSettings :: StringOrURI -> JWTValidationSettings
googleValSettings clientId = defaultJWTValidationSettings (== clientId)
    & (jwtValidationSettingsIssuerPredicate .~ (\iss -> iss == googleIssuer1 || iss == googleIssuer2))

loadKeys :: FilePath -> IO (Maybe JWKSet)
loadKeys path = do
    keyFileData <- L.readFile path
    pure $ decode keyFileData

pretty :: JWTError -> L.ByteString
pretty = L.fromStrict . C.pack . show

verifyToken :: JWKSet -> StringOrURI -> SignedJWT -> IO (Either L.ByteString ClaimsSet)
verifyToken keys clientId token = do
    res <- runExceptT $ verifyClaims (googleValSettings clientId) keys token
    pure $ mapLeft pretty res

loadKey :: FilePath -> IO (Maybe JWK)
loadKey path = do
    keyFileData <- L.readFile path
    pure $ decode keyFileData
