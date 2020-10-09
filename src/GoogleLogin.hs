{-# LANGUAGE OverloadedStrings #-}

module GoogleLogin where

import Control.Monad.Except
import Control.Lens.Operators
import           Crypto.JOSE.JWS
import           Crypto.JWT
import           Data.Aeson
import qualified Data.ByteString.Lazy          as L
import qualified Data.ByteString.UTF8          as U

googleIssuer1 = "accounts.google.com"
googleIssuer2 = "https://accounts.google.com"

googleValSettings :: StringOrURI -> JWTValidationSettings
googleValSettings clientId = defaultJWTValidationSettings (== clientId)
    & (jwtValidationSettingsIssuerPredicate .~ (\iss -> iss == googleIssuer1 || iss == googleIssuer2))

loadKeys :: FilePath -> IO (Maybe JWKSet)
loadKeys path = do
    keyFileData <- L.readFile path
    pure $ decode keyFileData

verifyToken :: JWKSet -> StringOrURI -> SignedJWT -> IO (Either JWTError ClaimsSet)
verifyToken keys clientId token = runExceptT $ verifyClaims (googleValSettings clientId) keys token

loadKey :: FilePath -> IO (Maybe JWK)
loadKey path = do
    keyFileData <- L.readFile path
    pure $ decode keyFileData
