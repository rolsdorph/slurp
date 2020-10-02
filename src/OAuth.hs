{-# LANGUAGE OverloadedStrings #-}

module OAuth where

import qualified Data.ByteString.UTF8          as U
import qualified Data.ByteString               as B
import qualified Crypto.Hash.MD5               as MD5
import qualified Data.ByteString.Base16        as B16
import           Text.Regex.PCRE.Light

-- Type definitions
type Nonce = B.ByteString
type Realm = B.ByteString
type ClientId = U.ByteString
type ClientSecret = U.ByteString
type AppId = String
type DeviceId = String
type State = String

-- Constructs an OAuth redirect url
buildOauthRedirect
    :: ClientId -> ClientSecret -> AppId -> DeviceId -> State -> String
buildOauthRedirect clientId clientSecret appId deviceId state =
    "https://api.meethue.com/oauth2/auth?"
        ++ "clientid="
        ++ U.toString clientId
        ++ "&appid="
        ++ appId
        ++ "&deviceid="
        ++ deviceId
        ++ "&state="
        ++ state
        ++ "&response_type=code"

realmex = compile "realm=\"([^\"]+)\"" []
noncex = compile "nonce=\"([^\"]+)\"" []
-- Extracts the realm and nonce from a WWW-Authenticate response header
extractNonceAndRealm :: U.ByteString -> Maybe (U.ByteString, U.ByteString)
extractNonceAndRealm header = do
    [_, realm] <- match realmex header []
    [_, nonce] <- match noncex header []
    pure (realm, nonce)

-- Given a nonce and a realm, builds the response hash for an OAuth digest header
buildResponse :: ClientId -> ClientSecret -> Nonce -> Realm -> B.ByteString
buildResponse clientId clientSecret nonce realm = B16.encode
    $ MD5.hash (hash1 <> ":" <> nonce <> ":" <> hash2)
  where
    hash1 =
        B16.encode $ MD5.hash (clientId <> ":" <> realm <> ":" <> clientSecret)
    hash2 = B16.encode $ MD5.hash "POST:/oauth2/token"

-- Generates the Digest auth header from the given nonce and realm
buildDigestHeader :: ClientId -> ClientSecret -> Nonce -> Realm -> B.ByteString
buildDigestHeader clientId clientSecret nonce realm =
    "Digest username=\""
        <> clientId
        <> "\", realm=\""
        <> realm
        <> "\""
        <> ", nonce=\""
        <> nonce
        <> "\", uri=\"/oauth2/token\""
        <> ", response=\""
        <> buildResponse clientId clientSecret nonce realm
        <> "\""
