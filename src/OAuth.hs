{-# LANGUAGE OverloadedStrings #-}

module OAuth where

import qualified Data.ByteString.Lazy          as L
import qualified Data.ByteString.UTF8          as U
import qualified Data.ByteString               as B
import qualified Crypto.Hash.MD5               as MD5
import qualified Data.ByteString.Base16        as B16
import           Text.Regex.PCRE.Light
import           System.Environment

import Util

-- Type definitions
type Nonce = B.ByteString
type Realm = B.ByteString
type ClientId = U.ByteString
type ClientSecret = U.ByteString
type AppId = String
type DeviceId = String
type State = String

data OAuthCreds = OAuthCreds {
    clientId :: ClientId,
    clientSecret :: ClientSecret,
    appId :: AppId,
    deviceId :: DeviceId
}

-- Attempts to read OAuth variables from the environment
readCreds :: IO (Maybe OAuthCreds)
readCreds = do
    id       <- lookupEnv "clientId"
    secret   <- lookupEnv "clientSecret"
    appId    <- lookupEnv "appId"
    deviceId <- lookupEnv "deviceId"
    pure
        $   OAuthCreds
        <$> (U.fromString <$> id)
        <*> (U.fromString <$> secret)
        <*> appId
        <*> deviceId

-- Constructs an OAuth redirect url
buildOauthRedirect :: OAuthCreds -> State -> String
buildOauthRedirect creds state =
    "https://api.meethue.com/oauth2/auth?"
        ++ "clientid="
        ++ U.toString (clientId creds)
        ++ "&appid="
        ++ appId creds
        ++ "&deviceid="
        ++ deviceId creds
        ++ "&state="
        ++ state
        ++ "&response_type=code"

realmex = compile "realm=\"([^\"]+)\"" []
noncex = compile "nonce=\"([^\"]+)\"" []
-- Extracts the realm and nonce from a WWW-Authenticate response header
extractNonceAndRealm :: U.ByteString -> Either L.ByteString (U.ByteString, U.ByteString)
extractNonceAndRealm header = do
    let realm = extractFromHeader "realm" header
    let nonce = extractFromHeader "nonce" header
    combineEithers realm nonce

extractFromHeader :: B.ByteString -> U.ByteString -> Either L.ByteString U.ByteString
extractFromHeader name header = do
    let re = compile (name <> "=\"([^\"]+)\"") []

    let maybeHits = match re header []
    case maybeHits of
         Just [full, hit] -> Right hit
         _ -> Left (L.fromStrict name <> " not found in header from upstream")

-- Given a nonce and a realm, builds the response hash for an OAuth digest header
buildResponse :: ClientId -> ClientSecret -> Nonce -> Realm -> B.ByteString
buildResponse clientId clientSecret nonce realm = B16.encode
    $ MD5.hash (hash1 <> ":" <> nonce <> ":" <> hash2)
  where
    hash1 =
        B16.encode $ MD5.hash (clientId <> ":" <> realm <> ":" <> clientSecret)
    hash2 = B16.encode $ MD5.hash "POST:/oauth2/token"

-- Generates the Digest auth header from the given nonce and realm
buildDigestHeader :: OAuthCreds -> Nonce -> Realm -> B.ByteString
buildDigestHeader creds nonce realm =
    "Digest username=\""
        <> clientId creds
        <> "\", realm=\""
        <> realm
        <> "\""
        <> ", nonce=\""
        <> nonce
        <> "\", uri=\"/oauth2/token\""
        <> ", response=\""
        <> buildResponse (clientId creds) (clientSecret creds) nonce realm
        <> "\""
