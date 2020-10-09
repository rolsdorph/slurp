{-# LANGUAGE OverloadedStrings #-}

module OAuth where

import qualified Data.ByteString.Lazy          as L
import qualified Data.ByteString.UTF8          as U
import qualified Data.ByteString               as B
import qualified Crypto.Hash.MD5               as MD5
import qualified Data.ByteString.Base16        as B16
import           Text.Regex.PCRE.Light

import Util
import Types

-- Type definitions
type Nonce = B.ByteString
type Realm = B.ByteString
type State = String

-- Constructs an OAuth redirect url
buildOauthRedirect :: AppCreds -> State -> String
buildOauthRedirect creds state =
    "https://api.meethue.com/oauth2/auth?"
        ++ "clientid="
        ++ U.toString (hueClientId creds)
        ++ "&appid="
        ++ hueAppId creds
        ++ "&deviceid="
        ++ hueDeviceId creds
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
buildDigestHeader :: AppCreds -> Nonce -> Realm -> B.ByteString
buildDigestHeader creds nonce realm =
    "Digest username=\""
        <> hueClientId creds
        <> "\", realm=\""
        <> realm
        <> "\""
        <> ", nonce=\""
        <> nonce
        <> "\", uri=\"/oauth2/token\""
        <> ", response=\""
        <> buildResponse (hueClientId creds) (hueClientSecret creds) nonce realm
        <> "\""
