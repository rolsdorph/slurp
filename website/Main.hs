{-# LANGUAGE OverloadedStrings #-}

module Main where

import           Types
import           Lib
import           OAuth
import           Html

import           HomeDB
import           Network.Wai
import qualified Network.Wai.Handler.Warp      as W
                                                ( run )

import           Control.Monad.IO.Class
import           Network.Wai.Parse
import           Network.Wai.Middleware.RequestLogger
import qualified Network.HTTP.Types            as HTTP
import           Data.Aeson
import           Data.Aeson.Types
import qualified Data.ByteString               as B
import qualified Data.ByteString.Lazy          as L
import qualified Data.ByteString.Char8         as C
import qualified Data.ByteString.UTF8          as U
import           Data.List
import           Data.Fixed
import qualified Data.Text                     as T
import           Data.Time
import           Data.Time.Clock
import qualified Data.Vector                   as V
import           Network.HTTP.Req


clientId = "" :: B.ByteString
clientSecret = ""
appId = "hueinflux"
deviceId = "hueinflux"

app :: Application
app request respond = do
    reqBodyParsed <- parseRequestBodyEx defaultParseRequestBodyOptions
                                        lbsBackEnd
                                        request

    response <- case (requestMethod request, rawPathInfo request) of
        ("GET" , "/"     ) -> pure index
        ("POST", "/homes") -> postHome $ fst reqBodyParsed
        ("GET", "/callback") -> oauthCallback (queryString request)
        (_     , _       ) -> pure notFound

    respond response

main :: IO ()
main = do
    -- Create necessary tables if they don't exist
    setupDb

     -- Flex some IO
    putStrLn "http://localhost:8080/"

     -- Start serving requests
    W.run 8080 $ logStdout app

-- Helpers for generating responses
redirectResponse :: String -> Response
redirectResponse target =
    responseBuilder HTTP.found302 [(HTTP.hLocation, C.pack target)] mempty

index :: Response
index =
    responseFile HTTP.status200 [("Content-Type", "text/html")] "index.html" Nothing

notFound :: Response
notFound =
    responseLBS HTTP.status404 [("Content-Type", "text/plain")] "Not found :("

badRequest :: L.ByteString -> Response
badRequest errMsg =
    responseLBS HTTP.status400 [("Content-Type", "text/plain")] ("Bad request >:( - " <> errMsg)

err500 :: L.ByteString -> Response
err500 = responseLBS HTTP.status500 [("Content-Type", "text/plain")]

oauthRedirect :: Home -> IO Response
oauthRedirect home = case oauthState home of
    Just state -> return
        (redirectResponse (buildOauthRedirect clientId clientSecret appId deviceId state))
    _ -> return
        (err500 "Internal Server Error - did not find expected OAuth state")

-- Gets the value of the given param, if present
getParamValue :: C.ByteString -> HTTP.Query -> Maybe String
getParamValue name params = case find (\p -> fst p == name) params of
    (Just val) -> C.unpack <$> snd val
    _          -> Nothing

-- GET /callback
oauthCallback :: HTTP.Query -> IO Response
oauthCallback queryParams = do
    let state = getParamValue "state" queryParams
    let code = getParamValue "code" queryParams

    case (state, code) of
         (Just stateVal, Just codeVal) -> callbackResponse stateVal (U.fromString codeVal)
         _ -> return (badRequest "Code and state parameters must both be present")

-- Generates a callback response for the given state and code param
callbackResponse :: String -> B.ByteString -> IO Response
callbackResponse state code = do
    maybeHome <- getOauthPendingHome state

    case maybeHome of 
         (Just h) -> finishOAuthFlow code h
         _ -> return notFound


type Username = String
-- Obtains an allow-listed username for the given home
generateUsername :: Home -> IO (Maybe Username)
generateUsername home = do
    let buttonPayload = object ["linkbutton" .= True]
    let usernamePayload = object ["devicetype" .= appId]

    runReq defaultHttpConfig $ 
         case accessToken home of
            (Just token) -> do
                buttonRes <- req
                    PUT
                    (https "api.meethue.com" /: "bridge" /: "0" /: "config")
                    (ReqBodyJson buttonPayload)
                    jsonResponse
                    (header "Authorization" (U.fromString $ "Bearer " ++ token))
                
                liftIO $ print "Got button response: "
                liftIO $ print (responseBody buttonRes :: Value)

                usernameRes <- req
                    POST
                    (https "api.meethue.com" /: "bridge")
                    (ReqBodyJson usernamePayload)
                    jsonResponse
                    (header "Authorization" (U.fromString $ "Bearer " ++ token))

                liftIO $ print "Got username response: "
                liftIO $ print (responseBody usernameRes)

                pure $ parseMaybe extractUsername (responseBody usernameRes)

            _ -> do
                liftIO $ print "No token found"
                pure Nothing

extractUsername :: Value -> Parser String
extractUsername = withArray "usernameList" $ \a -> case V.toList a of
    (x : _) -> withObject
        "usernameObject"
        (\o -> do
            success <- o .: "success"
            success .: "username"
        )
        x
    _ -> fail "Did not get a list of potential username objects"

finishOAuthFlow :: B.ByteString -> Home -> IO Response
finishOAuthFlow code home = do
    tokens <- getOAuthTokens code
    case tokens of 
         (Just resp) -> do
             currentTime <- getCurrentTime
             let accessExpires = addUTCTime (secondsToNominalDiffTime $ accessTokenExpiresIn resp) currentTime
             let refreshExpires = addUTCTime (secondsToNominalDiffTime $ refreshTokenExpiresIn resp) currentTime

             let newHome = home { 
                            accessToken = Just $ respAccessToken resp, 
                            refreshToken = Just $ respRefreshToken resp,
                            accessExpiry = Just accessExpires,
                            refreshExpiry = Just refreshExpires,
                            state = UsernamePending
                 }
             updatedHome <- updateHome newHome
             case updatedHome of
                  (Just h) -> generateAndSaveUsername h
                  Nothing -> return $ err500 "Failed to store OAuth information"
         _ -> return $ err500 "Actual error: Lacking token"

generateAndSaveUsername :: Home -> IO Response
generateAndSaveUsername home = do
    maybeUsername <- generateUsername home
    case maybeUsername of
         (Just username) -> do
             let newHome = home { hueUsername = Just username
                                , state = Verified }
             updatedHome <- updateHome newHome
             
             case updatedHome of
                  (Just _) -> return index
                  Nothing -> return $ err500 "Couldn't store username"
         _ -> return $ err500 "Failed to extract username"

-- Finishes an OAuth Flow with the given access code,
getOAuthTokens :: B.ByteString -> IO (Maybe OAuthResponse)
getOAuthTokens code = runReq defaultHttpConfig { httpConfigCheckResponse = \_ _ _ -> Nothing} $ do
    res <- req
        POST
        (https "api.meethue.com" /: "oauth2" /: "token")
        NoReqBody
        ignoreResponse
        (("grant_type" =: ("authorization_code" :: T.Text)) <> ("code" =: U.toString code))

    let oauthData = do
            digestHeader <- responseHeader res "WWW-Authenticate"
            (realm, nonce) <- extractNonceAndRealm  digestHeader
            pure $ finalOAuth realm nonce code

    case oauthData of
         (Just ioResp) -> do
             oresp <- liftIO ioResp
             pure $ Just oresp
         _ -> pure Nothing


finalOAuth :: B.ByteString -> B.ByteString -> B.ByteString -> IO OAuthResponse
finalOAuth realm nonce code = do
   let authHeader = buildDigestHeader clientId clientSecret nonce realm

   runReq defaultHttpConfig $ do
       res <- req
                POST
                (https "api.meethue.com" /: "oauth2" /: "token")
                NoReqBody
                jsonResponse
                (header "Authorization" authHeader <> ("grant_type" =: ("authorization_code" :: T.Text)) <> ("code" =: U.toString code))
       return $ responseBody res
           

data OAuthResponse = OAuthResponse {respAccessToken::String, respRefreshToken :: String, accessTokenExpiresIn :: Pico, refreshTokenExpiresIn :: Pico }
    deriving Show
instance FromJSON OAuthResponse where
    parseJSON = withObject "oauthResponse" $ \o ->
        OAuthResponse
            <$> o .:  "access_token"
            <*> o .:  "refresh_token"
            <*> (read <$> o .: "access_token_expires_in")
            <*> (read <$> o .: "refresh_token_expires_in")

-- POST /homes
postHome :: [Param] -> IO Response
postHome params = do
    parsedHome <- homeFrom params

    case parsedHome of
        Just x -> do
            print "Storing home..."
            storeHomeRes <- storeHome x
            case storeHomeRes of
                (Just storedHome) -> oauthRedirect storedHome
                Nothing -> return $ err500 "Couldn't store home"

        Nothing -> return (badRequest "Malformed request body")

-- Attempts to construct a Home DTO from a set of parameters originating from a HTTP request
homeFrom :: [Param] -> IO (Maybe Home)
homeFrom params = do
    currentTime <- getCurrentTime

    return
        (   Home
        <$> pure Nothing
        <*> (C.unpack . snd <$> find (paramNamed "influxHost") params)
        <*> (read . C.unpack . snd <$> find (paramNamed "influxPort") params)
        <*> (isCheckboxSet <$> find (paramNamed "influxTLS") params)
        <*> (C.unpack . snd <$> find (paramNamed "influxHost") params)
        <*> (C.unpack . snd <$> find (paramNamed "influxPassword") params)
        <*> pure currentTime
        <*> pure OAuthPending
        <*> pure Nothing
        <*> pure Nothing
        <*> pure Nothing
        <*> pure Nothing
        <*> pure Nothing
        <*> pure Nothing
        )

