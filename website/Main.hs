{-# LANGUAGE OverloadedStrings #-}

module Main where

import           Types
import           Lib
import           OAuth
import           Html
import           Util
import           Secrets
import           GoogleLogin

import           HomeDB
import           Network.Wai
import qualified Network.Wai.Handler.Warp      as W
                                                ( run )
import qualified Crypto.JWT                    as J
import           Crypto.JOSE.JWK
import qualified Control.Lens                  as Lens
import           Control.Monad.IO.Class
import           Network.Wai.Parse
import           Network.Wai.Middleware.RequestLogger
import qualified Network.HTTP.Types            as HTTP
import           Data.Aeson
import           Data.Aeson.Types
import qualified Data.ByteString               as B
import qualified Data.ByteString.Lazy          as L
import qualified Data.ByteString.Lazy.Char8    as LB
import qualified Data.ByteString.Char8         as C
import qualified Data.ByteString.UTF8          as U
import           Data.List
import           Data.Fixed
import qualified Data.Text                     as T
import           Data.Time
import           Data.Time.Clock
import qualified Data.Vector                   as V
import           Network.HTTP.Req

app :: AppCreds -> JWKSet -> Application
app creds keys request respond = do
    reqBodyParsed <- parseRequestBodyEx defaultParseRequestBodyOptions
                                        lbsBackEnd
                                        request

    response <- case (requestMethod request, rawPathInfo request) of
        ("GET" , "/"        )   -> pure index
        ("GET" , "/login"   )   -> pure loginLanding
        ("POST", "/googleAuth") -> googleAuth creds keys (fst reqBodyParsed)
        ("POST", "/homes"   )   -> postHome creds (fst reqBodyParsed)
        ("GET" , "/callback")   -> oauthCallback creds (queryString request)
        (_     , _          )   -> pure notFound

    respond response

main :: IO ()
main = do
    -- Create necessary tables if they don't exist
    setupDb

     -- Flex some IO
    putStrLn "http://localhost:8080/"

    maybeCreds <- readCreds
    maybeKeys <- loadKeys "resources/certs.json"
    case (maybeCreds, maybeKeys) of
        (Just oauthCreds, Just keys) -> W.run 8080 $ logStdout (app oauthCreds keys)
        _                            -> putStrLn "Some secrets not loaded, not running"

-- Helpers for generating responses
redirectResponse :: String -> Response
redirectResponse target =
    responseBuilder HTTP.found302 [(HTTP.hLocation, C.pack target)] mempty

index :: Response
index = responseFile HTTP.status200
                     [("Content-Type", "text/html")]
                     "index.html"
                     Nothing

loginLanding :: Response
loginLanding = responseFile HTTP.status200
                     [("Content-Type", "text/html")]
                     "login-landing.html"
                     Nothing

notFound :: Response
notFound =
    responseLBS HTTP.status404 [("Content-Type", "text/plain")] "Not found :("

badRequest :: L.ByteString -> Response
badRequest errMsg = responseLBS HTTP.status400
                                [("Content-Type", "text/plain")]
                                ("Bad request >:( - " <> errMsg)

err500 :: L.ByteString -> Response
err500 = responseLBS HTTP.status500 [("Content-Type", "text/plain")]

-- Gets the value of the given param
getParamValue :: L.ByteString -> HTTP.Query -> Either L.ByteString String
getParamValue name params =
    justOrErr (name <> " not found in params") (lookupParamValue name params)

-- Gets the value of the given param, if present
lookupParamValue :: L.ByteString -> HTTP.Query -> Maybe String
lookupParamValue name params =
    case find (\p -> fst p == L.toStrict name) params of
        (Just val) -> U.toString <$> snd val
        _          -> Nothing

-- POST /googleAuth
googleAuth :: AppCreds -> JWKSet -> [Param] -> IO Response
googleAuth creds keys params = do
    maybeId <- validateAndGetId creds keys params
    case maybeId of
        (Right uid) -> pure $ err500 ("Could not log in " <> uid)
        (Left  err) -> pure $ err500 err

validateAndGetId
    :: AppCreds -> JWKSet -> [Param] -> IO (Either L.ByteString L.ByteString)
validateAndGetId creds keys params = do
    (Right googleClientId) <- pure $ buildGoogleClientId creds
    (Right tokenParam    ) <- pure $ lookupParam "idtoken" params
    (Right token         ) <- pure
        ((J.decodeCompact . utf8ToLbs . snd) tokenParam :: Either
              J.JWTError
              J.SignedJWT
        )
    (Right claims) <- verifyToken keys googleClientId token
    (Right uid   ) <- pure $ extractUserId claims
    pure $ Right uid

buildGoogleClientId :: AppCreds -> Either L.ByteString J.StringOrURI
buildGoogleClientId creds = do
    let res = Lens.preview J.stringOrUri (googleClientId creds)
    case res of
        (Just r) -> pure r
        _        -> Left "Failed to parse client ID"

-- POST /homes
postHome :: AppCreds -> [Param] -> IO Response
postHome creds params = do
    parsedHome <- homeFrom params

    case parsedHome of
        Right x -> do
            print "Storing home..."
            storeHomeRes <- storeHome x
            case storeHomeRes of
                (Just storedHome) -> oauthRedirect creds storedHome
                Nothing           -> return $ err500 "Couldn't store home"

        Left e -> return (badRequest $ "Malformed request body: " <> e)

oauthRedirect :: AppCreds -> Home -> IO Response
oauthRedirect creds home = case oauthState home of
    Just state -> return (redirectResponse (buildOauthRedirect creds state))
    _          -> return
        (err500 "Internal Server Error - did not find expected OAuth state")


-- GET /callback
oauthCallback :: AppCreds -> HTTP.Query -> IO Response
oauthCallback creds queryParams = case res of
    Right (stateVal, codeVal) ->
        callbackResponse creds stateVal (U.fromString codeVal)
    Left err -> return (badRequest err)

  where
    state = getParamValue "state" queryParams
    code  = getParamValue "code" queryParams
    res   = combineEithers state code


-- Generates a callback response for the given state and code param
callbackResponse :: AppCreds -> String -> B.ByteString -> IO Response
callbackResponse creds state code = do
    maybeHome <- getOauthPendingHome state

    case maybeHome of
        (Just h) -> finishOAuthFlow creds code h
        _        -> return notFound

finishOAuthFlow :: AppCreds -> B.ByteString -> Home -> IO Response
finishOAuthFlow creds code home = do
    tokens <- getOAuthTokens creds code
    case tokens of
        (Right resp) -> do
            currentTime <- getCurrentTime
            let accessExpires = addUTCTime
                    (secondsToNominalDiffTime $ accessTokenExpiresIn resp)
                    currentTime
            let refreshExpires = addUTCTime
                    (secondsToNominalDiffTime $ refreshTokenExpiresIn resp)
                    currentTime

            let newHome = home { accessToken   = Just $ respAccessToken resp
                               , refreshToken  = Just $ respRefreshToken resp
                               , accessExpiry  = Just accessExpires
                               , refreshExpiry = Just refreshExpires
                               , state         = UsernamePending
                               }
            updatedHome <- updateHome newHome
            case updatedHome of
                (Just h) -> generateAndSaveUsername creds h
                Nothing  -> return $ err500 "Failed to store OAuth information"
        (Left err) -> return $ err500 ("Internal Server Error: " <> err)

-- Finishes an OAuth Flow with the given access code,
getOAuthTokens
    :: AppCreds -> B.ByteString -> IO (Either L.ByteString OAuthResponse)
getOAuthTokens creds code =
    runReq defaultHttpConfig { httpConfigCheckResponse = \_ _ _ -> Nothing }
        $ do
              res <- req
                  POST
                  (https "api.meethue.com" /: "oauth2" /: "token")
                  NoReqBody
                  ignoreResponse
                  (  ("grant_type" =: ("authorization_code" :: T.Text))
                  <> ("code" =: U.toString code)
                  )

              let
                  oauthData = do
                      digestHeader <-
                          justOrErr
                                  "Missing WWW-Authenticate header in upstream response"
                              $ responseHeader res "WWW-Authenticate"
                      (realm, nonce) <- extractNonceAndRealm digestHeader
                      pure $ finalOAuth creds realm nonce code

              case oauthData of
                  (Right ioResp) -> do
                      oresp <- liftIO ioResp
                      pure $ pure oresp
                  (Left err) -> pure $ Left err


finalOAuth
    :: AppCreds
    -> B.ByteString
    -> B.ByteString
    -> B.ByteString
    -> IO OAuthResponse
finalOAuth creds realm nonce code = do
    let authHeader = buildDigestHeader creds nonce realm

    runReq defaultHttpConfig $ do
        res <- req
            POST
            (https "api.meethue.com" /: "oauth2" /: "token")
            NoReqBody
            jsonResponse
            (  header "Authorization" authHeader
            <> ("grant_type" =: ("authorization_code" :: T.Text))
            <> ("code" =: U.toString code)
            )
        return $ responseBody res

generateAndSaveUsername :: AppCreds -> Home -> IO Response
generateAndSaveUsername creds home = do
    maybeUsername <- generateUsername creds home
    case maybeUsername of
        (Right username) -> do
            let newHome =
                    home { hueUsername = Just username, state = Verified }
            updatedHome <- updateHome newHome

            case updatedHome of
                (Just _) -> return index
                Nothing  -> return $ err500 "Couldn't store username"
        (Left err) -> return $ err500 ("Failed to extract username: " <> err)

type Username = String
-- Obtains an allow-listed username for the given home
generateUsername :: AppCreds -> Home -> IO (Either L.ByteString Username)
generateUsername creds home = do
    let buttonPayload   = object ["linkbutton" .= True]
    let usernamePayload = object ["devicetype" .= hueAppId creds]

    runReq defaultHttpConfig $ case accessToken home of
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

            pure $ mapLeft (L.fromStrict . U.fromString) $ parseEither
                extractUsername
                (responseBody usernameRes)

        _ -> do
            liftIO $ print "No token found"
            pure $ Left "No OAuth token found for home"

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
           
data OAuthResponse = OAuthResponse {respAccessToken::String, respRefreshToken :: String, accessTokenExpiresIn :: Pico, refreshTokenExpiresIn :: Pico }
    deriving Show
instance FromJSON OAuthResponse where
    parseJSON = withObject "oauthResponse" $ \o ->
        OAuthResponse
            <$> o .:  "access_token"
            <*> o .:  "refresh_token"
            <*> (read <$> o .: "access_token_expires_in")
            <*> (read <$> o .: "refresh_token_expires_in")


lookupParam :: L.ByteString -> [Param] -> Either L.ByteString Param
lookupParam paramName params =
    justOrErr ("Couldn't find param " <> paramName)
        $ find (paramNamed paramName) params

-- Attempts to construct a Home DTO from a set of parameters originating from a HTTP request
homeFrom :: [Param] -> IO (Either L.ByteString Home)
homeFrom params = do
    currentTime <- getCurrentTime

    pure
        $   Home
        <$> pure Nothing
        <*> pure Nothing
        <*> (C.unpack . snd <$> lookupParam "influxHost" params)
        <*> (read . C.unpack . snd <$> lookupParam "influxPort" params)
        <*> (isCheckboxSet <$> lookupParam "influxTLS" params)
        <*> (C.unpack . snd <$> lookupParam "influxHost" params)
        <*> (C.unpack . snd <$> lookupParam "influxPassword" params)
        <*> pure currentTime
        <*> pure OAuthPending
        <*> pure Nothing
        <*> pure Nothing
        <*> pure Nothing
        <*> pure Nothing
        <*> pure Nothing
        <*> pure Nothing

