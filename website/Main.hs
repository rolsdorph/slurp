{-# LANGUAGE OverloadedStrings #-}

module Main where

import           Types
import           Lib
import           OAuth
import           Html
import           Util
import           Secrets
import           GoogleLogin
import           UserDB
import           Auth
import           TokenDB
import           InfluxDB

import           HomeDB
import           Network.Wai
import qualified Network.Wai.Handler.Warp      as W
                                                ( run )
import qualified Data.CaseInsensitive          as CI
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
import qualified Data.ByteString.Search        as SS
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

    let lookupAuth = Auth.verifyToken . extractBearerToken <$> getRequestHeader "Authorization" request

    currentUser <- case lookupAuth of
                        (Just u) -> u
                        _ -> pure Nothing

    response <- case (requestMethod request, rawPathInfo request) of
        ("GET" , "/"        )   -> pure index
        ("GET" , "/login"   )   -> pure loginLanding
        ("POST", "/googleAuth") -> googleAuth creds keys (fst reqBodyParsed)
        ("GET", "/sinks"    )   -> getSinks currentUser
        ("POST", "/sinks"   )   -> postSink creds currentUser (fst reqBodyParsed)
        ("GET", "/homes"    )   -> getHomes currentUser
        ("POST", "/homes"   )   -> postHome creds currentUser (queryString request)
        ("GET" , "/callback")   -> oauthCallback creds (queryString request)
        (_     , _          )   -> pure notFound

    respond response

extractBearerToken :: HTTP.Header -> L.ByteString
extractBearerToken header = SS.replace "Bearer " ("" :: B.ByteString) (snd header)

main :: IO ()
main = do
    -- Create necessary tables if they don't exist
    UserDB.setupDb
    HomeDB.setupDb
    TokenDB.setupDb
    InfluxDB.setupDb

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
unauthenticated :: Response
unauthenticated =
    responseLBS HTTP.status401 [("Content-Type", "text/plain")] "Not authenticated >:("

notFound :: Response
notFound =
    responseLBS HTTP.status404 [("Content-Type", "text/plain")] "Not found :("

badRequest :: L.ByteString -> Response
badRequest errMsg = responseLBS HTTP.status400
                                [("Content-Type", "text/plain")]
                                ("Bad request >:( - " <> errMsg)

err500 :: L.ByteString -> Response
err500 = responseLBS HTTP.status500 [("Content-Type", "text/plain")]

success200 :: L.ByteString -> Response
success200 = responseLBS HTTP.status200 [("Content-Type", "text/plain")]

success200Json :: ToJSON a => a -> Response
success200Json body = responseLBS HTTP.status200
                                  [("Content-Type", "application/json")]
                                  (encode body)

success201Json :: ToJSON a => a -> Response
success201Json body = responseLBS HTTP.status201
                                  [("Content-Type", "application/json")]
                                  (encode body)

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
        (Right uid) -> do
           maybeUser <- fetchOrCreateGoogleUser uid
           case maybeUser of
                (Just user) -> loginResponse user
                _ -> pure $ err500 "Internal server error: Could not authenticate user"
        (Left err) -> pure $ err500 err

loginResponse :: User -> IO Response
loginResponse user = do
    token <- login $ userId user
    case token of
         (Right t) -> pure $ success200 ((L.fromStrict . C.pack) t)
         _ -> pure $ err500 "Login error"

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
    (Right claims) <- GoogleLogin.verifyToken keys googleClientId token
    (Right uid   ) <- pure $ extractUserId claims
    pure $ Right uid

buildGoogleClientId :: AppCreds -> Either L.ByteString J.StringOrURI
buildGoogleClientId creds = do
    let res = Lens.preview J.stringOrUri (googleClientId creds)
    case res of
        (Just r) -> pure r
        _        -> Left "Failed to parse client ID"

-- GET /sinks
getSinks :: Maybe User -> IO Response
getSinks Nothing            = pure unauthenticated
getSinks (Just currentUser) = do
    sinks <- getUserInfluxSinks (userId currentUser)
    pure $ success200Json sinks

-- GET /homes
getHomes :: Maybe User -> IO Response
getHomes Nothing            = pure unauthenticated
getHomes (Just currentUser) = do
    homes <- getUserHomes (userId currentUser)
    pure $ success200Json homes

-- POST /sinks
postSink :: AppCreds -> Maybe User -> [Param] -> IO Response
postSink _     Nothing            _      = pure unauthenticated
postSink creds (Just currentUser) params = do
    parsedSink <- influxSinkFrom currentUser params

    case parsedSink of
        (Right sink) -> do
            print "Storing sink..."
            storeSinkRes <- storeInfluxSink sink

            case storeSinkRes of
                Just storedSink -> return $ success201Json storedSink
                _               -> return $ err500 "Couldn't complete operation"

        (Left e) -> return (badRequest $ "Malformed request body: " <> e)

-- POST /homes
postHome :: AppCreds -> Maybe User -> HTTP.Query -> IO Response
postHome _     Nothing            _           = pure unauthenticated
postHome creds (Just currentUser) queryParams = do
    home <- homeFrom currentUser

    let redirectInBodyQParam = getParamValue "redirectUrlInBody" queryParams
    let redirectInBody = case redirectInBodyQParam of
            (Right "true") -> True
            _              -> False

    print "Storing home..."
    storeHomeRes <- storeHome home

    case storeHomeRes of
        Just storedHome -> oauthRedirect creds storedHome redirectInBody
        _               -> return $ err500 "Couldn't complete operation"

oauthRedirect :: AppCreds -> Home -> Bool -> IO Response
oauthRedirect creds home redirectInBody = case oauthState home of
    Just state -> do
        let redirectTarget = buildHueOauthRedirect creds state
        if redirectInBody
            then return $ success200Json redirectTarget
            else return $ redirectResponse redirectTarget
    _ ->
        return
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

getRequestHeader :: U.ByteString -> Request -> Maybe HTTP.Header
getRequestHeader headerName req = find (\h -> fst h == CI.mk headerName) (requestHeaders req)

lookupParam :: L.ByteString -> [Param] -> Either L.ByteString Param
lookupParam paramName params =
    justOrErr ("Couldn't find param " <> paramName)
        $ find (paramNamed paramName) params

-- Constructs a new Home DTO with the given user as the owner
homeFrom :: User -> IO Home
homeFrom currentUser = do
    currentTime <- getCurrentTime

    pure $ Home Nothing
                (Just $ userId currentUser)
                currentTime
                OAuthPending
                Nothing
                Nothing
                Nothing
                Nothing
                Nothing
                Nothing

-- Attempts to construct an Influx sink DTO from a set of parameters originating from a HTTP request
influxSinkFrom :: User -> [Param] -> IO (Either L.ByteString InfluxSink)
influxSinkFrom currentUser params = do
    currentTime <- getCurrentTime

    pure $ InfluxSink
        <$> pure Nothing
        <*> pure (Just $ userId currentUser)
        <*> (C.unpack . snd <$> lookupParam "influxHost" params)
        <*> (read . C.unpack . snd <$> lookupParam "influxPort" params)
        <*> (isCheckboxSet <$> lookupParam "influxTLS" params)
        <*> (C.unpack . snd <$> lookupParam "influxHost" params)
        <*> (C.unpack . snd <$> lookupParam "influxPassword" params)
        <*> pure currentTime
