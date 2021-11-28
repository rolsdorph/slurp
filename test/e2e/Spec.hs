{-# LANGUAGE OverloadedStrings #-}

import qualified Api
import qualified Collector
import qualified InfluxPusher
import qualified Notifier
import Secrets


import DBUtil
import TestUtil
import Types

import Database.HDBC.PostgreSQL (connectPostgreSQL, Connection)

import Test.Hspec
import System.Environment.Blank (setEnv)
import Control.Concurrent.Async (async)
import Network.HTTP.Simple
import Network.HTTP.Types.Status
import Control.Concurrent.STM.TSem (newTSem, waitTSem, signalTSem)
import Control.Concurrent.STM (atomically)
import Data.ByteString (ByteString, isInfixOf)
import Data.ByteString.Lazy (toStrict)
import Data.Text (Text)
import qualified Data.ByteString.UTF8 as U
import Data.UUID.V4 (nextRandom)
import Database.HDBC (disconnect, run, commit)
import Control.Exception (bracket)
import Configuration.Dotenv (loadFile, Config (..))
import qualified Network.WebSockets as WS
import qualified Network.Wai as Wai
import qualified Network.Wai.Handler.Warp as  W
import Data.Aeson (Value, object, encode, Value (Bool, Number), eitherDecode, Object)
import Data.Aeson.Types ((.:), parseMaybe)
import Control.Concurrent (Chan, writeChan, newChan, readChan)
import Control.Monad (replicateM)
import Network.Wai (queryString, strictRequestBody)

main :: IO ()
main = hspec spec

configureEnv :: IO ()
configureEnv = do
    -- Queue configuration
    _ <- loadFile $ Config [".env"] [] True

    -- Third-party client credentials
    setEnv "hueClientId" "" True
    setEnv "hueClientSecret" "" True
    setEnv "hueAppId" "" True
    setEnv "hueDeviceId" "" True
    setEnv "googleClientId" "" True
    setEnv "spotifyClientId"  "" True
    setEnv "spotifyRedirectUri"  "" True

-- * Simple source server
testData :: Value
testData = object [("stringKey", "stringValue"), ("trueKey", Bool True), ("intKey", Number 42)]

testServerPort :: Int
testServerPort =  8077

testServerPort' :: ByteString
testServerPort' = U.fromString $ show testServerPort

serveTestData :: Wai.Application
serveTestData request respond = do
    case (Wai.requestMethod request, Wai.rawPathInfo request) of
        ("GET" , "/example.json") -> respond $ Wai.responseLBS status200 mempty (encode testData)
        _ -> respond $ Wai.responseLBS status404 mempty ""

captureInfluxWrites :: Chan (Wai.Request, ByteString) -> Wai.Application
captureInfluxWrites captureChannel request respond = do
  case (Wai.requestMethod request, Wai.rawPathInfo request) of
    ("POST" , "/write") -> do
      body <- toStrict <$> strictRequestBody request
      writeChan captureChannel (request, body)
      respond $ Wai.responseLBS status200 mempty ""
    _ -> respond $ Wai.responseLBS status404 mempty ""

-- * Fake Influx server
testInfluxPort :: Int
testInfluxPort = 8066

testInfluxPort'  :: ByteString
testInfluxPort' = U.fromString $ show testInfluxPort

testInfluxUsername :: ByteString
testInfluxUsername = "mysecretinflux"

testInfluxPassword :: ByteString
testInfluxPassword = "mysecrethunter2"

testInfluxDbName :: ByteString
testInfluxDbName = "mysecretdb"

testInfluxDbName' :: String
testInfluxDbName' = U.toString testInfluxDbName

withDbAndEnv :: (Connection -> IO ()) -> IO ()
withDbAndEnv test = do
  configureEnv
  schemaName <- show <$> nextRandom
  setEnv "pgSchema" schemaName True
  bracket (createDb schemaName) (deleteDb schemaName) test

createDb :: String -> IO Connection
createDb schemaName = do
  maybeConnInfo <- readPgConnInfo
  case maybeConnInfo of
    Just connInfo -> do
      conn <- connectPostgreSQL connInfo
      _ <- run conn ("CREATE SCHEMA " ++ quote schemaName) []
      commit conn
      _ <- run conn ("SET search_path TO " ++ quote schemaName) []
      return conn
    Nothing -> error "Connection info not found"

deleteDb :: String -> Connection -> IO ()
deleteDb schemaName conn = do
  _ <- run conn ("DROP SCHEMA " ++ quote schemaName ++ " CASCADE") []
  disconnect conn

spec :: Spec
spec = describe "e2e functionality" $ do
  around withDbAndEnv $ do
    it "spins up a server" $ \conn -> do
      -- Serve up some example JSON for the collector to consume
      testDataReady <- atomically $ newTSem 0
      let testServerSettings = W.setBeforeMainLoop (atomically $ signalTSem testDataReady) $
                               W.setPort testServerPort
                               W.defaultSettings
      _  <- async $ W.runSettings testServerSettings serveTestData

      collectorReady <- atomically $ newTSem 0
      pusherReady <- atomically $ newTSem 0
      notifierReady <- atomically $ newTSem 0
      apiReady <- atomically $ newTSem 0

      -- Listen for Influx measurement posts
      influxReady <- atomically $ newTSem 0
      influxChan <- newChan
      let influxSettings = W.setBeforeMainLoop (atomically $ signalTSem influxReady) $
                           W.setPort testInfluxPort
                           W.defaultSettings
      _  <- async $ W.runSettings influxSettings (captureInfluxWrites influxChan)

      _ <- async $ Collector.run collectorReady conn
      _ <- async $ InfluxPusher.run pusherReady conn
      _ <- async $ Notifier.main' notifierReady conn
      _ <- async $ Api.run apiReady conn

      atomically $ do
        waitTSem collectorReady
        waitTSem pusherReady
        waitTSem notifierReady
        waitTSem apiReady
        waitTSem testDataReady
        waitTSem influxReady

      putStrLn "All services ready, running E2E tests"

      -- Create a new user
      req' <- parseRequest "POST http://localhost:8080/insecureAuth"
      let req = setRequestBodyURLEncoded [("auth", "mysupersecretauth")] req'
      resp <- httpBS req

      getResponseStatus resp `shouldBe` status200
      let token = getResponseBody resp
      let withAuth = addAuth token

      -- Look up the logged in user
      req2 <- withAuth <$> parseRequest "GET http://localhost:8080/currentUser"
      userResp  <- httpJSON req2

      getResponseStatus userResp `shouldBe` status200
      let user = getResponseBody userResp :: User
      authType user `shouldBe` Insecure

      -- Register a simple source for the user
      req3' <- withAuth <$> parseRequest "POST http://localhost:8080/simpleSources"
      let req3 = setRequestBodyURLEncoded simpleSourceRequest req3'
      postSourceResp <- httpJSON req3

      getResponseStatus postSourceResp `shouldBe` status201
      let createdSource = getResponseBody postSourceResp :: SimpleShallowJsonSource
      let sourceDef = ssDefinition createdSource
      shallowOwnerId sourceDef `shouldBe` userId user
      authHeader sourceDef `shouldBe` ""  -- Censored in output

      getSourcesReq <- withAuth <$> parseRequest "GET http://localhost:8080/simpleSources"
      sourcesResp <- httpJSON getSourcesReq
      getResponseStatus sourcesResp `shouldBe` status200
      getResponseBody sourcesResp `shouldBe` [createdSource]

      -- Register an Influx sink for the user
      createSink' <- withAuth <$> parseRequest "POST http://localhost:8080/sinks"
      let createSink = setRequestBodyURLEncoded influxSinkRequest createSink'
      postSinkResp <- httpJSON createSink
      getResponseStatus postSinkResp `shouldBe` status201
      let createdSink = getResponseBody postSinkResp :: InfluxSink
      let sinkDef = influxDefinition createdSink
      influxOwnerId sinkDef `shouldBe` userId user
      influxHost sinkDef `shouldBe` "localhost"
      influxPort sinkDef `shouldBe` testInfluxPort
      influxTLS sinkDef `shouldBe` False
      influxUsername sinkDef `shouldBe` ""
      influxPassword sinkDef `shouldBe` ""
      influxDbName sinkDef `shouldBe` testInfluxDbName'

      getSinksReq <- withAuth <$> parseRequest "GET http://localhost:8080/sinks"
      sinksResp <- httpJSON getSinksReq
      getResponseStatus sinksResp `shouldBe` status200
      getResponseBody sinksResp `shouldBe` [createdSink]

      -- Register a WebSocket, wait for a source collection notification and a sink fed notification
      receivedNotifications <- (fmap . fmap) (either error id) $ WS.runClient "127.0.0.1" 8090 "/" (waitForCollection 2 token)
      receivedNotifications `shouldContainPredicate` isSourceCollectionFor (genericSourceId createdSource)
      receivedNotifications `shouldContainPredicate` isSinkFedFor (influxUuid createdSink)

      (writeReq, body) <- readChan influxChan
      queryString writeReq `shouldContain` [("u", Just testInfluxUsername)]
      queryString writeReq `shouldContain` [("p", Just testInfluxPassword)]
      queryString writeReq `shouldContain` [("db", Just testInfluxDbName)]
      isInfixOf "mappedString=stringValue" body `shouldBe` True
      isInfixOf "mappedBool=true" body `shouldBe` True

      putStrLn "done"

isSourceCollectionFor :: String -> Object -> Bool
isSourceCollectionFor sourceId notification = do
  lookupField "sourceId" notification == Just sourceId && lookupField "type" notification == Just "SourceCollected"

isSinkFedFor :: String -> Object -> Bool
isSinkFedFor sinkId notification = do
  lookupField "sinkId" notification == Just sinkId && lookupField "type" notification == Just "SinkFed"

lookupField :: Text -> Object -> Maybe String
lookupField fieldName = parseMaybe (\o -> o .: fieldName)

simpleSourceRequest :: [(ByteString, ByteString)]
simpleSourceRequest = [
                       ("datakey", "e2etestsimplesource"),
                       ("url", "http://localhost:" <> testServerPort' <> "/example.json"),
                       ("authHeader", "mysupersecretauth"),
                       ("tagMappings", toStrict $ encode [("stringKey" :: String,  "mappedString" :: String)]),
                       ("fieldMappings", toStrict $ encode [("trueKey" :: String, "mappedBool" :: String)])
                      ]

influxSinkRequest :: [(ByteString, ByteString)]
influxSinkRequest = [
                     ("influxHost", "localhost"),
                     ("influxPort", testInfluxPort'),
                     ("influxTLS", "false"),
                     ("influxUsername", testInfluxUsername),
                     ("influxPassword", testInfluxPassword),
                     ("influxDbName", testInfluxDbName)
                    ]

addAuth :: ByteString -> Request -> Request
addAuth auth = addRequestHeader "Authorization" ("Bearer " <> auth)

waitForCollection :: Int -> ByteString -> WS.Connection -> IO [Either String Object]
waitForCollection numMessages authToken ws = do
  WS.sendTextData ws authToken
  notifications <- replicateM numMessages (WS.receiveDataMessage ws)
  return $ eitherDecode . WS.fromDataMessage <$> notifications