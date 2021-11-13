{-# LANGUAGE OverloadedStrings #-}

import qualified Api
import qualified Collector
import qualified InfluxPusher
import qualified Notifier


import Types

import           Database.HDBC.Sqlite3 (connectSqlite3, Connection)

import Test.Hspec
import System.Environment.Blank (setEnv)
import System.Directory (removeFile)
import Control.Concurrent.Async (async)
import Network.HTTP.Simple
import Network.HTTP.Types.Status
import Control.Concurrent.STM.TSem (newTSem, waitTSem, signalTSem)
import Control.Concurrent.STM (atomically)
import Data.ByteString (ByteString)
import Data.Text (Text)
import qualified Data.ByteString.UTF8 as U
import Data.UUID.V4 (nextRandom)
import Database.HDBC (disconnect)
import Control.Exception (bracket)
import qualified Network.WebSockets as WS
import qualified Network.Wai as Wai
import qualified Network.Wai.Handler.Warp as  W
import Data.Aeson (Value, object, encode, Value (Bool, Number), eitherDecode, Object)
import Data.Aeson.Types (parseEither, (.:))

main :: IO ()
main = hspec spec

configureEnv :: IO ()
configureEnv = do
    -- Queue configuration
    -- TODO: Read from .env
    setEnv "rmqHost" "127.0.0.1" True
    setEnv "rmqVhost" "/" True
    setEnv "rmqUsername" "" True
    setEnv "rmqPassword" "" True
    setEnv "userNotificationQueueName" "testUserNotQueue" True
    setEnv "dataQueueName" "testDataQueue" True

    -- Third-party client credentials
    setEnv "hueClientId" "" True
    setEnv "hueClientSecret" "" True
    setEnv "hueAppId" "" True
    setEnv "hueDeviceId" "" True
    setEnv "googleClientId" "" True
    setEnv "spotifyClientId"  "" True
    setEnv "spotifyRedirectUri"  "" True

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

withDb :: (Connection -> IO ()) -> IO ()
withDb test = do
  dbName <- show <$> nextRandom
  bracket (createDb dbName) (deleteDb dbName) test

createDb :: FilePath -> IO Connection
createDb = connectSqlite3

deleteDb :: FilePath -> Connection -> IO ()
deleteDb fp conn = do
  disconnect conn
  removeFile fp

spec :: Spec
spec = describe "e2e functionality" $ do
  around withDb $ do
    it "spins up a server" $ \conn -> do
      configureEnv

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

      _ <- async $ Collector.run collectorReady conn
      _ <- async $ InfluxPusher.run pusherReady conn
      _ <- async $ Notifier.main' notifierReady conn
      _ <- async $ Api.run apiReady conn

      atomically $ do
        waitTSem collectorReady
        waitTSem pusherReady
        waitTSem notifierReady
        waitTSem apiReady

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
      postSinkResp <- httpJSON req3

      getResponseStatus postSinkResp `shouldBe` status201
      let createdSource = getResponseBody postSinkResp :: SimpleShallowJsonSource
      let sourceDef = ssDefinition createdSource
      shallowOwnerId sourceDef `shouldBe` userId user
      authHeader sourceDef `shouldBe` ""  -- Censored in output

      getSourcesReq <- withAuth <$> parseRequest "GET http://localhost:8080/simpleSources"
      sourcesResp <- httpJSON getSourcesReq
      getResponseStatus sourcesResp `shouldBe` status200
      getResponseBody sourcesResp `shouldBe` [createdSource]

      -- Register a WebSocket, wait for a source collection notification
      receivedNotification <- either error id <$> WS.runClient "127.0.0.1" 8090 "/" (waitForCollection token)
      fieldOrFail "sourceId" receivedNotification `shouldBe` genericSourceId createdSource
      fieldOrFail "type" receivedNotification `shouldBe` "SourceCollected"

fieldOrFail :: Text -> Object -> String
fieldOrFail fieldName obj = either error id $ parseEither (\o -> o .: fieldName) obj

simpleSourceRequest :: [(ByteString, ByteString)]
simpleSourceRequest = [
                       ("datakey", "e2etestsimplesource"),
                       ("url", "http://localhost:" <> testServerPort' <> "/example.json"),
                       ("authHeader", "mysupersecretauth"),
                       ("tagMappings", "[]"),
                       ("fieldMappings", "[]")
                      ]

addAuth :: ByteString -> Request -> Request
addAuth auth = addRequestHeader "Authorization" ("Bearer " <> auth)

waitForCollection :: ByteString -> WS.Connection -> IO (Either String Object)
waitForCollection authToken ws = do
  WS.sendTextData ws authToken
  notification <- WS.receiveDataMessage ws
  return . eitherDecode $ WS.fromDataMessage notification