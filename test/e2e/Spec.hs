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
import Control.Concurrent.STM.TSem (newTSem, waitTSem)
import Control.Concurrent.STM (atomically)
import Data.UUID.V4 (nextRandom)
import Database.HDBC (disconnect)
import Control.Exception (bracket)

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

      -- Create a new user
      req' <- parseRequest "POST http://localhost:8080/insecureAuth"
      let req = setRequestBodyURLEncoded [("auth", "mysupersecretauth")] req'
      resp <- httpBS req

      getResponseStatus resp `shouldBe` status200
      let token = getResponseBody resp

      -- Lookup the logged in user
      req2' <- parseRequest "GET http://localhost:8080/currentUser"
      let req2 = addRequestHeader "Authorization" ("Bearer " <> token) req2'
      userResp  <- httpJSON req2

      getResponseStatus userResp `shouldBe` status200
      authType (getResponseBody userResp) `shouldBe` Insecure

      print (getResponseBody userResp :: User)
