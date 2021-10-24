{-# LANGUAGE OverloadedStrings #-}

module CollectorSpec where

import Collector
import Control.Concurrent (Chan, newChan, writeChan)
import Control.Concurrent.Async (async, wait)
import Control.Monad (when)
import Control.Monad.Reader (runReaderT)
import Data.Aeson (Value, withObject, (.:))
import Data.Aeson.Types (Parser, parseMaybe)
import Generators
import System.Log.Logger (errorM, infoM)
import Test.Hspec (Spec, before, describe, hspec, it, shouldBe, shouldSatisfy)
import Test.QuickCheck
import TestUtil
import Types hiding (fromString)
import Control.Concurrent.STM (atomically)
import Control.Concurrent.STM.TSem (newTSem, signalTSem, TSem, waitTSem)

loggerName :: String
loggerName = "Collector tests"

main :: IO ()
main = hspec spec

baseEnv :: Env
baseEnv =
  Env
    { envGetAllUsers = return [],
      envGetUserSs = const $ return [],
      envGetUserHomes = const $ return [],
      envCollectHome = const . return $ Left "Not implemented",
      envCollectSs = const . return $ Left "Not implemented",
      envLogInfo = infoM loggerName,
      envLogError = errorM loggerName,
      envPublishNotification = const $ return (),
      envPublishData = const $ return (),
      envSignalReady = return ()
    }

data TestEnv = TestEnv
  { notifications :: Chan MessageToUser,
    dataPushes :: Chan SourceData,
    env :: Env,
    user :: User,
    homeData :: SourceData,
    simpleSourceData :: SourceData,
    readySignal :: TSem
  }

withCapturingEnv :: IO TestEnv
withCapturingEnv = do
  notiChan <- newChan
  dataChan <- newChan

  testUser <- getUser <$> generate arbitrary
  testHome <- generate $ testHomeFor testUser
  testSimpleSource <- generate $ testSsFor testUser
  homeData' <- generate $ testSourceDataFor testHome
  simpleSourceData' <- generate $ testSourceDataForSs testSimpleSource
  readySignal' <- atomically $ newTSem 0

  let capturingEnv =
        baseEnv
          { envGetAllUsers = return [testUser],
            envGetUserSs = returnIf testUser testSimpleSource,
            envGetUserHomes = returnIf testUser testHome,
            envCollectHome = rightIf testHome homeData',
            envCollectSs = rightIf testSimpleSource simpleSourceData',
            envPublishNotification = writeChan notiChan,
            envPublishData = writeChan dataChan,
            envSignalReady = atomically $ signalTSem readySignal'
          }

  return
    TestEnv
      { notifications = notiChan,
        dataPushes = dataChan,
        env = capturingEnv,
        user = testUser,
        homeData = homeData',
        simpleSourceData = simpleSourceData',
        readySignal = readySignal'
      }

spec :: Spec
spec = do
  before withCapturingEnv $
    describe "Collector" $ do
      it "Collects simple sources and homes" $ \testEnv -> do
        wNotifications <- async $ waitForElems (notifications testEnv) 2
        wDataPushes <- async $ waitForElems (dataPushes testEnv) 2

        _ <- async $ runReaderT app (env testEnv)

        atomically $ waitTSem (readySignal testEnv)

        -- Two notifications: one for the home collection, one for the simple source collection
        sentNotifications <- wait wNotifications
        length sentNotifications `shouldBe` 2
        sentNotifications `shouldSatisfy` all (\n -> targetUserId n == userId (user testEnv))
        (payload <$> sentNotifications)
          `shouldSatisfy` containsAll [isSourceCollectionFor (homeData testEnv), isSourceCollectionFor (simpleSourceData testEnv)]

        -- And two data collections:
        pushedData <- wait wDataPushes
        length pushedData `shouldBe` 2
        pushedData `shouldSatisfy` containsAll [(== homeData testEnv), (== simpleSourceData testEnv)]

containsAll :: [a -> Bool] -> [a] -> Bool
containsAll predicates values = all (\p -> any p values) predicates

parseSourceCollection :: Value -> Parser String
parseSourceCollection = withObject "notification" $ \v -> do
  notiType <- v .: "type"
  when (notiType /= "SourceCollected") (fail ("Wrong type " <> notiType))
  v .: "sourceId"

isSourceCollectionFor :: SourceData -> Value -> Bool
isSourceCollectionFor sd val =
  parseMaybe parseSourceCollection val == Just (sourceId sd)
