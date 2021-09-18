{-# LANGUAGE OverloadedStrings #-}

import Control.Concurrent (Chan, newChan, readChan, writeChan)
import Control.Concurrent.Async (async, wait)
import Control.Monad (when)
import Control.Monad.Reader (runReaderT)
import Data.Aeson (Value, encode, withObject, (.:))
import Data.Aeson.Types (Parser, parseMaybe)
import Generators
import InfluxPublish (InfluxPushResult (..))
import InfluxPusher
import qualified Network.AMQP as Q
import RabbitMQ (QueueConsumer)
import System.Log.Logger (errorM, infoM, warningM)
import Test.Hspec (Spec, before, describe, hspec, it, shouldBe, shouldSatisfy)
import Test.QuickCheck hiding (Success)
import TestUtil
import Types

loggerName :: String
loggerName = "InfluxPusher tests"

main :: IO ()
main = hspec spec

type InfluxPushData = (SourceData, InfluxDefinition)

baseEnv :: Env
baseEnv =
  Env
    { envGetUserSinks = const (return []),
      envInfluxPush = \_ _ -> return Success,
      envLogInfo = infoM loggerName,
      envLogWarn = warningM loggerName,
      envLogError = errorM loggerName,
      envPublishNotification = const $ return (),
      envConsumerRegistry = const $ return ()
    }

data TestEnv = TestEnv
  { consumers :: Chan QueueConsumer,
    influxData :: Chan InfluxPushData,
    notifications :: Chan MessageToUser,
    env :: Env,
    user :: User,
    sink :: InfluxSink
  }

withCapturingEnv :: IO TestEnv
withCapturingEnv = do
  consumerCapture <- newChan
  influxCapture <- newChan
  notificationCapture <- newChan
  testUser <- getUser <$> generate arbitrary
  testSink <- generate (testSinkFor testUser)

  let capturingEnv =
        baseEnv
          { envInfluxPush = \influxPayload target -> writeChan influxCapture (influxPayload, target) >> return Success,
            envConsumerRegistry = writeChan consumerCapture,
            envPublishNotification = writeChan notificationCapture,
            envGetUserSinks = returnIf testUser testSink
          }

  return
    TestEnv
      { consumers = consumerCapture,
        influxData = influxCapture,
        notifications = notificationCapture,
        user = testUser,
        sink = testSink,
        env = capturingEnv
      }

-- Grabs elements from the given channel until {target} elements have been read,
-- and then returns the accumulated elements
waitForElems :: Chan a -> Int -> IO [a]
waitForElems dataChan target = go dataChan target []
  where
    go chan targetCount cur =
      readChan dataChan >>= \nextElem -> do
        let newCur = nextElem : cur
        if length newCur == target then return newCur else go chan targetCount newCur

spec :: Spec
spec = do
  describe "InfluxPusher" $ do
    describe "When user has sinks" $ do
      before withCapturingEnv $ do
        it "Pushes valid source data to Influx" $ \testEnv -> do
          wConsumers <- async $ waitForElems (consumers testEnv) 1
          wInfluxPushes <- async $ waitForElems (influxData testEnv) 1
          wNotifications <- async $ waitForElems (notifications testEnv) 1

          -- Start the app, wait for it to register a consumer
          _ <- async $ runReaderT app (env testEnv)
          msgConsumer <- head <$> wait wConsumers

          -- Pass a message to the consumer
          testSd <- generate $ testSsFor (user testEnv) >>= testSourceDataForSs
          validMsg <- generate $ validMessage testSd
          msgConsumer validMsg

          -- This should lead to an Influx push
          (sourceData, targetSink) <- head <$> wait wInfluxPushes
          sourceData `shouldBe` testSd
          targetSink `shouldBe` influxDefinition (sink testEnv)

          -- And also to a notification about the push
          notification <- head <$> wait wNotifications
          payload notification `shouldSatisfy` isSinkFedFor (sink testEnv)
          targetUserId notification `shouldBe` userId (user testEnv)

withoutSink :: TestEnv -> IO TestEnv
withoutSink oldEnv = do
  return
    oldEnv
      { env =
          (env oldEnv)
            { envGetUserSinks = const $ return []
            }
      }

validMessage :: SourceData -> Gen Q.Message
validMessage sd = return Q.Message {Q.msgBody = encode sd}

parseSinkFed :: Value -> Parser String
parseSinkFed = withObject "notification" $ \v -> do
  notiType <- v .: "type"
  sinkId <- v .: "sinkId"
  when (notiType /= "SinkFed") (fail ("Wrong type " <> notiType))
  return sinkId

isSinkFedFor :: InfluxSink -> Value -> Bool
isSinkFedFor s val =
  parseMaybe parseSinkFed val == Just (influxUuid s)
