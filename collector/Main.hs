{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE FlexibleContexts #-}

module Main where

import           Control.Monad
import           Control.Concurrent             ( threadDelay )
import           Control.Concurrent.Async
import           Control.Concurrent.MVar
import           Data.Foldable
import           Data.Aeson
import           Data.Aeson.Types
import           Data.List
import qualified Data.Text                     as T
import           Data.Time.Clock
import qualified Data.ByteString.Lazy          as LB
import qualified Network.AMQP                  as Q
import           Text.Printf
import           GHC.IO.Handle.FD
import           System.Log.Logger
import           System.Log.Handler.Simple

import qualified SimpleSourceDB
import qualified HomeDB
import qualified UserDB
import           HueHome
import           Secrets
import           Types
import           UserNotification
import qualified SimpleSource as SS
import Control.Monad.Except (runExceptT, MonadIO, ExceptT)
import Network.HTTP.Req (runReq, defaultHttpConfig, req, header, GET (..), NoReqBody (..), lbsResponse, responseBody, HttpException)
import Control.Exception (catch)
import Control.Monad.Reader (ReaderT, asks, liftIO, runReaderT, MonadReader, ask)

import Collector

loggerName = "Collector"

newtype CollectorStack a = CollectorStack { runCollector :: IO a }
  deriving (Functor, Applicative, Monad)

instance HasHttp CollectorStack where
  simpleGet url options = CollectorStack $ do
    catch
      ( runReq defaultHttpConfig $ do
          res <-
            req
              GET
              url
              NoReqBody
              lbsResponse
              options
          return . Right $ responseBody res
      )
      (\ex -> return . Left $ "Failed to collect simple source: " ++ show (ex :: HttpException))

instance HasLogger CollectorStack where
  infoLog  = CollectorStack . infoM loggerName
  errorLog = CollectorStack . errorM loggerName

main :: IO ()
main = do
  updateGlobalLogger rootLoggerName removeHandler
  updateGlobalLogger rootLoggerName $ setLevel DEBUG
  stdOutHandler <- verboseStreamHandler stdout DEBUG
  updateGlobalLogger rootLoggerName $ addHandler stdOutHandler

  maybeConfig <- readUserNotificationQueueConfig
  case maybeConfig of
    (Just config) -> do
      queueConnection <- liftIO $ Q.openConnection (hostname config) (vhost config) (username config) (password config)
      queueChannel    <- liftIO $ Q.openChannel queueConnection

      _ <- Q.declareQueue queueChannel $ Q.newQueue { Q.queueName = notiQueueName config }
      _ <- Q.declareQueue queueChannel $ Q.newQueue { Q.queueName = dataQueueName config }

      let env = Env {
          envGetAllUsers = UserDB.getAllUsers
        , envGetUserSs = SimpleSourceDB.getUserSimpleSources
        , envGetUserHomes = HomeDB.getUserHomes
        , envCollectHome = runCollector . HueHome.collect
        , envCollectSs = runCollector . SS.collect
        , envLogInfo = infoM loggerName
        , envLogError = errorM loggerName
        , envPublishNotification = rmqPushFunction queueChannel (notiQueueName config)
        , envPublishData = rmqPushFunction queueChannel (dataQueueName config)
      }

      runReaderT app env
    Nothing -> emergencyM loggerName "User notification queue missing, not starting"
