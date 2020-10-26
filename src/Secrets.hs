module Secrets where

import           Types
import qualified Data.ByteString.UTF8          as U
import qualified Data.Text                     as T
import           System.Environment


-- Attempts to read OAuth variables from the environment
readCreds :: IO (Maybe AppCreds)
readCreds = do
    -- Hue secrets
    hueClientId <- lookupEnv "hueClientId"
    hueClientSecret   <- lookupEnv "hueClientSecret"
    hueAppId    <- lookupEnv "hueAppId"
    hueDeviceId <- lookupEnv "hueDeviceId"

    -- Login with Google secrets
    googleClientId <- lookupEnv "googleClientId"

    -- Spotify secrets
    spotifyClientId <- lookupEnv "spotifyClientId"
    spotifyRedirectUri <- lookupEnv "spotifyRedirectUri"

    pure
        $   AppCreds
        <$> (U.fromString <$> hueClientId)
        <*> (U.fromString <$> hueClientSecret)
        <*> hueAppId
        <*> hueDeviceId
        <*> (T.pack <$> googleClientId)
        <*> spotifyClientId
        <*> spotifyRedirectUri

readUserNotificationQueueConfig :: IO (Maybe QueueConfig)
readUserNotificationQueueConfig = do
    hostname  <- lookupEnv "rmqHost"
    vhost     <- lookupEnv "rmqVhost"
    username  <- lookupEnv "rmqUsername"
    password  <- lookupEnv "rmqPassword"
    notiQueueName <- lookupEnv "userNotificationQueueName"
    dataQueueName <- lookupEnv "dataQueueName"

    pure
        $   QueueConfig
        <$> hostname
        <*> (T.pack <$> vhost)
        <*> (T.pack <$> username)
        <*> (T.pack <$> password)
        <*> (T.pack <$> notiQueueName)
        <*> (T.pack <$> dataQueueName)
