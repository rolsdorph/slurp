module Secrets where

import           Types
import qualified Data.ByteString.UTF8          as U
import           System.Environment


-- Attempts to read OAuth variables from the environment
readCreds :: IO (Maybe AppCreds)
readCreds = do
    hueClientId <- lookupEnv "hueClientId"
    hueClientSecret   <- lookupEnv "hueClientSecret"
    hueAppId    <- lookupEnv "hueAppId"
    hueDeviceId <- lookupEnv "hueDeviceId"
    pure
        $   AppCreds
        <$> (U.fromString <$> hueClientId)
        <*> (U.fromString <$> hueClientSecret)
        <*> hueAppId
        <*> hueDeviceId

