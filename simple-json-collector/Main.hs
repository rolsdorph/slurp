{-# LANGUAGE OverloadedStrings #-}

module Main where

import           Data.Time
import           GHC.IO.Handle.FD
import           System.Log.Logger
import           System.Log.Handler.Simple

import           GenericJson
import           Types

loggerName = "SimpleJsonCollector"

main :: IO ()
main = do
    updateGlobalLogger rootLoggerName removeHandler
    updateGlobalLogger rootLoggerName $ setLevel DEBUG
    stdOutHandler <- verboseStreamHandler stdout DEBUG
    updateGlobalLogger rootLoggerName $ addHandler stdOutHandler

    currentTime <- getCurrentTime

    let
        testSource = SimpleShallowJsonSource
            { genericSourceId = Just "asd"
            , genericDataKey = "some-measurement"
            , shallowCreatedAt = currentTime
            , shallowOwnerId = "bcg"
            , url = "https://static.rolsdorph.io/simpletest.json"
            , authHeader = "Asd 123"
            , tagMappings = [("property1", "tag1"), ("aBoolean", "booleanTag")]
            , fieldMappings = [("aDouble", "dubField"), ("aFalse", "boolField")]
            }

    sd <- collect (errorM loggerName) testSource
    print sd
