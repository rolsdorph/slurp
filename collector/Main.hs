module Main where

import HomeDB
import Collector
import Control.Concurrent (threadDelay)
import Data.Foldable
import Types
import qualified Data.Text as T
import Text.Printf

hueBridgeApi = "api.meethue.com"

main :: IO ()
main = do
    putStrLn "Fetching verified homes..."
    homes <- getVerifiedHomes

    putStrLn "Collecting metrics..."

    forM_ homes collect

    putStrLn "All done, soon looping again!"

    threadDelay 1000
    main

-- Collects stats from a home and publishes them to the correct Influx
collect :: Home -> IO ()
collect home = do
    printf "About to publish to %s:%d" (influxHost home) (influxPort home)

    let maybeToken = accessToken home
    let maybeUsername = hueUsername home
    case (maybeToken, maybeUsername) of
         (Just t, Just u) -> collectAndPublish (T.pack $ influxHost home) (influxPort home) (T.pack $ influxUsername home) (T.pack $ influxPassword home) hueBridgeApi (Just t) u
         _ -> print "Token or username missing, can't update home"
