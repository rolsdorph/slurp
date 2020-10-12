module Main where

import           HomeDB
import           InfluxDB
import           UserDB
import           Collector
import           Control.Concurrent             ( threadDelay )
import           Data.Foldable
import           Data.List
import           Types
import qualified Data.Text                     as T
import           Text.Printf

hueBridgeApi = "api.meethue.com"

main :: IO ()
main = do
    putStrLn "Fetching users..."
    users <- getAllUsers

    forM_ users publishForUser

    putStrLn "All done, soon looping again!"

    threadDelay 1000
    main

-- Publishes data from all user homes to all user sinks
publishForUser :: User -> IO ()
publishForUser user = do
    putStrLn "Fetching verified user homes..."
    homes <- getUserHomes (userId user)

    putStrLn "Fetching all user sinks..."
    sinks <- getUserInfluxSinks (userId user)

    putStrLn "Collecting metrics..."
    forM_ homes (collectHome sinks)

    putStrLn "Done!"

-- Publishes data from the given home to each of the given sinks
collectHome :: [InfluxSink] -> Home -> IO ()
collectHome sinks home = forM_ sinks (\sink -> collect home sink)

-- Collects stats from a home and publishes them to the given sink
collect :: Home -> InfluxSink -> IO ()
collect home sink = do
    printf "About to publish to %s:%d" (influxHost sink) (influxPort sink)

    let maybeToken    = accessToken home
    let maybeUsername = hueUsername home
    case (maybeToken, maybeUsername) of
        (Just t, Just u) -> collectAndPublish
            (T.pack $ influxHost sink)
            (influxPort sink)
            (T.pack $ influxUsername sink)
            (T.pack $ influxPassword sink)
            hueBridgeApi
            (Just t)
            u
        _ -> print "Token or username missing, can't update home"
