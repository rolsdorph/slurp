{-# LANGUAGE OverloadedStrings #-}

module Main where

import           HomeDB
import           InfluxDB
import           UserDB
import           Collector
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
import qualified Network.AMQP                  as Q
import           Text.Printf

import           Secrets
import           Types

hueBridgeApi = "api.meethue.com"

main :: IO ()
main = do
    maybeQueueConfig <- readUserNotificationQueueConfig

    case maybeQueueConfig of
        (Just queueConfig) -> do
            notificationVar <- newEmptyMVar

            queueConnection <- Q.openConnection (hostname queueConfig)
                                                (vhost queueConfig)
                                                (username queueConfig)
                                                (password queueConfig)
            queueChannel    <- Q.openChannel queueConnection

            publishJob      <- async $ publishAll notificationVar
            notificationJob <- async $ publishNotifications
                notificationVar
                queueChannel
                (queueName queueConfig)

            wait publishJob
        _ -> putStrLn "User notification queue config missing, not starting"

-- Waits for messages and pushes them onto the given channel
publishNotifications :: MVar MessageToUser -> Q.Channel -> T.Text -> IO ()
publishNotifications messageVar chan routingKey = forever $ do
    msg <- takeMVar messageVar
    Q.publishMsg chan "" routingKey $ Q.newMsg { Q.msgBody = encode msg } -- "" = default exchange

-- Publishes data from all sources to all sinks, for all users
publishAll :: MVar MessageToUser -> IO ()
publishAll notificationVar = forever $ do
    putStrLn "Fetching users..."
    users <- getAllUsers

    forM_ users $ publishForUser notificationVar

    putStrLn "All done, soon looping again!"

    threadDelay (1000 * 1000 * 10) -- 10s

type UserNotifyer = Home -> InfluxSink -> IO ()

-- Publishes data from all user homes to all user sinks
publishForUser :: MVar MessageToUser -> User -> IO ()
publishForUser notificationVar user = do
    putStrLn "Fetching verified user homes..."
    homes <- getUserHomes (userId user)

    putStrLn "Fetching all user sinks..."
    sinks <- getUserInfluxSinks (userId user)

    let userNotifyer = notify notificationVar user

    putStrLn "Collecting metrics..."
    forM_ homes (collectHome userNotifyer sinks)

    putStrLn "Done!"

notify :: MVar MessageToUser -> User -> Home -> InfluxSink -> IO ()
notify notificationVar user home sink = do
    payload <- buildNotificationPayload home sink
    putMVar notificationVar $ MessageToUser (userId user) payload

buildNotificationPayload :: Home -> InfluxSink -> IO Value
buildNotificationPayload home sink = do
    curTime <- getCurrentTime
    pure $ object
        [ "time" .= curTime
        , "homeId" .= uuid home
        , "influxId" .= influxUuid sink
        ]

-- Publishes data from the given home to each of the given sinks
collectHome :: UserNotifyer -> [InfluxSink] -> Home -> IO ()
collectHome notifyUser sinks home = forM_ sinks $ \sink -> do
    collect home sink
    notifyUser home sink

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
