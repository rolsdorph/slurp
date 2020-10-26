{-# LANGUAGE OverloadedStrings #-}
module UserNotification where

import           Control.Concurrent.MVar
import           Control.Monad
import           Data.Aeson
import qualified Data.Text                     as T
import qualified Network.AMQP                  as Q

import           Types

type UserNotifier = Value -> IO ()

-- Puts a message with a given payload/user on the MVar
notify :: MVar MessageToUser -> String -> Value -> IO ()
notify notificationVar userId payload =
    putMVar notificationVar $ MessageToUser userId payload

-- Waits for messages and pushes them onto the given channel
publishNotifications :: ToJSON a => MVar a -> Q.Channel -> T.Text -> IO ()
publishNotifications messageVar chan routingKey = forever $ do
    msg <- takeMVar messageVar
    Q.publishMsg chan "" routingKey $ Q.newMsg { Q.msgBody = encode msg } -- "" = default exchange

