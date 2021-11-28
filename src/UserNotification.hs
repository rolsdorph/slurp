{-# LANGUAGE OverloadedStrings #-}
module UserNotification where

import           Control.Concurrent.MVar
import           Control.Monad
import           Data.Aeson
import qualified Data.Text                     as T
import qualified Network.AMQP                  as Q

import           Types

-- Puts a message with a given payload/user on the MVar
notify :: MVar MessageToUser -> String -> Value -> IO ()
notify notificationVar userId payload =
    putMVar notificationVar $ MessageToUser userId payload

-- Waits for messages and pushes them using the given function
publishNotifications :: IO a -> (a -> IO ()) -> IO ()
publishNotifications messageReceiver pushFunction = forever $ messageReceiver >>= pushFunction

-- Publishes the given message onto the given RMQ channel
rmqPushFunction :: (ToJSON a) => Q.Channel -> T.Text -> (a -> IO ())
rmqPushFunction chan routingKey msg = void (Q.publishMsg chan "" routingKey $ Q.newMsg { Q.msgBody = encode msg }) -- "" = default exchange
