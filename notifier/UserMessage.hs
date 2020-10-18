{-# LANGUAGE OverloadedStrings #-}

module UserMessage where

import           Types
import           Data.Aeson

data Message = Message {
    targetUserId :: String,
    payload :: Value
}

instance ToJSON Message where
    toJSON message =
        object ["userId" .= targetUserId message, "message" .= payload message]

instance FromJSON Message where
    parseJSON = withObject "Message"
        $ \m -> Message <$> m .: "targetUserId" <*> m .: "payload"
