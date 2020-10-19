{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE MultiParamTypeClasses #-}

module Types where

import           Data.Aeson
import           Data.Time.Clock
import           Data.Convertible
import           Database.HDBC.SqlValue
import qualified Data.ByteString.UTF8          as U
import qualified Data.Text                     as T

type ClientId = U.ByteString
type GoogleClientId = T.Text
type ClientSecret = U.ByteString
type AppId = String
type DeviceId = String
data AppCreds = AppCreds {
    hueClientId :: ClientId,
    hueClientSecret :: ClientSecret,
    hueAppId :: AppId,
    hueDeviceId :: DeviceId,
    googleClientId :: GoogleClientId,
    spotifyClientId :: String,
    spotifyRedirectUri :: String
}

data QueueConfig = QueueConfig {
    hostname :: String,
    vhost :: T.Text,
    username :: T.Text,
    password :: T.Text,
    queueName :: T.Text
}

data VerificationState = Verified | UsernamePending | OAuthPending | Unknown
                       deriving Show

instance Convertible VerificationState SqlValue where
    safeConvert Verified = Right $ toSql ("Verified" :: String)
    safeConvert UsernamePending = Right $ toSql ("UsernamePending" :: String)
    safeConvert OAuthPending  = Right $ toSql ("Pending" :: String)
    safeConvert _        = Right $ toSql ("Unknown" :: String)

instance ToJSON VerificationState where
    toJSON Verified = "Verified"
    toJSON UsernamePending = "UsernamePending"
    toJSON OAuthPending = "OAuthPending"
    toJSON _ = "Unknown"

fromString :: String -> VerificationState
fromString s | s == "Verified"  = Verified
             | s == "UsernamePending" = UsernamePending 
             | s == "OAuthPending" = OAuthPending 
             | otherwise       = Unknown

data AuthType = Google | UnknownAuth
              deriving Show

instance Convertible AuthType SqlValue where
    safeConvert Google = Right $ toSql ("Google" :: String)
    safeConvert _        = Right $ toSql ("Unknown" :: String)

authFromString :: String -> AuthType
authFromString s | s == "Google" = Google
                 | otherwise     = UnknownAuth

data User = User { userId :: String,
                   userCreatedAt :: UTCTime,
                   authType :: AuthType,
                   googleUuid :: Maybe String }
    deriving Show

data Home = Home { uuid :: Maybe String
                 , ownerId :: Maybe String
                 , createdAt :: UTCTime
                 , state :: VerificationState
                 , oauthState :: Maybe String
                 , accessToken :: Maybe String
                 , refreshToken :: Maybe String
                 , accessExpiry :: Maybe UTCTime
                 , refreshExpiry :: Maybe UTCTime
                 , hueUsername :: Maybe String }
    deriving Show

instance ToJSON Home where
    toJSON (Home uuid _ createdAt state _ _ _ _ _ _)
        = object
            [ "id" .= uuid
            , "state" .= state
            , "createdAt" .= createdAt
            ]

data InfluxSink = InfluxSink { influxUuid :: Maybe String
                 , influxOwnerId :: Maybe String
                 , influxHost :: String
                 , influxPort :: Int
                 , influxTLS :: Bool
                 , influxUsername :: String
                 , influxPassword :: String
                 , influxCreatedAt :: UTCTime }
    deriving Show

instance ToJSON InfluxSink where
    toJSON (InfluxSink uuid _ influxHost influxPort influxTLS _ _ createdAt)
        = object
            [ "id" .= uuid
            , "influxHost" .= influxHost
            , "influxPort" .= influxPort
            , "influxTLS" .= influxTLS
            , "createdAt" .= createdAt
            ]

data MessageToUser = MessageToUser {
    targetUserId :: String,
    payload :: Value
}

instance ToJSON MessageToUser where
    toJSON message =
        object ["targetUserId" .= targetUserId message, "payload" .= payload message]

instance FromJSON MessageToUser where
    parseJSON = withObject "Message"
        $ \m -> MessageToUser <$> m .: "targetUserId" <*> m .: "payload"
