{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE MultiParamTypeClasses #-}

module Types
    ( ClientId
    , ClientSecret
    , GoogleClientId
    , AppId
    , DeviceId
    , AppCreds(..)
    , QueueConfig(..)
    , VerificationState(..)
    , fromString
    , AuthType(..)
    , authFromString
    , User(..)
    , SourceData(..)
    , DataPoint(..)
    , DataPointValue(..)
    , Home(..)
    , InfluxSink(..)
    , MessageToUser(..)
    )
where

import           Data.Aeson
import           Data.Aeson.Types
import           Data.Time.Clock
import           Data.Convertible
import           Database.HDBC.SqlValue
import qualified Data.ByteString.UTF8          as U
import qualified Data.Text                     as T
import           Data.Scientific

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
    notiQueueName :: T.Text,
    dataQueueName :: T.Text
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


data SourceData = SourceData {
    sourceId :: String,
    datapoints :: [DataPoint]
} deriving Show

instance ToJSON SourceData where
    toJSON s = object ["sourceId" .= sourceId s, "datapoints" .= datapoints s]

instance FromJSON SourceData where
    parseJSON = withObject "SourceData"
        $ \d -> SourceData <$> d .: "sourceId" <*> d .: "datapoints"

data DataPoint = DataPoint {
    tags :: [(String, DataPointValue)],
    fields :: [(String, DataPointValue)]
} deriving Show

instance ToJSON DataPoint where
    toJSON dp =
        object ["tags" .= tags dp, "fields" .= fields dp]

instance FromJSON DataPoint where
    parseJSON = withObject "DataPoint"
        $ \d -> DataPoint <$> d .: "tags" <*> d .: "fields"

data DataPointValue = IntValue Int
                    | DoubleValue Double
                    | StringValue String
                    | BoolValue Bool
    deriving Show

instance ToJSON DataPointValue where
    toJSON (IntValue val) = toJSON val
    toJSON (DoubleValue val) = toJSON val
    toJSON (StringValue val) = toJSON val
    toJSON (BoolValue val) = toJSON val

instance FromJSON DataPointValue where
    parseJSON (String s  ) = pure $ StringValue (T.unpack s)
    parseJSON (Number val) = if isInteger val
        then parseIntNumber val
        else pure (DoubleValue $ toRealFloat val)
    parseJSON (Bool b) = pure $ BoolValue b
    parseJSON _        = fail "Unexpected JSON data type"

parseIntNumber :: Scientific -> Parser DataPointValue
parseIntNumber val = case parseRes of
                          Just v -> pure $ IntValue v
                          Nothing -> fail "Failed to parse int"
    where parseRes = toBoundedInteger val

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
