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
    , UserId
    , SourceData(..)
    , DataPoint(..)
    , DataPointValue(..)
    , Home(..)
    , InfluxSink(..)
    , InfluxDefinition(..)
    , MessageToUser(..)
    , JsonMapping
    , MappedValue
    , SimpleSourceDefinition (..)
    , SimpleShallowJsonSource (..)
    , HasHttp (..)
    , HasLogger (..)
    )
where

import           Data.Aeson
import           Data.Aeson.Types
import           Data.Time.Clock
import           Data.Convertible
import           Database.HDBC.SqlValue
import qualified Data.ByteString.UTF8          as U
import qualified Data.ByteString               as B
import qualified Data.ByteString.Lazy          as LB
import qualified Data.Text                     as T
import           Data.Scientific
import Network.HTTP.Req (Url, Option)

class (Monad m) => HasHttp m where
  simpleGet :: Url scheme -> Option scheme -> m (Either String LB.ByteString)

class Monad m => HasLogger m where
  infoLog :: String -> m ()
  errorLog :: String -> m ()

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
                       deriving (Eq, Show)

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

data AuthType = Google | Insecure | UnknownAuth
              deriving (Eq, Show)

instance ToJSON AuthType where
    toJSON Google = "Google"
    toJSON Insecure = "Insecure"
    toJSON _ = "Unknown"

instance FromJSON AuthType where
  parseJSON =
    withText "AuthType" (return . authFromString . T.unpack)

instance Convertible AuthType SqlValue where
    safeConvert Google = Right $ toSql ("Google" :: String)
    safeConvert Insecure = Right $ toSql ("Insecure" :: String)
    safeConvert _        = Right $ toSql ("Unknown" :: String)

authFromString :: String -> AuthType
authFromString s | s == "Google" = Google
                 | s == "Insecure" = Insecure
                 | otherwise     = UnknownAuth

type UserId = String

data User = User { userId :: UserId,
                   userCreatedAt :: UTCTime,
                   authType :: AuthType,
                   thirdPartyId :: Maybe String }
    deriving Show

instance ToJSON User where
  toJSON u =
    object
      [ "userId" .= userId u,
        "userCreatedAt" .= userCreatedAt u,
        "authType" .= authType u
      ]
data SourceData = SourceData {
    sourceId :: String,
    sourceOwnerId :: String,
    datakey :: String,
    datapoints :: [DataPoint]
} deriving (Show, Eq)

instance FromJSON User where
  parseJSON =
    withObject "User"
      $ \u ->
       User
        <$> u .: "userId"
        <*> u .: "userCreatedAt"
        <*> u .: "authType"
        <*> (pure Nothing)

instance ToJSON SourceData where
    toJSON s = object
        [ "sourceId" .= sourceId s
        , "ownerId" .= sourceOwnerId s
        , "datakey" .= datakey s
        , "datapoints" .= datapoints s
        ]

instance FromJSON SourceData where
    parseJSON =
        withObject "SourceData"
            $ \d ->
                  SourceData
                      <$> d .:  "sourceId"
                      <*> d .:  "ownerId"
                      <*> d .:  "datakey"
                      <*> d .:  "datapoints"

data DataPoint = DataPoint {
    tags :: [(String, DataPointValue)],
    fields :: [(String, DataPointValue)]
} deriving Show

instance Eq DataPoint where
  dp1 == dp2 = tags dp1 == tags dp2 && fields dp1 == fields dp2

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
    deriving (Show, Eq)

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

data Home =
          PreCreationHome { homeDataKey :: String
                        , ownerId :: String
                        , createdAt :: UTCTime
                        , state :: VerificationState }
          | Home { uuid :: String
                 , homeDataKey :: String
                 , ownerId :: String
                 , createdAt :: UTCTime
                 , state :: VerificationState
                 , oauthState :: Maybe String
                 , accessToken :: Maybe String
                 , refreshToken :: Maybe String
                 , accessExpiry :: Maybe UTCTime
                 , refreshExpiry :: Maybe UTCTime
                 , hueUsername :: Maybe String }
    deriving (Eq, Show)

instance ToJSON Home where
    toJSON (Home uuid homeDataKey _ createdAt state _ _ _ _ _ _)
        = object
            [ "id" .= uuid
            , "datakey" .= homeDataKey
            , "state" .= state
            , "createdAt" .= createdAt
            ]

data InfluxSink = InfluxSink
  { influxUuid :: String,
    influxDefinition :: InfluxDefinition
  } deriving (Show, Eq)

data InfluxDefinition = InfluxDefinition
  { influxOwnerId :: String,
    influxHost :: String,
    influxPort :: Int,
    influxTLS :: Bool,
    influxUsername :: String,
    influxPassword :: String,
    influxCreatedAt :: UTCTime
  } deriving (Show, Eq)

instance ToJSON InfluxSink where
    toJSON (InfluxSink uuid (InfluxDefinition _ influxHost influxPort influxTLS _ _ createdAt))
        = object
            [ "id" .= uuid
            , "influxHost" .= influxHost
            , "influxPort" .= influxPort
            , "influxTLS" .= influxTLS
            , "createdAt" .= createdAt
            ]

data MessageToUser = MessageToUser {
    targetUserId :: UserId,
    payload :: Value
} deriving Show

instance ToJSON MessageToUser where
    toJSON message =
        object ["targetUserId" .= targetUserId message, "payload" .= payload message]

instance FromJSON MessageToUser where
    parseJSON = withObject "Message"
        $ \m -> MessageToUser <$> m .: "targetUserId" <*> m .: "payload"

type JsonPath = T.Text
type TagOrFieldName = String
type JsonMapping = (JsonPath, TagOrFieldName)
type MappedValue = (TagOrFieldName, DataPointValue)

data SimpleShallowJsonSource = SimpleShallowJsonSource {
    genericSourceId :: String,
    ssDefinition :: SimpleSourceDefinition
} deriving (Eq, Show)

data SimpleSourceDefinition = SimpleSourceDefinition {
    genericDataKey :: String,
    shallowOwnerId :: String, -- TODO: Figure out how to properly deal with these conflicts.. are records really the way to go?
    shallowCreatedAt :: UTCTime,
    url :: T.Text, -- URL to fetch JSON file from
    authHeader :: B.ByteString, -- Authorization header value to use for fetching
    tagMappings :: [JsonMapping], -- Json path => Tag mappings
    fieldMappings :: [JsonMapping] -- Json path => Field mappings
} deriving (Eq, Show)

instance ToJSON SimpleShallowJsonSource where
    toJSON (SimpleShallowJsonSource uuid (SimpleSourceDefinition datakey ownerId createdAt url _ tagMappings fieldMappings)) =
        object
            [ "id" .= uuid
            , "ownerId" .= ownerId
            , "datakey" .= datakey
            , "createdAt" .= createdAt
            , "url" .= url
            , "tagMappings" .= tagMappings
            , "fieldMappings" .= fieldMappings
            ]

instance FromJSON SimpleShallowJsonSource where
  parseJSON = withObject "SimpleShallowJSONSource" $
    \s -> SimpleShallowJsonSource <$> s .:  "id"
            <*> (
              SimpleSourceDefinition <$> s .: "datakey"
                <*> s .: "ownerId"
                <*> s .: "createdAt"
                <*> s .: "url"
                <*> return "" -- No reason to serialize the auth header at the moment
                <*> s .: "tagMappings"
                <*> s .: "fieldMappings"
               )
