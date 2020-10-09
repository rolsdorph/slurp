{-# LANGUAGE MultiParamTypeClasses #-}

module Types where

import           Data.Time.Clock
import           Data.Convertible
import           Database.HDBC.SqlValue
import qualified Data.ByteString.UTF8          as U

type ClientId = U.ByteString
type ClientSecret = U.ByteString
type AppId = String
type DeviceId = String
data AppCreds = AppCreds {
    hueClientId :: ClientId,
    hueClientSecret :: ClientSecret,
    hueAppId :: AppId,
    hueDeviceId :: DeviceId
}

data VerificationState = Verified | UsernamePending | OAuthPending | Unknown
                       deriving Show

instance Convertible VerificationState SqlValue where
    safeConvert Verified = Right $ toSql "Verified"
    safeConvert UsernamePending = Right $ toSql "UsernamePending"
    safeConvert OAuthPending  = Right $ toSql "Pending"
    safeConvert _        = Right $ toSql "Unknown"

fromString :: String -> VerificationState
fromString s | s == "Verified"  = Verified
             | s == "UsernamePending" = UsernamePending 
             | s == "OAuthPending" = OAuthPending 
             | otherwise       = Unknown

data Home = Home { uuid :: Maybe String
                 , influxHost :: String
                 , influxPort :: Int
                 , influxTLS :: Bool
                 , influxUsername :: String
                 , influxPassword :: String
                 , createdAt :: UTCTime
                 , state :: VerificationState
                 , oauthState :: Maybe String
                 , accessToken :: Maybe String
                 , refreshToken :: Maybe String
                 , accessExpiry :: Maybe UTCTime
                 , refreshExpiry :: Maybe UTCTime
                 , hueUsername :: Maybe String }
    deriving Show
