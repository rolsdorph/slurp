{-# LANGUAGE MultiParamTypeClasses #-}

module Types where

import           Data.Time.Clock
import           Data.Convertible
import           Database.HDBC.SqlValue

data VerificationState = Verified | Pending | Unknown
                       deriving Show

instance Convertible VerificationState SqlValue where
    safeConvert Verified = Right $ toSql "Verified"
    safeConvert Pending  = Right $ toSql "Pending"
    safeConvert _        = Right $ toSql "Unknown"

fromString :: String -> VerificationState
fromString s | s == "Pending"  = Pending
             | s == "Verified" = Verified
             | otherwise       = Unknown

data Home = Home { uuid :: Maybe String
                 , influxHost :: String
                 , influxPort :: Int
                 , influxTLS :: Bool
                 , createdAt :: UTCTime
                 , state :: VerificationState
                 , oauthState :: Maybe String
                 , accessToken :: Maybe String
                 , refreshToken :: Maybe String
                 , accessExpiry :: Maybe UTCTime
                 , refreshExpiry :: Maybe UTCTime
                 , hueUsername :: Maybe String }
    deriving Show
