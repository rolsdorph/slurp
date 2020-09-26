module Types where

import           Data.Time.Clock

data VerificationState = Verified | Pending | Unknown
                       deriving Show

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
                 , refreshExpiry :: Maybe UTCTime }
    deriving Show
