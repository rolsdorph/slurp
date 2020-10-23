{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}

module Collector
  ( collectAndPublish,
  )
where

import Control.Lens
import qualified Data.ByteString.UTF8          as U
import Data.Aeson
import Data.Aeson.Types
import qualified Data.HashMap.Strict as HM
import Data.Int (Int64)
import qualified Data.Map as Map
import Data.Maybe
import Data.String (fromString)
import Data.Text (Text)
import Data.Time
import Database.InfluxDB
import Database.InfluxDB.Format (decimal, key, string)
import Network.HTTP.Req
import Database.InfluxDB (Server)
import System.IO (BufferMode (LineBuffering), hSetBuffering, stdout)
import Network.HTTP.Client (newManager)
import Network.HTTP.Client.TLS (tlsManagerSettings)

type InfluxHost = Text
type Port = Int
type Username = Text
type Password = Text
wp :: InfluxHost -> Port -> Username -> Password -> WriteParams
wp hostName port username password =
  writeParams "home"
    & authentication ?~ credentials username password 
    & server .~ (secureServer & host .~ hostName & Database.InfluxDB.port .~ port)
    & manager .~ Left tlsManagerSettings

type BridgeUsername = String
type BridgeHost = String
type Token = String
collectAndPublish :: InfluxHost -> Port -> Username -> Password -> BridgeHost -> Maybe Token -> BridgeUsername -> IO ()
collectAndPublish host port username password bridgeHost bridgeToken bridgeUsername = do
  hSetBuffering stdout LineBuffering

  lights <- getLights bridgeHost bridgeUsername bridgeToken
  print "Light data collected from bridge"

  let parsedLights = parseLights lights
  let metrics = map toLine parsedLights
  writeBatch (wp host port username password) metrics
  print "Metrics published to Influx"

parseLights :: Value -> [Light]
parseLights value = do
  let parseRes = parseMaybe (withObject "lightsList" (mapM (parseJSON . snd) . HM.toList)) value
  fromMaybe [] parseRes

bearerHeader :: Maybe Token -> Option scheme
bearerHeader (Just token) = (header "Authorization" (U.fromString $ "Bearer " ++ token))
bearerHeader Nothing = mempty

getLights :: String -> String -> Maybe Token -> IO Value
getLights bridgeIp username token = runReq defaultHttpConfig $ do
  res <-
    req
      GET
      (https (fromString bridgeIp) /: ("bridge" :: Text) /: fromString username /: ("lights" :: Text))
      NoReqBody
      lbsResponse
      (bearerHeader token)

  let resData = decode (responseBody res) :: Maybe Value

  case resData of
    Just x -> return x
    Nothing -> error "Failed to parse response from Hue Bridge"

toLine :: Light -> Line UTCTime
toLine light =
  Line @UTCTime
    "light"
    -- Tags:
    (Map.fromList [("name", formatKey string $ name light), ("uuid", formatKey string $ uuid light), ("type", formatKey string $ typeName light)])
    -- Fields:
    ( Map.fromList
        [ ("on", FieldBool (on light)),
          ("brightness", FieldInt (brightness light)),
          ("hue", FieldInt (hue light)),
          ("saturation", FieldInt (saturation light)),
          ("xcolor", FieldFloat (fst . xyColor $ light)),
          ("ycolor", FieldFloat (snd . xyColor $ light)),
          ("ctTemp", FieldInt (ctTemp light)),
          ("reachable", FieldBool (reachable light))
        ]
    )
    Nothing

data Light = Light
  { name :: String,
    uuid :: String,
    typeName :: String,
    on :: Bool,
    brightness :: Int64,
    hue :: Int64,
    saturation :: Int64,
    xyColor :: (Double, Double),
    ctTemp :: Int64,
    reachable :: Bool
  }
  deriving (Show)

instance FromJSON Light where
  parseJSON = withObject "Light" $ \l -> do
    lightState <- l .: "state"
    Light <$> l .: "name"
      <*> l .: "uniqueid"
      <*> l .: "type"
      <*> (lightState .: "on")
      <*> (lightState .: "bri")
      <*> (lightState .: "hue")
      <*> (lightState .: "sat")
      <*> (lightState .: "xy")
      <*> (lightState .: "ct")
      <*> (lightState .: "reachable")
