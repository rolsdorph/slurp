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
import qualified Data.String as S
import Data.Text (Text, pack)
import Data.Time
import Database.InfluxDB
import Database.InfluxDB.Format (decimal, key, string)
import Network.HTTP.Req
import Database.InfluxDB (Server)
import System.IO (BufferMode (LineBuffering), hSetBuffering, stdout)
import Network.HTTP.Client (newManager)
import Network.HTTP.Client.TLS (tlsManagerSettings)

import Types as T

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

  let parsedLights = parseLights lights
  let metrics = map (toLine "light") parsedLights
  writeBatch (wp host port username password) metrics

-- Keys are used as keys AND values for tags, and as keys for fields
toInfluxKey :: T.DataPointValue -> Key
toInfluxKey (T.IntValue    val) = formatKey string $ show val
toInfluxKey (T.DoubleValue val) = formatKey string $ show val
toInfluxKey (T.StringValue val) = formatKey string val
toInfluxKey (T.BoolValue   val) = formatKey string $ show val

-- LineFields are used as field values
toInfluxVal :: T.DataPointValue -> LineField
toInfluxVal (T.IntValue    val) = FieldInt $ fromIntegral val
toInfluxVal (T.DoubleValue val) = FieldFloat val
toInfluxVal (T.StringValue val) = FieldString $ pack val
toInfluxVal (T.BoolValue   val) = FieldBool val

toInfluxTag :: (String, T.DataPointValue) -> (Key, Key)
toInfluxTag = bimap (formatKey string) toInfluxKey

toInfluxField :: (String, T.DataPointValue) -> (Key, LineField)
toInfluxField = bimap (formatKey string) toInfluxVal

toLine :: DataPoint a => Measurement -> a -> Line UTCTime
toLine measurement light =
  Line @UTCTime
    measurement
    -- Tags:
    (Map.fromList $ map toInfluxTag (tags light))
    -- Fields:
    (Map.fromList $ map toInfluxField (fields light))
    Nothing

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
      (https (S.fromString bridgeIp) /: ("bridge" :: Text) /: S.fromString username /: ("lights" :: Text))
      NoReqBody
      lbsResponse
      (bearerHeader token)

  let resData = decode (responseBody res) :: Maybe Value

  case resData of
    Just x -> return x
    Nothing -> error "Failed to parse response from Hue Bridge"

instance T.DataPoint Light where
    tags l = [("name", T.StringValue $ name l)
             , ("uuid", T.StringValue $ lightId l)
             , ("type", T.StringValue $ typeName l)]
    -- Fields:
    fields l =
        [ ("on"        , T.BoolValue $ on l)
        , ("brightness", T.IntValue $ brightness l)
        , ("hue"       , T.IntValue $ hue l)
        , ("saturation", T.IntValue $ saturation l)
        , ("xcolor"    , T.DoubleValue (fst . xyColor $ l))
        , ("ycolor"    , T.DoubleValue (snd . xyColor $ l))
        , ("ctTemp"    , T.IntValue $ ctTemp l)
        , ("reachable" , T.BoolValue $ reachable l)
        ]

data Light = Light
  { name :: String,
    lightId :: String,
    typeName :: String,
    on :: Bool,
    brightness :: Int,
    hue :: Int,
    saturation :: Int,
    xyColor :: (Double, Double),
    ctTemp :: Int,
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
