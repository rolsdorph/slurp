{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Collector
  ( collect )
where

import Control.Lens
import qualified Data.ByteString.UTF8          as U
import Data.Aeson
import Data.Aeson.Types
import qualified Data.HashMap.Strict as HM
import Data.Int (Int64)
import Data.Maybe
import qualified Data.String as S
import Data.Text (Text)
import Network.HTTP.Req
import System.IO (BufferMode (LineBuffering), hSetBuffering, stdout)
import Network.HTTP.Client (newManager)

import Types as T

type BridgeUsername = String
type BridgeHost = String
type Token = String
collect :: BridgeHost -> BridgeUsername -> Maybe Token -> IO [Light]
collect host username token = do
  lights <- getLights host username token
  pure $ parseLights lights

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
    sourceId = lightId
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
