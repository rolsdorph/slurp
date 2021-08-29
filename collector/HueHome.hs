{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

module HueHome
  ( collect, toDataPoint )
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

hueBridgeApi :: BridgeHost
hueBridgeApi = "api.meethue.com" -- TODO: read from config

collect :: (HasLogger m, HasHttp m) => Home -> m (Either String SourceData)
collect home = do
  let maybeToken = accessToken home
  let maybeUsername = hueUsername home
  case (maybeToken, maybeUsername) of
    (Just token, Just username) -> do
      (fmap . fmap) (toSourceData home) (collect' hueBridgeApi username (Just token))
    _ -> return $ Left "Username or token missing, not able to collect home"

toSourceData :: Home -> [Light] -> SourceData
toSourceData home lights =
  SourceData
    { sourceId = uuid home,
      sourceOwnerId = ownerId home,
      datakey = homeDataKey home,
      datapoints = map toDataPoint lights
    }


type BridgeUsername = String
type BridgeHost = String
type Token = String
collect' :: (HasHttp m) => BridgeHost -> BridgeUsername -> Maybe Token -> m (Either String [Light])
collect' host username token = do
  lights <- getLights host username token
  return $ lights >>= parseLights

parseLights :: Value -> Either String [Light]
parseLights = parseEither (withObject "lightsList" (mapM (parseJSON . snd) . HM.toList))

bearerHeader :: Maybe Token -> Option scheme
bearerHeader (Just token) = header "Authorization" (U.fromString $ "Bearer " ++ token)
bearerHeader Nothing = mempty

getLights :: (HasHttp m) => String -> String -> Maybe Token -> m (Either String Value)
getLights bridgeIp username token = do
  res <-
    simpleGet
      (https (S.fromString bridgeIp) /: ("bridge" :: Text) /: S.fromString username /: ("lights" :: Text))
      (bearerHeader token)

  return $ res >>= eitherDecode

toDataPoint :: Light -> T.DataPoint
toDataPoint l = T.DataPoint {
    tags = [("name", T.StringValue $ name l)
             , ("uuid", T.StringValue $ lightId l)
             , ("type", T.StringValue $ typeName l)],
    fields =
        [ ("on"        , T.BoolValue $ on l)
        , ("brightness", T.IntValue $ brightness l)
        , ("hue"       , T.IntValue $ hue l)
        , ("saturation", T.IntValue $ saturation l)
        , ("xcolor"    , T.DoubleValue (fst . xyColor $ l))
        , ("ycolor"    , T.DoubleValue (snd . xyColor $ l))
        , ("ctTemp"    , T.IntValue $ ctTemp l)
        , ("reachable" , T.BoolValue $ reachable l)
        ]
}

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
  deriving (Show, Eq)

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
