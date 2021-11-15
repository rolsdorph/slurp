{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications #-}

module InfluxPublish
    ( publish
    , InfluxPushResult (..)
    )
where

import           Control.Exception
import           Control.Lens
import qualified Data.Map                      as Map
import           Data.Text                      ( Text
                                                , pack
                                                )
import           Data.Time
import           Database.InfluxDB
import           Database.InfluxDB.Format       ( decimal
                                                , key
                                                , string
                                                )
import Network.HTTP.Client.TLS (tlsManagerSettings)
import Types

data InfluxPushResult = Success | Error String

type InfluxHost = Text
type Port = Int
type Username = Text
type Password = Text

publish ::
  InfluxHost -> Port -> Bool -> Username -> Password -> Database -> Measurement -> [DataPoint] -> IO InfluxPushResult
publish host port useTLS username password db measurement points =
  catch
    ((writeBatch params measurements) >> pure Success)
    (\ex -> (pure . Error) $ "Failed to push to Influx: " ++ show (ex :: InfluxException))
  where
    params = mkWriteParams host port useTLS username password db
    measurements = map (toLine measurement) points

mkWriteParams :: InfluxHost -> Port -> Bool -> Username -> Password -> Database -> WriteParams
mkWriteParams hostName port useTLS username password db =
    writeParams db
        &  authentication
        ?~ credentials username password
        &  server
        .~ (defaultServer & ssl .~ useTLS & host .~ hostName & Database.InfluxDB.port .~ port)
        &  manager
        .~ Left tlsManagerSettings

-- Keys are used as keys AND values for tags, and as keys for fields
toInfluxKey :: DataPointValue -> Key
toInfluxKey (IntValue    val) = formatKey string $ show val
toInfluxKey (DoubleValue val) = formatKey string $ show val
toInfluxKey (StringValue val) = formatKey string val
toInfluxKey (BoolValue   val) = formatKey string $ show val

-- LineFields are used as field values
toInfluxVal :: DataPointValue -> LineField
toInfluxVal (IntValue    val) = FieldInt $ fromIntegral val
toInfluxVal (DoubleValue val) = FieldFloat val
toInfluxVal (StringValue val) = FieldString $ pack val
toInfluxVal (BoolValue   val) = FieldBool val

toInfluxTag :: (String, DataPointValue) -> (Key, Key)
toInfluxTag = bimap (formatKey string) toInfluxKey

toInfluxField :: (String, DataPointValue) -> (Key, LineField)
toInfluxField = bimap (formatKey string) toInfluxVal

toLine :: Measurement -> DataPoint -> Line UTCTime
toLine measurement element = Line @UTCTime
    measurement
    -- Tags:
    (Map.fromList $ map toInfluxTag (tags element))
    -- Fields:
    (Map.fromList $ map toInfluxField (fields element))
    Nothing
