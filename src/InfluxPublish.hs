{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications #-}

module InfluxPublish
    ( publish
    )
where

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
import           Network.HTTP.Client.TLS        ( tlsManagerSettings )

import           Types

type InfluxHost = Text
type Port = Int
type Username = Text
type Password = Text
publish
    :: DataPoint a => InfluxHost -> Port -> Username -> Password -> Database -> Measurement -> [a] -> IO ()
publish host port username password db measurement points =
    writeBatch (mkWriteParams host port username password db)
        $ map (toLine measurement) points

mkWriteParams :: InfluxHost -> Port -> Username -> Password -> Database -> WriteParams
mkWriteParams hostName port username password db =
    writeParams db
        &  authentication
        ?~ credentials username password
        &  server
        .~ (secureServer & host .~ hostName & Database.InfluxDB.port .~ port)
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

toLine :: DataPoint a => Measurement -> a -> Line UTCTime
toLine measurement element = Line @UTCTime
    measurement
    -- Tags:
    (Map.fromList $ map toInfluxTag (tags element))
    -- Fields:
    (Map.fromList $ map toInfluxField (fields element))
    Nothing
