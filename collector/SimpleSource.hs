{-# LANGUAGE TupleSections #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE LambdaCase #-}

module SimpleSource where

import           Control.Exception
import           Control.Monad
import           Control.Monad.IO.Class
import           Data.Aeson
import           Data.Aeson.Types
import qualified Data.ByteString               as B
import qualified Data.ByteString.Lazy          as LB
import qualified Data.List                     as List
import           Data.Maybe
import           Data.Text
import           Network.HTTP.Req
import           Text.URI

import           Types
import           Util

class (Monad m) => HasHttp m where
  simpleGet :: Url a -> B.ByteString -> m (Either String LB.ByteString)

class Monad m => HasLogger m where
  infoLog :: String -> m ()
  errorLog :: String -> m ()

-- Extracts the desired tags and fields from the given JSON source
collect :: (HasHttp m, HasLogger m) => SimpleShallowJsonSource -> m (Either String SourceData)
collect source = do
    let maybeUri = mkURI >=> useHttpsURI $ url source
    case maybeUri of
        (Just (uri, _)) -> do
            res <- simpleGet uri (authHeader source)

            let decoded = res >>= eitherDecode
            case decoded of
                (Right val) -> extract source val
                (Left err) -> return $ Left err

        _ -> return $ Left "Failed to parse collection URL"

extract
    :: (HasLogger m) => SimpleShallowJsonSource -> Value -> m (Either String SourceData)
extract definition value = do
    let tagsOrErrors   = List.map (getValue value) $ tagMappings definition
    let fieldsOrErrors = List.map (getValue value) $ fieldMappings definition

    logErrors' tagsOrErrors
    logErrors' fieldsOrErrors

    let tags          = mapMaybe rightOrNothing tagsOrErrors
    let fields        = mapMaybe rightOrNothing fieldsOrErrors

    let dp            = DataPoint { tags = tags, fields = fields }

    let maybeSourceId = genericSourceId definition
    case maybeSourceId of
        (Just sourceId) ->return $ Right (SourceData {
                                                sourceId   = sourceId,
                                                sourceOwnerId = shallowOwnerId definition,
                                                datakey    = genericDataKey definition,
                                                datapoints = [dp]
                                  })
        _ -> return $ Left "Source ID missing, skipping source"

getValue :: Value -> JsonMapping -> Either String MappedValue
getValue value mapping = mapRight (snd mapping, )
    $ parseEither (withObject "SimpleJson" $ \o -> o .: fst mapping) value

logErrors' :: (HasLogger m) => [Either String a] -> m ()
logErrors' = mapM_
    (\case
        (Left errMsg) -> errorLog errMsg
        _             -> return ()
    )
