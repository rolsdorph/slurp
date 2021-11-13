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

-- Extracts the desired tags and fields from the given JSON source
collect :: (HasHttp m, HasLogger m) => SimpleShallowJsonSource -> m (Either String SourceData)
collect source = do
    let maybeUri = mkURI >=> useURI $ url (ssDefinition source)
    case maybeUri of
        (Just (Left (httpUri, _))) -> doGet httpUri
        (Just (Right (httpsUri, _ ))) -> doGet httpsUri
        _ -> return $ Left "Failed to parse collection URL"
    where
      doGet :: (HasLogger m, HasHttp m) => Url scheme -> m (Either String SourceData)
      doGet uri = do
        res <- simpleGet uri (header "Authorization" (authHeader (ssDefinition source)))
        let decoded = res >>= eitherDecode
        case decoded of
          (Right val) -> Right <$> extract source val
          (Left err) -> return $ Left err

extract ::
  (HasLogger m) => SimpleShallowJsonSource -> Value -> m SourceData
extract (SimpleShallowJsonSource ssId definition) value = do
  let tagsOrErrors = List.map (getValue value) $ tagMappings definition
  let fieldsOrErrors = List.map (getValue value) $ fieldMappings definition

  logErrors' tagsOrErrors
  logErrors' fieldsOrErrors

  let tags = mapMaybe rightOrNothing tagsOrErrors
  let fields = mapMaybe rightOrNothing fieldsOrErrors

  let dp = DataPoint {tags = tags, fields = fields}

  return
    SourceData
      { sourceId = ssId,
        sourceOwnerId = shallowOwnerId definition,
        datakey = genericDataKey definition,
        datapoints = [dp]
      }

getValue :: Value -> JsonMapping -> Either String MappedValue
getValue value mapping = mapRight (snd mapping, )
    $ parseEither (withObject "SimpleJson" $ \o -> o .: fst mapping) value

logErrors' :: (HasLogger m) => [Either String a] -> m ()
logErrors' = mapM_
    (\case
        (Left errMsg) -> errorLog errMsg
        _             -> return ()
    )
