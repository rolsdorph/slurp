{-# LANGUAGE TupleSections #-}
{-# LANGUAGE OverloadedStrings #-}

module GenericJson where

import           Control.Monad
import           Control.Monad.IO.Class
import           Data.Aeson
import           Data.Aeson.Types
import qualified Data.ByteString               as B
import qualified Data.List                     as List
import           Data.Maybe
import           Data.Text
import           Network.HTTP.Req
import           Text.URI

import           Types
import           Util

-- Extracts the desired tags and fields from the given JSON source
collect
    :: (String -> IO ())
    -> SimpleShallowJsonSource
    -> IO (Either String SourceData)
collect logger source = runReq defaultHttpConfig $ do
    let maybeUri = mkURI >=> useHttpsURI $ url source
    case maybeUri of
        (Just (uri, _)) -> do
            res <- req GET
                       uri
                       NoReqBody
                       lbsResponse
                       (header "Authorization" (authHeader source))

            let decoded = eitherDecode (responseBody res)
            case decoded of
                (Right val) -> do
                    res <- liftIO $ extract logger source val
                    pure res
                (Left err) -> pure $ Left err

        _ -> pure $ Left "Failed to parse collection URL"

extract
    :: (String -> IO ())
    -> SimpleShallowJsonSource
    -> Value
    -> IO (Either String SourceData)
extract logger definition value = do
    let tagsOrErrors   = List.map (getValue value) $ tagMappings definition
    let fieldsOrErrors = List.map (getValue value) $ fieldMappings definition

    logErrors logger tagsOrErrors
    logErrors logger fieldsOrErrors

    let tags          = mapMaybe rightOrNothing tagsOrErrors
    let fields        = mapMaybe rightOrNothing fieldsOrErrors

    let dp            = DataPoint { tags = tags, fields = fields }

    let maybeSourceId = genericSourceId definition
    case maybeSourceId of
        (Just sourceId) -> pure $ Right (SourceData {
                                                sourceId   = sourceId,
                                                datakey    = genericDataKey definition,
                                                datapoints = [dp]
                                  })
        _ -> pure $ Left "Source ID missing, skipping source"

getValue :: Value -> JsonMapping -> Either String MappedValue
getValue value mapping = mapRight (snd mapping, )
    $ parseEither (withObject "SimpleJson" $ \o -> o .: fst mapping) value
