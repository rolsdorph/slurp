{-# LANGUAGE OverloadedStrings #-}

module Util where

import qualified Data.ByteString.Char8         as C
import qualified Data.ByteString.UTF8          as U
import qualified Data.ByteString.Lazy          as L

justOrErr :: L.ByteString -> Maybe a -> Either L.ByteString a
justOrErr errMsg Nothing  = Left errMsg
justOrErr _      (Just x) = Right x

mapRight :: (b -> c) -> Either a b -> Either a c
mapRight _ (Left x) = Left x
mapRight mapper (Right err) = Right (mapper err)

mapLeft :: (a -> b) -> Either a c -> Either b c
mapLeft _ (Right x) = Right x
mapLeft mapper (Left err) = Left (mapper err)

combineEithers :: Either L.ByteString a -> Either L.ByteString a -> Either L.ByteString (a,a)
combineEithers (Right x) (Right y) = Right (x, y)
combineEithers (Left errX) (Left errY) = Left (errX <> ", " <> errY)
combineEithers (Left errX) _ = Left errX
combineEithers _ (Left errY) = Left errY

utf8ToLbs :: U.ByteString -> L.ByteString
utf8ToLbs utfString = L.fromStrict (C.pack (U.toString utfString))
