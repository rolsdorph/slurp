{-# LANGUAGE FlexibleContexts #-}

module DBUtil where

import           Data.Convertible.Base
import           Data.List
import           Database.HDBC

eitherValFrom
    :: Convertible SqlValue a
    => String
    -> [(String, SqlValue)]
    -> Either String a
eitherValFrom colName allVals = case maybeSqlVal of
    (Just sqlVal) -> Right $ fromSql sqlVal
    _             -> Left $ "Missing column " ++ colName
    where maybeSqlVal = snd <$> find (\v -> fst v == colName) allVals

valFrom
    :: Convertible SqlValue (Maybe a)
    => String
    -> [(String, SqlValue)]
    -> Maybe a
valFrom colName allVals = case maybeSqlVal of
    (Just sqlVal) -> fromSql sqlVal
    _             -> Nothing
    where maybeSqlVal = snd <$> find (\v -> fst v == colName) allVals
