{-# LANGUAGE FlexibleContexts #-}

module DBUtil where

import           Data.Convertible.Base
import           Data.List
import           Database.HDBC

valFrom
    :: Convertible SqlValue (Maybe a)
    => String
    -> [(String, SqlValue)]
    -> Maybe a
valFrom colName allVals = case maybeSqlVal of
    (Just sqlVal) -> fromSql sqlVal
    _             -> Nothing
    where maybeSqlVal = snd <$> find (\v -> fst v == colName) allVals
