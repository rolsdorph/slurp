{-# LANGUAGE OverloadedStrings #-}
module Html where

import           Network.Wai.Parse
import qualified Data.ByteString.Lazy          as L

-- Checks whether a parameter has the given name
paramNamed :: L.ByteString -> Param -> Bool
paramNamed name param | paramName == L.toStrict name = True
                      | otherwise         = False
    where paramName = fst param
