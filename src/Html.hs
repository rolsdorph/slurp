{-# LANGUAGE OverloadedStrings #-}
module Html where

import           Network.Wai.Parse
import qualified Data.ByteString.Char8         as C

-- Checks whether a checkbox is checked :)
isCheckboxSet :: Param -> Bool
isCheckboxSet param = snd param == "on"

-- Checks whether a parameter has the given name
paramNamed :: C.ByteString -> Param -> Bool
paramNamed name param | paramName == name = True
                      | otherwise         = False
    where paramName = fst param
