{-# LANGUAGE OverloadedStrings #-}

module TestUtil where

import Control.Concurrent (Chan, readChan)
import Data.Time.Calendar (Day (ModifiedJulianDay))
import Data.Time.Clock (UTCTime (..), secondsToDiffTime)
import Types

someTime :: UTCTime
someTime = UTCTime (ModifiedJulianDay 0) (secondsToDiffTime 0)

returnIf :: (Monad m) => User -> a -> (UserId -> m [a])
returnIf user a = \uid -> return $ if (userId user == uid) then [a] else []
-- Grabs elements from the given channel until {target} elements have been read,
-- and then returns the accumulated elements
waitForElems :: Chan a -> Int -> IO [a]
waitForElems dataChan target = go dataChan target []
  where
    go chan targetCount cur =
      readChan dataChan >>= \nextElem -> do
        let newCur = nextElem : cur
        if length newCur == target then return newCur else go chan targetCount newCur
