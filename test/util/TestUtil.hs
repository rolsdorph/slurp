module TestUtil where

import Data.Time.Calendar (Day (ModifiedJulianDay))
import Data.Time.Clock (UTCTime (..), secondsToDiffTime)
import Types

someTime :: UTCTime
someTime = UTCTime (ModifiedJulianDay 0) (secondsToDiffTime 0)

returnIf :: (Monad m) => User -> a -> (UserId -> m [a])
returnIf user a = \uid -> return $ if (userId user == uid) then [a] else []
