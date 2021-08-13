{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings #-}

import Api (ErrorResponse (..), getSimpleSources, postSimpleSource)
import Control.Monad.Except (forM_, runExceptT, throwError)
import Control.Monad.Identity (Identity (..))
import Data.ByteString (ByteString)
import Data.Time.Calendar (Day (ModifiedJulianDay))
import Data.Time.Clock (UTCTime (..), secondsToDiffTime)
import Network.HTTP.Types.Status (status200, status201)
import Network.Wai (Response, responseStatus)
import Network.Wai.Parse (Param)
import SimpleSourceDB
import Test.Hspec (Spec, describe, hspec, it, shouldSatisfy)
import Types (AuthType (Insecure), User (..))
import Util (MonadTime, currentTime)

main :: IO ()
main = hspec spec

spec :: Spec
spec = do
  describe "GET /simplesources" $ do
    it "Returns a list of sources" $
      do
        runWorkingStore (runExceptT (getSimpleSources (Just validUser)))
        `shouldSatisfy` is200

    it "Returns 500 when the database layer fails" $
      do
        runFailingStore (runExceptT (getSimpleSources (Just validUser)))
        `shouldSatisfy` is500

  describe "POST /simplesources" $ do
    it "Returns 401 when not logged in" $
      do
        runWorkingStore . runExceptT $ postSimpleSource Nothing []
        `shouldSatisfy` is401

    it "Returns 400 for invalid JSON" $
      do
        runWorkingStore . runExceptT $ postSimpleSource (Just validUser) (replace ("tagMappings", "]]]") validSource)
        `shouldSatisfy` is400

    forM_
      (keys validSource)
      ( \k -> it ("Returns 400 for missing " ++ show k) $ do
          runWorkingStore (runExceptT (postSimpleSource (Just validUser) (remove k validSource))) `shouldSatisfy` is400
      )

    it "Returns 201 for successful store" $
      do
        runWorkingStore . runExceptT $ postSimpleSource (Just validUser) validSource
        `shouldSatisfy` is201

    it "Returns 500 for DB errors" $
      do
        runFailingStore . runExceptT $ postSimpleSource (Just validUser) validSource
        `shouldSatisfy` is500

is200 :: Either a Response -> Bool
is200 (Right res) = responseStatus res == status200
is200 _ = False

is201 :: Either a Response -> Bool
is201 (Right res) = responseStatus res == status201
is201 _ = False

is400 :: Either ErrorResponse a -> Bool
is400 (Left (BadRequest _)) = True
is400 _ = False

is401 :: Either ErrorResponse a -> Bool
is401 (Left (Unauthorized _)) = True
is401 _ = False

is500 :: Either ErrorResponse a -> Bool
is500 (Left (InternalServerError _)) = True
is500 _ = False

instance Show Response where
  show r = "Response with status code " ++ (show . responseStatus) r

newtype FailingStore a = FailingStore (Identity a)
  deriving (Functor, Applicative, Monad)

instance MonadTime FailingStore where
  currentTime = return $ UTCTime (ModifiedJulianDay 0) (secondsToDiffTime 0)

instance MonadSimpleSource FailingStore where
  storeSimpleSource _ = throwError "what"
  getUserSimpleSources _ = throwError "que"

runFailingStore :: FailingStore a -> a
runFailingStore (FailingStore (Identity a)) = a

newtype WorkingStore a = WorkingStore (Identity a)
  deriving (Functor, Applicative, Monad)

instance MonadTime WorkingStore where
  currentTime = return $ UTCTime (ModifiedJulianDay 0) (secondsToDiffTime 0)

instance MonadSimpleSource WorkingStore where
  storeSimpleSource source = return source
  getUserSimpleSources _ = return []

runWorkingStore :: WorkingStore a -> a
runWorkingStore (WorkingStore (Identity a)) = a

validUser :: User
validUser =
  User
    { userId = "user-1-id",
      userCreatedAt = UTCTime (ModifiedJulianDay 0) (secondsToDiffTime 0),
      authType = Insecure,
      thirdPartyId = Just "third-party-user-1"
    }

validSource :: [Param]
validSource =
  [ ("datakey", "some-key"),
    ("url", "some-url"),
    ("authHeader", "123-auth"),
    ("tagMappings", "[]"),
    ("fieldMappings", "[]")
  ]

replace :: (ByteString, ByteString) -> [Param] -> [Param]
replace (newKey, newVal) = map (\el -> if fst el == newKey then (newKey, newVal) else el)

remove :: ByteString -> [Param] -> [Param]
remove key = filter (\el -> fst el /= key)

keys :: [Param] -> [ByteString]
keys = map fst
