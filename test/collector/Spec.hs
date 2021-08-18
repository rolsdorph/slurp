{-# LANGUAGE GeneralisedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeSynonymInstances #-}

import Data.Aeson (Value (Bool, Number, String), encode, object)
import qualified Data.ByteString.Lazy as LB
import Data.Functor.Identity
import Data.List (isInfixOf)
import Data.Time.Calendar (Day (ModifiedJulianDay))
import Data.Time.Clock (UTCTime (..), secondsToDiffTime)
import qualified SimpleSource as SS
import Test.Hspec (Spec, describe, hspec, it, shouldSatisfy)
import Types

main :: IO ()
main = hspec spec

newtype SuccessStack a = SuccessStack (Identity a)
  deriving (Functor, Applicative, Monad)

instance SS.HasHttp SuccessStack where
  simpleGet _ _ = return $ Right validResponse

instance SS.HasLogger SuccessStack where
  infoLog = undefined
  errorLog = undefined

runSuccessStack :: SuccessStack a -> a
runSuccessStack (SuccessStack (Identity a)) = a

spec :: Spec
spec = do
  describe "Simple Source Collector" $ do
    it "Returns an error when encountering a malformed URL" $ do
      let res = runSuccessStack $ SS.collect (testSource {url = "MALFORMED"})
      res `shouldSatisfy` isErrorContaining "collection URL"

    it "Returns an error when encountering malformed JSON" $ do
      putStrLn "TODO"

    it "Returns an error if the source does not have an ID" $ do
      putStrLn "TODO"

    it "Extracts tags and fields from the result JSON" $ do
      putStrLn "TODO"

isErrorContaining :: String -> Either String a -> Bool
isErrorContaining desired (Left actual) = desired `isInfixOf` actual
isErrorContaining _ _ = False

someTime :: UTCTime
someTime = UTCTime (ModifiedJulianDay 0) (secondsToDiffTime 0)

noopLogger :: String -> IO ()
noopLogger = const (return ())

testSource :: SimpleShallowJsonSource
testSource =
  SimpleShallowJsonSource
    { genericSourceId = Just "some-id",
      genericDataKey = "some-key",
      shallowOwnerId = "1234",
      shallowCreatedAt = someTime,
      url = "https://localhost:1337/source.json",
      authHeader = "auth here",
      tagMappings = [],
      fieldMappings = []
    }

validResponse :: LB.ByteString
validResponse = encode testResponse

testResponse :: Value
testResponse =
  object
    [ ("key1", Number 1337),
      ("key2", String "what"),
      ("key3", Bool False)
    ]
