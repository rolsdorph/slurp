{-# LANGUAGE GeneralisedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeSynonymInstances #-}

import Data.Aeson (Value (Bool, Number, String), encode, object)
import qualified Data.ByteString.Lazy as LB
import Data.Either (isRight)
import Data.Functor.Identity
import Data.List (isInfixOf)
import Data.Time.Calendar (Day (ModifiedJulianDay))
import Data.Time.Clock (UTCTime (..), secondsToDiffTime)
import qualified SimpleSource as SS
import Test.Hspec (Spec, describe, hspec, it, shouldBe, shouldSatisfy)
import Types

main :: IO ()
main = hspec spec

newtype FailingStack a = FailingStack (Identity a)
  deriving (Functor, Applicative, Monad)

runFailingStack :: FailingStack a -> a
runFailingStack (FailingStack (Identity a)) = a

instance SS.HasHttp FailingStack where
  simpleGet _ _ = return $ Left "HTTP error"

instance SS.HasLogger FailingStack where
  infoLog = undefined
  errorLog = undefined

newtype InvalidJsonStack a = InvalidJsonStack (Identity a)
  deriving (Functor, Applicative, Monad)

runInvalidJsonStack :: InvalidJsonStack a -> a
runInvalidJsonStack (InvalidJsonStack (Identity a)) = a

instance SS.HasHttp InvalidJsonStack where
  simpleGet _ _ = return $ Right invalidResponse

instance SS.HasLogger InvalidJsonStack where
  infoLog = undefined
  errorLog = undefined

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

    it "Returns an error when the JSON fetch fails" $ do
      let res = runFailingStack $ SS.collect testSource
      res `shouldSatisfy` isErrorContaining "HTTP error"

    it "Returns an error when encountering malformed JSON" $ do
      let res = runInvalidJsonStack $ SS.collect testSource
      res `shouldSatisfy` isErrorContaining "not a valid json value"

    it "Returns an error if the source does not have an ID" $ do
      let res = runSuccessStack $ SS.collect testSource {genericSourceId = Nothing}
      res `shouldSatisfy` isErrorContaining "Source ID missing"

    it "Extracts tags and fields from the result JSON" $ do
      let res = runSuccessStack $ SS.collect testSource
      res `shouldSatisfy` isRight
      case res of
        (Left _) -> fail "Should never happen"
        (Right collectedSource) -> do
          sourceId collectedSource `shouldBe` testSourceId
          sourceOwnerId collectedSource `shouldBe` shallowOwnerId testSource
          datakey collectedSource `shouldBe` genericDataKey testSource

          datapoints collectedSource
            `shouldBe` [ DataPoint
                           { tags = [("mappedKey1", IntValue 1337)],
                             fields =
                               [ ("mappedKey2", StringValue "what"),
                                 ("mappedKey3", BoolValue False)
                               ]
                           }
                       ]

isErrorContaining :: String -> Either String a -> Bool
isErrorContaining desired (Left actual) = desired `isInfixOf` actual
isErrorContaining _ _ = False

someTime :: UTCTime
someTime = UTCTime (ModifiedJulianDay 0) (secondsToDiffTime 0)

noopLogger :: String -> IO ()
noopLogger = const (return ())

testSourceId :: String
testSourceId = "some-id"

testSource :: SimpleShallowJsonSource
testSource =
  SimpleShallowJsonSource
    { genericSourceId = Just testSourceId,
      genericDataKey = "some-key",
      shallowOwnerId = "1234",
      shallowCreatedAt = someTime,
      url = "https://localhost:1337/source.json",
      authHeader = "auth here",
      tagMappings = [("key1", "mappedKey1")],
      fieldMappings = [("key2", "mappedKey2"), ("key3", "mappedKey3")]
    }

invalidResponse :: LB.ByteString
invalidResponse = "asd[[[]]{}"

validResponse :: LB.ByteString
validResponse = encode testResponse

testResponse :: Value
testResponse =
  object
    [ ("key1", Number 1337),
      ("key2", String "what"),
      ("key3", Bool False)
    ]
