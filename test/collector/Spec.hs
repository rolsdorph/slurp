{-# LANGUAGE GeneralisedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeSynonymInstances #-}

import Data.Aeson (Value (Array, Bool, Number, String), encode, object)
import qualified Data.ByteString.Lazy as LB
import Data.Either (isRight)
import Data.Functor.Identity
import Data.List (isInfixOf)
import Data.Time.Calendar (Day (ModifiedJulianDay))
import Data.Time.Clock (UTCTime (..), secondsToDiffTime)
import Data.Vector (fromList)
import qualified HueHome as HH
import qualified SimpleSource as SS
import Test.Hspec (Spec, describe, hspec, it, shouldBe, shouldSatisfy)
import Types

main :: IO ()
main = hspec spec

newtype FailingStack a = FailingStack (Identity a)
  deriving (Functor, Applicative, Monad)

runFailingStack :: FailingStack a -> a
runFailingStack (FailingStack (Identity a)) = a

instance HasHttp FailingStack where
  simpleGet _ _ = return $ Left "HTTP error"

instance HasLogger FailingStack where
  infoLog = undefined
  errorLog = undefined

newtype InvalidJsonStack a = InvalidJsonStack (Identity a)
  deriving (Functor, Applicative, Monad)

runInvalidJsonStack :: InvalidJsonStack a -> a
runInvalidJsonStack (InvalidJsonStack (Identity a)) = a

instance HasHttp InvalidJsonStack where
  simpleGet _ _ = return $ Right invalidResponse

instance HasLogger InvalidJsonStack where
  infoLog = undefined
  errorLog = undefined

newtype SimpleStack a = SimpleStack (Identity a)
  deriving (Functor, Applicative, Monad)

instance HasHttp SimpleStack where
  simpleGet _ _ = return $ Right validSimpleResponse

instance HasLogger SimpleStack where
  infoLog = undefined
  errorLog = undefined

runSimpleStack :: SimpleStack a -> a
runSimpleStack (SimpleStack (Identity a)) = a

newtype LightsStack a = LightsStack (Identity a)
  deriving (Functor, Applicative, Monad)

instance HasHttp LightsStack where
  simpleGet _ _ = return $ Right validLightsResponse

instance HasLogger LightsStack where
  infoLog = undefined
  errorLog = undefined

runLightsStack :: LightsStack a -> a
runLightsStack (LightsStack (Identity a)) = a

spec :: Spec
spec = do
  describe "Simple Source Collector" $ do
    it "Returns an error when encountering a malformed URL" $ do
      let res = runSimpleStack $ SS.collect (testSource {url = "MALFORMED"})
      res `shouldSatisfy` isErrorContaining "collection URL"

    it "Returns an error when the JSON fetch fails" $ do
      let res = runFailingStack $ SS.collect testSource
      res `shouldSatisfy` isErrorContaining "HTTP error"

    it "Returns an error when encountering malformed JSON" $ do
      let res = runInvalidJsonStack $ SS.collect testSource
      res `shouldSatisfy` isErrorContaining "not a valid json value"

    it "Returns an error if the source does not have an ID" $ do
      let res = runSimpleStack $ SS.collect testSource {genericSourceId = Nothing}
      res `shouldSatisfy` isErrorContaining "Source ID missing"

    it "Extracts tags and fields from the result JSON" $ do
      let res = runSimpleStack $ SS.collect testSource
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

  describe "Hue Home Collector" $ do
    it "Returns an error when the collection request fails" $ do
      let res = runFailingStack $ HH.collect "somehost" "myusername" (Just "token")
      res `shouldSatisfy` isErrorContaining "HTTP error"

    it "Returns an error when receiving invalid data" $ do
      let res = runInvalidJsonStack $ HH.collect "somehost" "myusername" (Just "token")
      res `shouldSatisfy` isErrorContaining "not a valid json value"

    it "Parses the received JSON into lights " $ do
      let res = runLightsStack $ HH.collect "somehost" "myusername" (Just "token")
      res `shouldSatisfy` isRight
      case res of
        (Left _) -> fail "Should never happen"
        (Right lights) ->
          HH.toDataPoint <$> lights
            `shouldBe` [ DataPoint
                           { tags = [("name", StringValue "my-light"), ("uuid", StringValue "some-uuid"), ("type", StringValue "lighttype")],
                             fields = [("on", BoolValue True), ("brightness", IntValue 1), ("hue", IntValue 2), ("saturation", IntValue 1337), ("xcolor", DoubleValue 4.0), ("ycolor", DoubleValue 2.0), ("ctTemp", IntValue 42), ("reachable", BoolValue True)]
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

validSimpleResponse :: LB.ByteString
validSimpleResponse = encode simpleResponse

validLightsResponse :: LB.ByteString
validLightsResponse = encode lightsResponse

simpleResponse :: Value
simpleResponse =
  object
    [ ("key1", Number 1337),
      ("key2", String "what"),
      ("key3", Bool False)
    ]

lightsResponse :: Value
lightsResponse =
  object
    [ ( "1",
        object
          [ ("name", "my-light"),
            ("uniqueid", "some-uuid"),
            ("type", "lighttype"),
            ( "state",
              object
                [ ("on", Bool True),
                  ("bri", Number 1),
                  ("hue", Number 2),
                  ("sat", Number 1337),
                  ("xy", Array $ fromList [Number 4, Number 2]),
                  ("ct", Number 42),
                  ("reachable", Bool True)
                ]
            )
          ]
      )
    ]
