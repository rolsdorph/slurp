module Generators where

import Data.ByteString.UTF8 (fromString)
import Data.Text (pack)
import Test.QuickCheck
import TestUtil
import Types hiding (fromString)

testSinkFor :: User -> Gen InfluxSink
testSinkFor owner =
  InfluxSink
    <$> arbitrary
    <*> ( InfluxDefinition (userId owner)
            <$> arbitrary
            <*> arbitrary
            <*> arbitrary
            <*> arbitrary
            <*> arbitrary
            <*> return someTime
        )

testSsFor :: User -> Gen SimpleShallowJsonSource
testSsFor owner =
  SimpleShallowJsonSource
    <$> arbitrary
    <*> ( SimpleSourceDefinition
            <$> arbitrary
            <*> return (userId owner)
            <*> return someTime
            <*> (pack <$> arbitrary)
            <*> (fromString <$> arbitrary)
            <*> return []
            <*> return []
        )

testHomeFor :: User -> Gen Home
testHomeFor owner =
  Home
    <$> arbitrary
    <*> arbitrary
    <*> return (userId owner)
    <*> return someTime
    <*> return Verified
    <*> (Just <$> arbitrary)
    <*> (Just <$> arbitrary)
    <*> (Just <$> arbitrary)
    <*> return (Just someTime)
    <*> return (Just someTime)
    <*> (Just <$> arbitrary)

testSourceDataFor :: Home -> Gen SourceData
testSourceDataFor home =
  arbitrary >>= \dps ->
    return
      SourceData
        { sourceId = uuid home,
          sourceOwnerId = ownerId home,
          datakey = homeDataKey home,
          datapoints = getDp <$> dps
        }

testSourceDataForSs :: SimpleShallowJsonSource -> Gen SourceData
testSourceDataForSs (SimpleShallowJsonSource sId definition) =
  arbitrary >>= \dps ->
    return
      SourceData
        { sourceId = sId,
          sourceOwnerId = shallowOwnerId definition,
          datakey = genericDataKey definition,
          datapoints = getDp <$> dps
        }

newtype TestUser = TestUser {getUser :: User}

instance Arbitrary TestUser where
  arbitrary = do
    someUserId <- arbitrary
    someThirdParty <- Just <$> arbitrary
    return $
      TestUser
        User
          { userId = someUserId,
            userCreatedAt = someTime,
            authType = Insecure,
            thirdPartyId = someThirdParty
          }

newtype TestDp = TestDp {getDp :: DataPoint}

instance Arbitrary TestDp where
  arbitrary = do
    t1 <- arbitrary
    t2 <- arbitrary
    f1 <- arbitrary
    f2 <- arbitrary
    v1 <- IntValue <$> arbitrary
    v2 <- DoubleValue <$> arbitrary
    v3 <- StringValue <$> arbitrary
    v4 <- BoolValue <$> arbitrary
    return $
      TestDp
        DataPoint
          { tags = [(t1, v1), (t2, v2)],
            fields = [(f1, v3), (f2, v4)]
          }
