{-# LANGUAGE OverloadedStrings #-}

import           Data.ByteString
import           Data.Either
import           Data.String
import           Test.Hspec
import           OAuth

nonce = "mynonce123"
realm = "myrealm"
headerWithNonceAndRealm = "nonce=" <> quote nonce <> ",realm=" <> quote realm
headerWithNonceAndRealm2 =
    "SOMEDATA,realm="
        <> quote realm
        <> ",SOMEOTHERDATA,nonce="
        <> quote nonce
garbageHeader = "nothing_of_value"
onlyNonce = "nonce=" <> quote nonce
onlyRealm = "realm=" <> quote realm

quote :: (Semigroup a, IsString a) => a -> a
quote string = "\"" <> string <> "\""

main :: IO ()
main = hspec $ do
    describe "extractNonceAndRealm" $ do
        it "Returns the nonce and realm if the header contains only nonce and realm" $ do
            extractNonceAndRealm headerWithNonceAndRealm `shouldBe` Right (realm, nonce)

        it "Returns the nonce and realm if the header nonce and realm mixed with other data" $ do
            extractNonceAndRealm headerWithNonceAndRealm `shouldBe` Right (realm, nonce)

        it "Returns error when given neither nonce nor realm" $ do
            extractNonceAndRealm garbageHeader `shouldSatisfy` (\r -> isLeft r)

        it "Returns error when given only nonce" $ do
            extractNonceAndRealm onlyNonce `shouldSatisfy` (\r -> isLeft r)

        it "Returns error when given only realm" $ do
            extractNonceAndRealm onlyRealm `shouldSatisfy` (\r -> isLeft r)
