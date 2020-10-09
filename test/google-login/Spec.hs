{-# LANGUAGE OverloadedStrings #-}

import           GoogleLogin

import           Control.Monad.Except
import           Control.Lens
import           Data.Either
import           Data.Maybe
import           Test.Hspec
import           Crypto.JOSE.JWK
import           Crypto.JWT
import qualified Data.Text                     as T
import           Data.List
import           Data.Time.Clock

googleKid1 = "2c6fa6f5950a7ce465fcf247aa0b094828ac952c"
googleKid2 = "5effa76ef33ecb5e346bd512d7d89b30e47d8e98"

myClientId = "my-legit-client-id"
notMyClientId = "some-evil-client-id"

{-# ANN module ("HLint: ignore Redundant do" :: String) #-}
main :: IO ()
main = hspec $ do
    describe "Google Auth Module" $ do
        it "Loads two Google certs from disk" $ do
            maybeKeys <- loadKeys "resources/certs.json"
            maybeKeys `shouldSatisfy` isJust
            case maybeKeys of
                (Just (JWKSet keys)) -> do
                    length keys `shouldBe` 2
                    hasKey googleKid1 keys `shouldBe` True
                    hasKey googleKid2 keys `shouldBe` True

                _ -> fail "Should never get here"

        it "Verifies the JWT against a set of criteria" $ do
            maybePubKeyA <- loadKey "test/google-login/resources/key-a.pub.json"
            maybePrivKeyA <- loadKey
                "test/google-login/resources/key-a.priv.json"
            maybePubKeyB <- loadKey "test/google-login/resources/key-b.pub.json"

            now          <- getCurrentTime
            let inTenMinutes = addUTCTime (secondsToNominalDiffTime 600) now
            let fiveMinutesAgo =
                    addUTCTime (secondsToNominalDiffTime (-300)) now

            case (maybePrivKeyA, maybePubKeyA, maybePubKeyB) of
                (Just privA, Just pubA, Just pubB) -> do
                    validToken1 <- createToken privA 
                                              myClientId 
                                              googleIssuer1
                                              inTenMinutes

                    validToken2 <- createToken privA 
                                              myClientId 
                                              googleIssuer2
                                              inTenMinutes

                    wrongIssuerToken <- createToken privA
                                                    myClientId
                                                    "EvilCorp inc."
                                                    inTenMinutes

                    wrongAudienceToken <- createToken privA
                                                      notMyClientId
                                                      googleIssuer1
                                                      inTenMinutes

                    expiredToken <- createToken privA
                                                myClientId
                                                googleIssuer1
                                                fiveMinutesAgo

                    case
                            ( validToken1
                            , validToken2
                            , wrongIssuerToken
                            , wrongAudienceToken
                            , expiredToken
                            )
                        of
                            (Right valid1, Right valid2, Right wrongIss, Right wrongAud, Right expired)
                                -> do
                                    -- The valid tokens should pass validation
                                    validRes1 <- verifyToken (JWKSet [pubA])
                                                             myClientId
                                                             valid1
                                    isRight validRes1 `shouldBe` True

                                    validRes2 <- verifyToken (JWKSet [pubA])
                                                             myClientId
                                                             valid2
                                    isRight validRes2 `shouldBe` True

                                    -- The wrong-issuer should not pass validation
                                    wrongIssRes <- verifyToken
                                        (JWKSet [pubA])
                                        myClientId
                                        wrongIss
                                    isRight wrongIssRes `shouldBe` False

                                    -- The wrong-audience should not pass validation
                                    wrongAudRes <- verifyToken
                                        (JWKSet [pubA])
                                        myClientId
                                        wrongAud
                                    isRight wrongAudRes `shouldBe` False

                                    -- The expired token should not pass validation
                                    expiredRes <- verifyToken
                                        (JWKSet [pubA])
                                        myClientId
                                        expired
                                    isRight expiredRes `shouldBe` False

                                    -- A token signed by org B should not pass validation
                                    orgBRes <- verifyToken (JWKSet [pubB]) myClientId valid1
                                    isRight orgBRes `shouldBe` False
                            _ -> fail "Failed to create test tokens"

                _ -> fail "Failed to read key resources"


-- Checks whether the set contains a JWK with the given kid
hasKey :: T.Text -> [JWK] -> Bool
hasKey kid keys = isJust $ find (\k -> (k ^. jwkKid) == Just kid) keys

-- Signs a JWT with the given key, aud, iss and expiration
createToken
    :: JWK
    -> StringOrURI
    -> StringOrURI
    -> UTCTime
    -> IO (Either JWTError SignedJWT)
createToken key aud iss expiration = do
    claims <- buildClaims aud iss expiration
    signWithClaims key claims

-- Signs a JWT with the given claims
signWithClaims :: JWK -> ClaimsSet -> IO (Either JWTError SignedJWT)
signWithClaims key claims = runExceptT $ do
    alg <- bestJWSAlg key
    signClaims key (newJWSHeader ((), alg)) claims

buildClaims :: StringOrURI -> StringOrURI -> UTCTime -> IO ClaimsSet
buildClaims aud iss expiration = do
    now <- getCurrentTime
    pure
        $  emptyClaimsSet
        &  claimAud ?~ Audience [aud]
        &  claimIss ?~ iss
        &  claimIat ?~ NumericDate now
        &  claimExp ?~ NumericDate expiration

