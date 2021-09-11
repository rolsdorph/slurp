module CollectorSpec where

import Collector
import Test.Hspec (Spec, describe, hspec, it, shouldBe, shouldSatisfy)
import System.Log.Logger (infoM, errorM)

loggerName = "Collector tests"

main :: IO ()
main = hspec spec

baseEnv =
  Env
    { envGetAllUsers = return [],
      envGetUserSs = const $ return [],
      envGetUserHomes = const $ return [],
      envCollectHome = const . return $ Left "Not implemented",
      envCollectSs = const . return $ Left "Not implemented",
      envLogInfo = infoM loggerName,
      envLogError = errorM loggerName,
      envPublishNotification = const $ return (),
      envPublishData = const $ return ()
    }

spec :: Spec
spec = do
  describe "Collector" $ do
    it "Collects things" $ do
      putStrLn "hello"
