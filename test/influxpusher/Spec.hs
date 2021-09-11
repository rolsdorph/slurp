import InfluxPublish (InfluxPushResult (..))
import InfluxPusher
import System.Log.Logger (errorM, infoM, warningM)
import Test.Hspec (Spec, describe, hspec, it)

loggerName :: String
loggerName = "InfluxPusher tests"

main :: IO ()
main = hspec spec

baseEnv :: Env
baseEnv =
  Env
    { envGetUserSinks = const (return []),
      envInfluxPush = \_ _ -> return Success,
      envLogInfo = infoM loggerName,
      envLogWarn = warningM loggerName,
      envLogError = errorM loggerName,
      envPublishNotification = const $ return (),
      envConsumerRegistry = const $ return ()
    }

spec :: Spec
spec = do
  describe "InfluxPusher" $ do
    it "Pushes things" $ do
      putStrLn "hello"
