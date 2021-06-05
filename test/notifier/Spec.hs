import qualified Notifier

main :: IO ()
main = do
  Notifier.run undefined undefined
  putStrLn "Test suite not yet implemented"
