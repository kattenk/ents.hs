module Main (main) where
import LambdaGame

main :: IO ()
main = do
  runGame $ do
    gameLoop $ do
      return ()