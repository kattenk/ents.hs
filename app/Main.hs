module Main (main) where
import LambdaGame
import Control.Concurrent (threadDelay)

newtype Counter = Counter {count :: Int}

countUp :: Counter -> Counter
countUp c =
  Counter { count = count c + 1}

stopAfter :: Counter -> GameLoop
stopAfter c =
  if count c >= 10 then
    GameLoop { loopRate = 60, exit = True }
  else
    GameLoop { loopRate = 60, exit = False }

printCount :: Counter -> Scene ()
printCount c = do
  liftIO $ putStrLn $ "the count is " ++ show (count c) ++ ".."

main :: IO ()
main = do
  runGame $ do
    spawn (Counter 0)
    gameLoop $ do
      system countUp
      system printCount
      system stopAfter