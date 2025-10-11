module Main (main) where
import LambdaGame

newtype Counter = Counter {count :: Int}

countUp :: Counter -> Counter
countUp c =
  Counter { count = count c + 1}

stopAfter :: Counter -> Window -> Window
stopAfter c win =
  if count c >= 10 then
    win { exit = True}
  else
    win { exit = False}

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