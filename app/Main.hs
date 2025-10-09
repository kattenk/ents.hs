{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE TypeApplications #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Use newtype instead of data" #-}

module Main (main) where
import Data.Char (toUpper)
import Debug.Trace (trace)
import Data.Typeable (Typeable, cast, typeRep)
import LambdaGame
import Data.Data (Proxy(..))

data Counter = Counter { count :: Int }

countUp :: Counter -> Counter
countUp c =
  Counter { count = count c + 1}

stopAfter :: Counter -> ExitGame
stopAfter c =
  if count c >= 10 then
    ExitGame True
  else
    ExitGame False

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