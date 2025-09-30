{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE UndecidableInstances #-}

module Main (main) where
import Data.Char (toUpper)
import Debug.Trace (trace)
import Data.Typeable (Typeable, cast, typeRep)
import LambdaGame
import qualified Data.Map as Map
import Data.Data (Proxy(..))

data Position = Position Int Int

testSystem :: Int -> String -> Int -> Int -> Scene Int
testSystem x y z f = do
  liftIO $ putStrLn "Yay! :D"
  return 2500

testAction :: Scene ()
testAction = do
  spawn (76 :: Int)
        (Position 2 3)
        "Hello i am one"

  spawn (32 :: Int)

  spawn (32 :: Int)
        "Hello i am three"
        (Position 6 4)

  fil <- ($ 2) $ getFilter testSystem

  runSystem testSystem
  liftIO $ putStrLn $ "the result is " ++ show fil

  return ()

initialState = LambdaGame.SceneState
  { resources = Map.empty,
    components = Map.empty,
    entityCount = 0,
    growComponents = return (),
    clearEntity = \i -> return (),
    reusableIndices = [],
    currentEntity = 0
  }

main :: IO ()
main = do
  runScene initialState testAction
  return ()