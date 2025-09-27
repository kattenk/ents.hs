{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TypeFamilies, ConstraintKinds #-}
module Main (main) where
import Data.Char (toUpper)
import Debug.Trace (trace)
import Data.Typeable (Typeable, cast)
import LambdaGame
import qualified Data.Map as Map

testAction :: Scene ()
testAction = do
  liftIO $ putStrLn "Hi"
  spawn (5 :: Int) "" (5 :: Int) (5 :: Int) (5 :: Int)
  liftIO $ putStrLn "Hi"


initialState = LambdaGame.SceneState
  { resources = Map.empty,
    components = Map.empty,
    entityCount = 0,
    growComponents = return (),
    reusableIndices = [],
    currentEntity = 0
  }

main :: IO ()
main = do
    putStrLn "Hello"
    runScene initialState testAction
    putStrLn "Hello"