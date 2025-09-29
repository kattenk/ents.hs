{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TypeFamilies, ConstraintKinds #-}
{-# LANGUAGE TypeApplications #-}
module Main (main) where
import Data.Char (toUpper)
import Debug.Trace (trace)
import Data.Typeable (Typeable, cast)
import LambdaGame
import qualified Data.Map as Map
import Data.Data (Proxy(..))

data Position = Position Int Int deriving Show

testAction :: Scene ()
testAction = do
  spawn (32 :: Int)
        (Position 2 3)
        "Hello i am one"
  
  spawn (32 :: Int)
        "Hello i am two"
        (Position 6 4)
  
  spawn (32 :: Int)
        "Hello i am three"
        (Position 6 4)

  val <- get (0, Proxy @Int)

  case val of
    Nothing -> liftIO $ putStrLn "was nothing"
    (Just v) -> liftIO $ putStrLn $ "it is " ++ show v


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