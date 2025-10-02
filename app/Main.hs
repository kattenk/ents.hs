{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE TypeApplications #-}

module Main (main) where
import Data.Char (toUpper)
import Debug.Trace (trace)
import Data.Typeable (Typeable, cast, typeRep)
import LambdaGame
import qualified Data.Map as Map
import Data.Data (Proxy(..))

data Position = Position Int Int

testSystem :: Int -> String -> Scene Int
testSystem int string = do
  liftIO $ putStrLn $ "The int is " ++ show int ++ ", the string is " ++ string
  set "New string"
  return 5360115

testAction :: Scene ()
testAction = do
  spawn (76 :: Int)
        (Position 2 3)
        "Hello i am one"

  spawn (32 :: Int)
        "Blah"

  spawn (32 :: Int)
        "Hello i am three"
        (Position 6 4)

  fil <- ($ 2) $ getFilter testSystem

  system testSystem
  
  val <- get (1, Proxy @[Char])

  case val of
    Nothing -> liftIO $ putStrLn "was nothing"
    (Just v) -> liftIO $ putStrLn $ "it is " ++ show v

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