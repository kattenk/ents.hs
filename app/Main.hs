{-# LANGUAGE TypeApplications #-}
module Main (main) where
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import LambdaGame
import Data.Data (Proxy (Proxy))

initialState = LambdaGame.SceneState
  { resources = Map.empty,
    components = Map.empty,
    recycleEntityIndices = [],
    currentEntity = 0
  }

testAction :: Scene ()
testAction = do
  id <- currentEnt

  x <- get (Proxy :: Proxy Int)

  return ()

main = do
  runScene initialState testAction