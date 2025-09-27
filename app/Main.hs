{-# LANGUAGE TypeApplications #-}
module Main (main) where
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import LambdaGame
import Data.Data (Proxy (Proxy))
import Control.Monad.State.Strict hiding (get)

initialState = LambdaGame.SceneState
  { resources = Map.empty,
    components = Map.empty,
    entityCount = 0,
    growComponents = return (),
    reusableIndices = [],
    currentEntity = 0
  }

testAction :: Scene ()
testAction = do
  spawnWithComponent (7 :: Int)
  set (21 :: Int)
  nthVal <- get (5 :: Int)
  liftIO $ print nthVal
  return ()

main = do
  runScene initialState testAction