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
  spawnWithComponent (21 :: Int)
  spawnWithComponent (31 :: Int)
  spawnWithComponent "hi"
  spawnWithComponent "metoo"
  set (0, "Waow")
  doesHas <- has (3, 4 :: Int)
  nthVal <- get (0, Proxy :: Proxy String)
  liftIO $ print nthVal
  return ()

main = do
  runScene initialState testAction