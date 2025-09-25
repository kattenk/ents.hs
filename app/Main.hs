module Main (main) where
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import LambdaGame

initialState = LambdaGame.SceneState
  { resources = Map.empty,
    components = Map.empty,
    nextEntityIndex = 0,
    entitySlots = 20, 
    recycleEntityIndices = [],
    currentEntity = 0
  }

-- testAction :: Scene ()
-- testAction = do
--   veca <- getComponentVector (Just 0) True
--   vec <- getComponentVector (Just 0) False
--   case vec of
--     (Just there) -> liftIO $ putStrLn "Hello! LambdaGame!"
--     Nothing -> liftIO $ putStrLn "Not there"

main = do
  runScene initialState testAction