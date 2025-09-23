{-# LANGUAGE ScopedTypeVariables #-}

module LambdaGame.Scene (
    Scene, SceneState(..), runScene,
    currentEnt, setResource, getResource
) where

import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import Data.Dynamic (Dynamic, toDyn, fromDynamic)
import Data.Proxy (Proxy (Proxy))
import Data.Typeable (Typeable, typeRep, typeOf, TypeRep)
import Control.Monad.State.Strict

type Scene = StateT SceneState IO

data SceneState = SceneState
  { resources :: Map TypeRep Dynamic
  , currentEntity :: Int }

runScene :: SceneState -> Scene a -> IO (a, SceneState)
runScene initialState scene = runStateT scene initialState

currentEnt :: Scene Int
currentEnt = gets currentEntity

setResource :: Typeable r => r -> Scene ()
setResource r =
    modify $ \ecs -> ecs { resources = Map.insert (typeOf r) (toDyn r) (resources ecs) }

getResource :: forall a. Typeable a => Scene (Maybe a)
getResource = do
    theResources <- gets resources
    case Map.lookup (typeRep (Proxy :: Proxy a)) theResources of
        (Just dyn) -> return $ fromDynamic dyn
        Nothing -> return Nothing