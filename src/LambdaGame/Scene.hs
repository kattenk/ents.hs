{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE RankNTypes #-}

-- {-# LANGUAGE FlexibleContexts #-}

module LambdaGame.Scene (
  Scene, SceneState(..), runScene,
  currentEnt, setResource, getResource,
  getComponentVector
) where

import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import Data.Dynamic (Dynamic, toDyn, fromDynamic)
import Data.Proxy (Proxy (Proxy))
import Data.Typeable (Typeable, typeRep, typeOf, TypeRep)
import Control.Monad.State.Strict hiding (get)
import Data.Vector.Mutable (IOVector)
import qualified Data.Vector.Mutable as Vector
import Control.Monad.Trans.Maybe

-- | The 'Scene' Monad
type Scene = StateT SceneState IO

data SceneState = SceneState
  { resources :: Map TypeRep Dynamic,  -- ^ Dynamic is the Resource itself
    components :: Map TypeRep Dynamic, -- ^ Dynamic is 'IOVector (Maybe a)' for each Component a
    nextEntityIndex :: Int,            -- ^ the next entity index
    entitySlots :: Int,                -- ^ how many slots are there for entities
    recycleEntityIndices :: [Int],     -- ^ IDs of entities that have been despawned
    currentEntity :: Int               -- ^ the ID of the entity a System is running on
  }

-- | Run a 'Scene' action, provided with an initial state
runScene :: SceneState -> Scene a -> IO (a, SceneState)
runScene initialState scene = runStateT scene initialState

-- | Gets the current entity, this is automatically set by
-- the system runner to the entity the system is running on
currentEnt :: Scene Int
currentEnt = gets currentEntity

-- | Sets a Resource
setResource :: Typeable r => r -> Scene ()
setResource r =
  modify $ \ecs -> ecs { resources = Map.insert (typeOf r) (toDyn r) (resources ecs) }

-- | Gets a Resource based on what you expect from it
-- e.g to get 'Time' you could do 'time <- getResource :: Scene (Maybe Time)'
getResource :: forall a. Typeable a => Scene (Maybe a)
getResource = do
  theResources <- gets resources
  case Map.lookup (typeRep (Proxy :: Proxy a)) theResources of
    (Just dyn) -> return $ fromDynamic dyn
    Nothing -> return Nothing

class ComponentAccess target where
  get :: target -> Scene (Maybe a)
  set :: target -> Scene ()
  -- has :: target -> Scene Bool  
  -- remove :: target -> Scene ()

getComponentVector ::
  forall a.
  (Typeable a) =>
  a ->                    -- ^ The component to look-up
  Bool ->                 -- ^ Try to create the vector if it doesn't exist?
  Scene (Maybe (IOVector (Maybe a)))
getComponentVector component shouldCreate = do
  storage <- gets components

  case Map.lookup (typeOf component) storage of
    (Just dyn) -> case fromDynamic dyn :: Maybe (IOVector (Maybe a)) of
      (Just componentVec) -> return (Just componentVec)
      Nothing -> return Nothing
    Nothing -> if shouldCreate then do
                    slots <- gets entitySlots
                    vec <- liftIO $ Vector.replicate slots (Nothing :: Maybe a)
                    modify $ \ecs -> ecs { components = Map.insert (typeOf component)
                                                                   (toDyn vec)
                                                                   (components ecs) }
                    return (Just vec)
                    else return Nothing
                                 

instance {-# OVERLAPPABLE #-} Typeable a => ComponentAccess a where
  get a = do
    cur <- currentEnt
    get (cur, a)

  set a = do
    cur <- currentEnt
    set (cur, a)

instance (Typeable a) => ComponentAccess (Int, a) where
  get (i, a) = return Nothing
  set (i, x) = return ()


-- -- | Sets a component for an entity
-- setComponentOf :: Typeable a => Int -> a -> Scene ()
-- setComponentOf ent component = do
--     return ()