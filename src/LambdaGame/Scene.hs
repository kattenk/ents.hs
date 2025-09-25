{-# LANGUAGE ScopedTypeVariables, FlexibleInstances, TypeFamilies,
             MultiParamTypeClasses, TypeOperators, TypeApplications#-}

module LambdaGame.Scene (
  Scene, SceneState(..), runScene,
  currentEnt, setResource, getResource,
  getComponentVec,
  ComponentAccess(..)
) where

import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import Data.Dynamic (Dynamic, toDyn, fromDynamic)
import Data.Proxy (Proxy (Proxy))
import Data.Typeable (Typeable, typeRep, typeOf, TypeRep)
import Control.Monad.State.Strict hiding (get)
import Data.Vector.Mutable (IOVector)
import qualified Data.Vector.Mutable as Vector

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

type family ReturnType t where
  ReturnType (Int, a) = a
  ReturnType a = a

class ComponentAccess t where
  get :: t -> Scene (Maybe (ReturnType t))
  set :: t -> Scene ()
  -- has :: target -> Scene Bool
  -- remove :: target -> Scene ()

instance (Typeable a, ReturnType a ~ a) => ComponentAccess a where
  get a = do
    cur <- currentEnt
    get (cur, a)

  set a = do
    cur <- currentEnt
    set (cur, a)

instance {-# OVERLAPPING #-} (Typeable a, ReturnType a ~ a) => ComponentAccess (Int, a) where
  get (i, x) = do
    componentVec <- getComponentVec
    maybe (return Nothing)
          (`Vector.read` i)
          componentVec

  set (i, x) = return ()

-- | Internal function for retrieving a component vector from storage,
-- for reading or writing to it.
getComponentVec :: forall a. Typeable a => Scene (Maybe (IOVector (Maybe a)))
getComponentVec = do
  storage <- gets components
  return $ Map.lookup (typeRep (Proxy @a)) storage >>= fromDynamic

-- -- | Internal function for retrieving a component vector from storage,
-- -- for reading or writing to it.
-- getComponentVec :: forall a. (Typeable a) => a -> Bool -> Scene (Maybe (IOVector (Maybe a)))
-- getComponentVec component createIfNonexistent = do
--   storage <- gets components

--   runMaybeT $ do
--     dyn <- MaybeT (pure (Map.lookup (typeOf component) storage))
--     MaybeT (pure (fromDynamic dyn :: Maybe (IOVector (Maybe a))))

-- getComponentVec ::
--   forall a.
--   (Typeable a) =>
--   a ->                    -- ^ The component to look-up
--   Bool ->                 -- ^ Should try to create the vector if it doesn't exist
--   Scene (Maybe (IOVector (Maybe a)))
-- getComponentVec component shouldCreate = do
--   storage <- gets components

--   case Map.lookup (typeOf component) storage of
--     (Just dyn) -> case fromDynamic dyn :: Maybe (IOVector (Maybe a)) of
--       (Just componentVec) -> return (Just componentVec)
--       Nothing -> return Nothing
--     Nothing -> if shouldCreate then do
--                     slots <- gets entitySlots
--                     vec <- liftIO $ Vector.replicate slots (Nothing :: Maybe a)
--                     modify $ \ecs -> ecs { components = Map.insert (typeOf component)
--                                                                    (toDyn vec)
--                                                                    (components ecs) }
--                     return (Just vec)
--                     else return Nothing