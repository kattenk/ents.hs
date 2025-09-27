{-# LANGUAGE ScopedTypeVariables, FlexibleInstances, TypeFamilies,
             TypeApplications, UndecidableInstances, FlexibleContexts #-}
{-# LANGUAGE BlockArguments #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Use join" #-}

module LambdaGame.Scene (
  Scene, SceneState(..), runScene,
  currentEnt, setResource, getResource,
  Rep,
  ComponentAccess(..),
  SpawnWithComponent(..)
) where

import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import Data.Dynamic (Dynamic, toDyn, fromDynamic)
import Data.Proxy (Proxy (Proxy))
import Data.Typeable (Typeable, typeRep, typeOf, TypeRep)
import Control.Monad.State.Strict hiding (get)
import qualified Control.Monad.State.Strict as State
import Data.Vector.Mutable (IOVector)
import qualified Data.Vector.Mutable as Vector
import Data.Maybe (isJust)

-- data Ent = Ent { index :: Int, generation :: Int }

-- | The 'Scene' Monad
type Scene = StateT SceneState IO

data SceneState = SceneState
  { resources            :: Map TypeRep Dynamic, -- ^ Dynamic is the Resource itself
    components           :: Map TypeRep Dynamic, -- ^ Dynamic is 'IOVector (Maybe a)' for each Component a
    entityCount          :: Int,                 -- ^ Entity count
    growComponents       :: Scene (),            -- ^ An action for growing all of the component vectors
    reusableIndices      :: [Int],               -- ^ Reusable indices in the vectors
    currentEntity        :: Int                  -- ^ The entity a System is running on
  }

-- | Run a 'Scene' action, provided with an initial state
runScene :: SceneState -> Scene a -> IO (a, SceneState)
runScene initialState scene = runStateT scene initialState

-- | Get the current entity, this is automatically set by
-- the system runner to the entity the system is running on
currentEnt :: Scene Int
currentEnt = gets currentEntity

-- | Something we can get a value-level type representation from,
-- this class mostly exists so Proxies can also be used if a value
-- of a given component type is not available, or a 'Proxy' is otherwise preferable
class Typeable a => Rep a where
  rep :: a -> TypeRep

instance Typeable a => Rep a where
  rep = typeOf

instance {-# OVERLAPPING  #-} Typeable a => Rep (Proxy a) where
  rep _ = typeRep (Proxy @a)

-- | Sets a Resource
setResource :: Rep r => r -> Scene ()
setResource r =
  modify $ \ecs -> ecs { resources = Map.insert (rep r) (toDyn r) (resources ecs) }

-- | Internal function for getting something from one of the Maps
fromStorage :: forall a. Rep a => (SceneState -> Map TypeRep Dynamic) -> Scene (Maybe a)
fromStorage f = do
  storage <- gets f
  return $ Map.lookup (rep (Proxy @a)) storage >>= fromDynamic

-- | Gets a Resource based on what you expect from it
-- e.g to get 'Time' you could do 'time <- getResource :: Scene (Maybe Time)'
getResource :: forall r. Rep r => Scene (Maybe r)
getResource = fromStorage resources

-- | Internal-only function for retrieving a component vector from storage,
-- for reading or writing to it.
getComponentVec :: forall a. Rep a => Scene (Maybe (IOVector (Maybe a)))
getComponentVec = fromStorage components

type family ReturnType t where
  ReturnType (Int, a) = ReturnType a
  ReturnType (Proxy a) = a
  ReturnType a = a

class ComponentAccess t where
  get :: t -> Scene (Maybe (ReturnType t)) -- ^ Get a component
  set :: t -> Scene ()                     -- ^ Set a component
  has :: t -> Scene Bool                   -- ^ Check if an entity has a component
  remove :: t -> Scene ()                  -- ^ Remove a component from an entity

-- | Access the current entity's components
instance (Rep a, Rep (ReturnType a)) => ComponentAccess a where
  get a = do
    cur <- currentEnt
    get (cur, a)

  set a = do
    cur <- currentEnt
    set (cur, a)

  has a = do
    cur <- currentEnt
    has (cur, a)

  remove a = do
    cur <- currentEnt
    remove (cur, a)

-- | Access a specific entity's components
instance {-# OVERLAPPING #-} (Rep a, Rep (ReturnType a)) => ComponentAccess (Int, a) where
  get (i, _) = do
    componentVec <- getComponentVec

    maybe (return Nothing)
          (`Vector.read` i)
          componentVec

  set (i, c) = do
    componentVec <- getComponentVec @a
    case componentVec of
      (Just vec) -> liftIO $ Vector.write vec i (Just c)
      Nothing -> do
        liftIO $ putStrLn $ "no component vec" ++ show (rep c)
        pure ()

  has (i, c) = do
    -- May be a bug with proxies and @a instead of ReturnType a?
    componentVec <- getComponentVec @a
    case componentVec of
      Nothing -> return False
      (Just vec) -> do val <- liftIO $ Vector.read vec i
                       return $ isJust val

  remove (i, c) = do
    componentVec <- getComponentVec @a
    case componentVec of
      Nothing -> return ()
      (Just vec) -> do
        liftIO $ Vector.write vec i Nothing

class (Rep c, Show c) => SpawnWithComponent c where
  spawnWithComponent :: c -> Scene ()

instance (Rep c, Show c) => SpawnWithComponent c where
  spawnWithComponent a = do
    reuseableIndices <- gets reusableIndices
    scnState <- State.get

    chosenIndex <- case reuseableIndices of
      (i:is) -> do
        -- this index is no longer re-useable
        put $ scnState { reusableIndices = is}
        return i
      [] -> do
        ec <- gets entityCount
        put $ scnState { entityCount = ec + 1 }

        -- grow all of the existing component vectors to fit the new entity
        grow <- gets growComponents
        grow
        return $ ec

    -- per component
    maybeComponentVec <- getComponentVec @c
    componentVec <- case maybeComponentVec of
      (Just vec) -> return vec
      Nothing -> do
        slots <- gets entityCount
        vec <- liftIO $ Vector.replicate slots (Nothing :: Maybe a)
        liftIO $ putStrLn $ "creating new vector of size " ++ show slots ++ "to store " ++ show (typeOf a)
        modify $ \ecs -> ecs { components = Map.insert (rep a)
                                                       (toDyn vec)
                                                       (components ecs) }

        modify $ \ecs -> ecs
          { growComponents = growComponents ecs >>
              do mv <- getComponentVec @c
                 case mv of
                   Nothing -> return ()
                   (Just vec) -> do _ <- liftIO $ Vector.grow vec 1
                                    return () }

        return vec

    liftIO $ putStrLn $ "writing into index " ++ show chosenIndex
    liftIO $ Vector.write componentVec chosenIndex (Just a)

    mcv <- gets components
    let x = Map.lookup (rep (Proxy @c)) mcv
    p <- case x of
      (Just y) -> do
        let z = fromDynamic y :: Maybe (IOVector (Maybe c))
        liftIO $ putStrLn $ "z is " ++ show (isJust z)
        case z of
          (Just ah) -> do liftIO $ Vector.read ah 0
          Nothing -> return Nothing
      Nothing -> return Nothing
    
    case p of
      (Just ji) -> do
        liftIO $ putStrLn $ "p is " ++ show ji
        return ()
      Nothing -> return ()

    return ()

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