{-# LANGUAGE ScopedTypeVariables, FlexibleInstances, TypeFamilies, TypeOperators,
             TypeApplications, UndecidableInstances, FlexibleContexts, MultiParamTypeClasses #-}

module LambdaGame.Scene (
  Scene, SceneState(..), runScene,
  currentEnt, resource, isResource,
  ComponentAccess(..), ReturnType,
  Spawn(..), Despawn(..), getComponentVec
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

-- TODO: Reuseable indices

-- | The 'Scene' Monad
type Scene = StateT SceneState IO

data SceneState = SceneState {
  resources       :: Map TypeRep Dynamic, -- ^ Dynamic is the Resource itself
  components      :: Map TypeRep Dynamic, -- ^ Dynamic is 'IOVector (Maybe a)' for each Component a
  entityCount     :: Int,                 -- ^ Entity count
  growComponents  :: Scene (),            -- ^ An action for growing all of the component vectors
  clearEntity     :: Int -> Scene (),     -- ^ An action for setting an index to Nothing in all component vectors
  reusableIndices :: [Int],               -- ^ Reusable indices in the vectors
  currentEntity   :: Int                  -- ^ The entity a System is running on
}

-- | Run a 'Scene' action, provided with an initial state
runScene :: SceneState -> Scene a -> IO (a, SceneState)
runScene initialState scene = runStateT scene initialState

-- | Get the current entity, this is automatically set by
-- the system runner to the entity the system is running on
currentEnt :: Scene Int
currentEnt = gets currentEntity

-- | Something we can get a value-level type representation from,
-- this class exists so Proxies can also be used if a value of a
-- given component type is not available, or a 'Proxy' is otherwise preferable
class Typeable a => Rep a where
  rep :: a -> TypeRep

instance {-# INCOHERENT #-} Typeable a => Rep a where
  rep = typeOf

instance {-# OVERLAPS #-} Typeable a => Rep (Proxy a) where
  rep _ = typeRep (Proxy @a)

-- | Sets a Resource
resource :: Typeable r => r -> Scene ()
resource r =
  modify $ \ecs -> ecs { resources = Map.insert (rep r) (toDyn r) (resources ecs) }

-- | Internal function for fetching a resource
getResource :: forall r. Rep r => Scene (Maybe r)
getResource = do
  storage <- gets resources
  return $ Map.lookup (rep (Proxy @r)) storage >>= fromDynamic

isResource :: forall a. Typeable a => Proxy a -> Scene Bool
isResource _ = do
  res <- getResource @a
  return $ isJust res

-- | Function for retrieving a component vector from storage,
-- for reading or writing to it.
getComponentVec :: forall a. Rep a => Scene (Maybe (IOVector (Maybe a)))
getComponentVec = do
  storage <- gets components
  return $ Map.lookup (rep (Proxy @a)) storage >>= fromDynamic

class ComponentAccess t where
  get :: t -> Scene (Maybe (ReturnType t)) -- ^ Get a component
  set :: t -> Scene ()                     -- ^ Set a component
  has :: t -> Scene Bool                   -- ^ Check if an entity has a component
  remove :: t -> Scene ()                  -- ^ Remove a component from an entity

-- used to establish that the return type of 'get'
-- is gathered from the input type, even though _how_ it is gathered varies
type family ReturnType t where
  ReturnType (i, a) = ReturnType a
  ReturnType (Proxy a) = a
  ReturnType a = a

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
instance {-# OVERLAPPING #-} (Rep a, Rep (ReturnType a), Integral i) => ComponentAccess (i, a) where
  get (i, _) = do
    componentVec <- getComponentVec

    maybe (getResource @(ReturnType a))
          (`Vector.read` fromIntegral i)
          componentVec

  set (i, c) = do
    isRes <- isResource (Proxy @a)
    if isRes then do
      resource c
    else do
      componentVec <- getComponentVec
      case componentVec of
        Nothing -> do
          _ <- makeComponentVector c
          set c
        (Just vec) -> Vector.write vec (fromIntegral i) (Just c)

  has (i, _) = do
    componentVec <- getComponentVec

    component <- maybe (getResource @(ReturnType a))
                       (`Vector.read` fromIntegral i)
                       componentVec

    return $ isJust component

  remove (i, _) = do
    componentVec <- getComponentVec @(ReturnType a)
    case componentVec of
      Nothing -> return ()
      (Just vec) -> do liftIO $ Vector.write vec (fromIntegral i) Nothing


-- returns an action for expanding some component's vector
makeVectorGrower :: forall c. Rep c => c -> Scene ()
makeVectorGrower a = do
  maybeVec <- getComponentVec @c
  case maybeVec of
    Nothing -> do return ()
    (Just vecToGrow) -> do
      grownVec <- Vector.grow vecToGrow 1
      Vector.write grownVec (Vector.length grownVec - 1) Nothing

      modify $ \scn ->
        scn { components = Map.insert (rep a)
                                      (toDyn grownVec)
                                      (components scn) }

-- returns a function for setting an index in a component vector to Nothing
makeClearFunction :: forall c. Rep c => c -> Int -> Scene ()
makeClearFunction _ i = do
  maybeVec <- getComponentVec @c
  case maybeVec of
    (Just vec) -> Vector.write vec i Nothing
    Nothing -> return ()

makeComponentVector :: forall c. Rep c => c -> Scene (IOVector (Maybe c))
makeComponentVector a = do
  slots <- gets entityCount
  vec <- Vector.replicate slots (Nothing :: Maybe a)

  modify $ \scnState -> scnState
    { components = Map.insert (rep a)
                              (toDyn vec)
                              (components scnState) }

  -- modify the growComponents action to be itself joined with
  -- a new action that grows the new vector
  modify $ \scnState -> scnState
    { growComponents = growComponents scnState >> makeVectorGrower a}

  -- similar thing with clearing
  clearEntityFn <- gets clearEntity
  modify $ \scnState -> scnState
    { clearEntity =
        \i -> do clearEntityFn i
                 makeClearFunction a i}

  return vec

-- takes an entity index and returns an action
-- that adds some component to that entity
type ComponentAdder = (Int -> Scene ())

makeComponentAdder :: forall c. Rep c => c -> ComponentAdder
makeComponentAdder a index = do
  maybeComponentVec <- getComponentVec @c

  componentVec <- case maybeComponentVec of
    (Just vec) -> return vec
    Nothing -> makeComponentVector a

  Vector.write componentVec index (Just a)

spawn' :: [ComponentAdder] -> Scene ()
spawn' adders = do
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
      gets growComponents >>= (>> return ec)

  mapM_ ($ chosenIndex) adders

class Spawn a r where
  -- | Spawns a new entity, takes any number of arguments,
  -- each of which is a component to add to the new entity
  spawn :: a -> r

instance (a ~ ()) => Spawn [ComponentAdder] (Scene a) where
  spawn = spawn'

-- only one argument
instance {-# OVERLAPS #-} (a ~ (), Typeable b) => Spawn b (Scene a) where
  spawn a = spawn' [makeComponentAdder a]

-- recursive case
instance (Typeable a, Spawn [ComponentAdder] r) => Spawn [ComponentAdder] (a -> r) where
  spawn x y = spawn (x ++ [makeComponentAdder y])

-- the next type is the same
instance {-# OVERLAPPABLE #-} (Typeable a, Spawn [ComponentAdder] r) => Spawn a (a -> r) where
  spawn x y = spawn $ makeComponentAdder x : [makeComponentAdder y]

-- the next type is different
instance {-# OVERLAPPABLE #-} (Typeable a, Typeable b, Spawn [ComponentAdder] r) => Spawn a (b -> r) where
  spawn x y = spawn $ makeComponentAdder x : [makeComponentAdder y]

class Despawn e where
  -- | Despawn an entity
  despawn :: e

despawn' :: Int -> Scene ()
despawn' i = do
  clearFn <- gets clearEntity
  clearFn i

  -- causes strange freaking issues man, we'll just leave this out
  -- modify $ \scn ->
  --   scn { reusableIndices = i : reusableIndices scn }

-- | Despawn the current entity
instance {-# OVERLAPS #-} a ~ () => Despawn (Scene a) where
  despawn = do
    cur <- currentEnt
    despawn' cur

-- | Despawn a specific entity
instance {-# OVERLAPPABLE #-} a ~ () => Despawn (Int -> Scene a) where
  despawn x = do
    despawn' x