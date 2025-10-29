{-# LANGUAGE ScopedTypeVariables, MultiParamTypeClasses, FlexibleInstances, FlexibleContexts,
             UndecidableInstances, TypeOperators, TypeFamilies, QuantifiedConstraints, AllowAmbiguousTypes #-}
-- |
-- Module      : Ents.Scene
-- License     : MIT
-- 
-- Contains the 'Scene' monad, a state monad
-- which stores components and resources, along with
-- the CRUD actions for them, which are contextualized by
-- the current entity, usually set by the 'system'

module Ents.Scene (
  Scene, SceneState(..), defaultSceneState, runScene, currentEnt,
  resource, getResource, isResource, getComponentVec,
  get, set, has, remove, withEnt,
  Spawn(..), despawn
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
import Control.Monad (when)

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

defaultSceneState :: SceneState
defaultSceneState = SceneState
  { resources = Map.empty,
    components = Map.empty,
    entityCount = 0,
    growComponents = return (),
    clearEntity = \_ -> return (),
    reusableIndices = [],
    currentEntity = -1
  }

-- | Run a 'Scene' action, provided with an initial state
runScene :: SceneState -> Scene a -> IO (a, SceneState)
runScene initialState scene = runStateT scene initialState

-- | Get the current entity, this is automatically set by
-- the system runner to the entity the system is running on
currentEnt :: Scene Int
currentEnt = gets currentEntity

-- | Run with some entity index as the current entity
withEnt :: Int -> Scene a -> Scene a
withEnt i a = do
  before <- currentEnt
  modify $ \s -> s {currentEntity = i}
  res <- a
  modify $ \s -> s {currentEntity = before}
  return res

-- | Set a Resource
resource :: Typeable r => r -> Scene ()
resource r =
  modify (\ecs -> ecs { resources = Map.insert (typeOf r) (toDyn r) (resources ecs) })

-- | Get a resource
getResource :: forall r. Typeable r => Scene (Maybe r)
getResource = do
  storage <- gets resources
  return (Map.lookup (typeRep (Proxy :: Proxy r)) storage >>= fromDynamic)

-- | Is it a resource?
isResource :: forall a. Typeable a => Proxy a -> Scene Bool
isResource _ = do
  res <- getResource :: Scene (Maybe a)
  return (isJust res)

-- | Retrieve a component vector from storage for reading or writing to it
getComponentVec :: forall a. Typeable a => Scene (Maybe (IOVector (Maybe a)))
getComponentVec = do
  storage <- gets components
  return (Map.lookup (typeRep (Proxy :: Proxy a)) storage >>= fromDynamic)

-- | Get a component
get :: Typeable a => Proxy a -> Scene (Maybe a)
get _ = do
  i <- currentEnt
  componentVec <- getComponentVec

  maybe getResource
        (`Vector.read` fromIntegral i)
        componentVec

-- | Set a component
set :: forall a. Typeable a => a -> Scene ()
set a = do
  i <- currentEnt
  isRes <- isResource (Proxy :: Proxy a)

  if isRes then do
    resource a
  else do
    when (i /= -1) $ do
      componentVec <- getComponentVec
      case componentVec of
        Nothing -> do
          -- Hack: For when we are setting a component that hasn't been
          -- added by normal spawning before
          _ <- makeComponentVector a
          set a
        (Just vec) -> Vector.write vec (fromIntegral i) (Just a)

-- | Has a component?
has :: forall a. Typeable a => Proxy a -> Scene Bool
has _ = do
  i <- currentEnt
  componentVec <- getComponentVec :: Scene (Maybe (IOVector (Maybe a)))
  component <- maybe getResource
                     (`Vector.read` fromIntegral i)
                     componentVec

  return $ isJust component

-- | Remove a component
remove :: forall a. Typeable a => Proxy a -> Scene ()
remove _ = do
  i <- currentEnt
  componentVec <- getComponentVec :: Scene (Maybe (IOVector (Maybe a)))
  
  case componentVec of
    Nothing -> return ()
    (Just vec) -> do
      liftIO $ putStrLn $ "Removing............... " ++ show (typeRep (Proxy :: Proxy a))
      liftIO $ Vector.write vec (fromIntegral i) Nothing

-- returns an action for expanding some component's vector
makeVectorGrower :: forall a. Typeable a => a -> Scene ()
makeVectorGrower a = do
  maybeVec <- getComponentVec :: Scene (Maybe (IOVector (Maybe a)))
  case maybeVec of
    Nothing -> do return ()
    (Just vecToGrow) -> do
      grownVec <- Vector.grow vecToGrow 1
      Vector.write grownVec (Vector.length grownVec - 1) Nothing

      modify $ \scn ->
        scn { components = Map.insert (typeOf a)
                                      (toDyn grownVec)
                                      (components scn) }

-- returns a function for setting an index in a component vector to Nothing
makeClearFunction :: forall c. Typeable c => c -> Int -> Scene ()
makeClearFunction _ i = do
  maybeVec <- getComponentVec :: Scene (Maybe (IOVector (Maybe c)))
  case maybeVec of
    (Just vec) -> Vector.write vec i Nothing
    Nothing -> return ()

-- adds a component vector to "components", also returns it
makeComponentVector :: forall c. Typeable c => c -> Scene (IOVector (Maybe c))
makeComponentVector a = do
  slots <- gets entityCount
  vec <- Vector.replicate slots (Nothing :: Maybe a)

  liftIO $ putStrLn $ "adding component vector for " ++ show (typeOf a)

  modify $ \scnState -> scnState
    { components = Map.insert (typeOf a)
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

class MakeComponentAdder a where
  makeComponentAdder :: a -> ComponentAdder

-- regular components
instance {-# OVERLAPPABLE #-} Typeable c => MakeComponentAdder c where
  makeComponentAdder a index = do
    maybeComponentVec <- getComponentVec

    componentVec <- case maybeComponentVec of
      (Just vec) -> return vec
      Nothing -> makeComponentVector a

    Vector.write componentVec index (Just a)

-- Scene actions that produce components
--
-- It might be possible to extend this so that you can have bundles,
-- functions that produce multiple components, but that isn't present here,
-- and this particular instance didn't even end up getting used
-- instance {-# OVERLAPPING #-} Typeable c => MakeComponentAdder (Scene c) where
--   makeComponentAdder a index = do
--     maybeComponentVec <- getComponentVec
--     comp <- a

--     componentVec <- case maybeComponentVec of
--       (Just vec) -> return vec
--       Nothing -> makeComponentVector comp

--     Vector.write componentVec index (Just comp)

spawn' :: [ComponentAdder] -> Scene ()
spawn' adders = do
  reuseableIndices <- gets reusableIndices
  scnState <- State.get

  chosenIndex <- case reuseableIndices of
    (i:is) -> do
      -- this index is no longer re-useable
      put $ scnState { reusableIndices = is }
      liftIO $ putStrLn $ "Re-using index: " ++ show i
      return i
    [] -> do
      ec <- gets entityCount
      liftIO $ putStrLn $ "Growing vector length from " ++ show ec ++ " to " ++ show (ec + 1)
      put $ scnState { entityCount = ec + 1 }

      -- grow all of the existing component vectors to fit the new entity
      gets growComponents >>= (>> return ec)

  mapM_ ($ chosenIndex) adders

-- | This engine uses a variadic function for spawning,
-- tuples could also be used, but I think this makes it unique and is kept in
-- for the fun of it, many people probably don't know you can do things like this
--
-- When you have a method that varies in it's return type between instances,
-- and Haskell sees an excess value, it will pick an instance that can consume
-- that value, over and over again, this makes lots of variadic behavior possible,
-- you can even make a function that takes zero arguments or more arguments,
-- although it would typically require a manual type annotation,
-- using a variadic monadic action inside of a do block is enough to
-- tie the knot and coax the final value out
class Spawn a r where
  -- | Spawns a new entity, takes any number of arguments,
  -- each of which is a component to add to the new entity
  spawn :: a -> r

-- the final value
instance (a ~ ()) => Spawn [ComponentAdder] (Scene a) where
  spawn = spawn'

-- only one argument
instance {-# OVERLAPS #-} (a ~ (), Typeable b, MakeComponentAdder b) => Spawn b (Scene a) where
  spawn a = spawn' [makeComponentAdder a]

-- recursive case
instance (Typeable a, Spawn [ComponentAdder] r, MakeComponentAdder a) => Spawn [ComponentAdder] (a -> r) where
  spawn x y = spawn (x ++ [makeComponentAdder y])

-- the next type is the same
instance {-# OVERLAPPABLE #-} (Typeable a, Spawn [ComponentAdder] r, MakeComponentAdder a) => Spawn a (a -> r) where
  spawn x y = spawn $ makeComponentAdder x : [makeComponentAdder y]

-- the next type is different
instance {-# OVERLAPPABLE #-} (Typeable a, Typeable b, Spawn [ComponentAdder] r, MakeComponentAdder a, MakeComponentAdder b) => Spawn a (b -> r) where
  spawn x y = spawn $ makeComponentAdder x : [makeComponentAdder y]

-- | Despawn the current entity
despawn :: Scene ()
despawn = do
  i <- currentEnt

  modify $ \scn ->
    scn { reusableIndices = i : reusableIndices scn }

  clearFn <- gets clearEntity
  clearFn i