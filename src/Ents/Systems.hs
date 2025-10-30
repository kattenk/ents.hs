{-# LANGUAGE AllowAmbiguousTypes, TypeApplications, ScopedTypeVariables,
             FlexibleInstances, UndecidableInstances, FlexibleContexts #-}
-- |
-- Module      : Ents.Systems
-- License     : MIT
-- 
-- Contains 'system', which takes a function
-- and finds all the entities in the 'Scene' that have
-- the components required to run the function, and then
-- runs the function for all of them, it also detects when
-- a parameter is a resource, which are always provided,
-- no matter what. systems that have no parameters or
-- take exclusively resources run only once.
--
-- It also contains 'SystemParam' a type class which can be
-- used to customize how a parameter is interpreted by the system
-- runner, and what data is injected into the function.
--
-- Along with 'SystemResult' which allows you to override what is
-- done with the output of a system

module Ents.Systems (
  SystemParam(..), SystemResult(..), system, Not(..), Every(..),
) where

import Ents.Scene
import Data.Data (Proxy(..))
import Data.Typeable (Typeable)
import Control.Monad (filterM, forM_)
import Control.Monad.State.Strict hiding (get)
import Data.Maybe (catMaybes)

-- | Action for running a System
system :: (Typeable f, SystemFilter f, SystemRunner f) => f -> Scene ()
system f = do
  global <- isGlobal f
  if global then do
    runSystem f
    else do
      entities <- gets entityCount
      entitiesToRunOn <- filterM (getFilter f) [0 .. entities - 1]

      forM_ entitiesToRunOn $ \e -> do
        modify $ \s -> s {currentEntity = e}
        runSystem f

class Typeable p => SystemParam p where
  -- | Should a system with this parameter run on this entity?
  shouldRun :: Int -> Scene Bool

  -- | Provides an argument for this parameter for an entity,
  -- returns Nothing if the parameter cannot be satisfied
  -- (shouldn't happen ever after 'shouldRun' returns True).
  argumentFor :: Int -> Scene (Maybe p)

---
--- System parameters
---

instance {-# OVERLAPPABLE #-} (Typeable a) => SystemParam a where
  shouldRun e =
    withEnt e $ do
      has (Proxy @a)

  argumentFor e =
    withEnt e $ do
      get (Proxy @a)

instance {-# OVERLAPS #-} (Typeable a) => SystemParam (Maybe a) where
  shouldRun _ = return True
  argumentFor e = do
    withEnt e $ do
      component <- get (Proxy @a)
      return (Just component)

data Not a = Not
instance {-# OVERLAPS #-} (Typeable a) => SystemParam (Not a) where
  shouldRun e = do
    withEnt e $ do
      not <$> has (Proxy @a)

  argumentFor _ = return (Just Not)

-- | This is called "Every" but what it really means is
-- "Every component of this type except the current entity's one"
newtype Every a = Every [a]
instance {-# OVERLAPPING #-} (Typeable a) => SystemParam (Every a) where
  shouldRun _ = return True
  argumentFor e = do
    entities <- gets entityCount
    let entitiesToRunOn = filter (/= e) [0 .. (entities - 1)]

    comps <- mapM
      (\eid -> do
        withEnt eid $ do
          get (Proxy @a))
      entitiesToRunOn
    return $ Just (Every (catMaybes comps))

class SystemFilter f where
  -- | Takes a system function and returns a "filter" function,
  -- used to determine if a system should run on an entity
  getFilter :: f -> (Int -> Scene Bool)
  getFilter f i = do
    results <- sequence filters
    return $ and results
    where
      filters = map ($ i) (paramFilters f)

  paramFilters :: f -> [Int -> Scene Bool]
  isGlobal :: f -> Scene Bool

-- | Internal class for running systems, runs a system for the current entity
class SystemRunner f where
  runSystem :: f -> Scene ()

-- | Case where we have an argument yet to supply
instance {-# OVERLAPPING #-} (SystemParam a, SystemRunner b) => SystemRunner (a -> b) where
  runSystem f = do
    cur <- currentEnt
    argument <- argumentFor @a cur
    case argument of
      (Just arg) -> do
        runSystem $ f arg
      
      -- This should never happen
      Nothing -> liftIO $ putStrLn "Couldn't provide argument"

-- | Case where there are no parameters left and we have a result
instance (SystemResult a) => SystemRunner a where
  runSystem a = do
    handleResult a

class SystemResult r where
  handleResult :: r -> Scene ()

instance {-# OVERLAPPABLE #-} (Typeable a) => SystemResult a where
  handleResult a = do
    set a

instance {-# OVERLAPPABLE #-} SystemResult () where
  handleResult _ = return ()

instance {-# OVERLAPPABLE #-} (Typeable a, SystemResult a) =>
  SystemResult (Scene a) where
  handleResult a = do
    res <- a
    handleResult res

instance {-# OVERLAPS #-} (Typeable a) =>
  SystemResult (Maybe a) where
  handleResult a = do
    case a of
      Nothing -> return ()
      (Just res) -> do
        handleResult res

-- Tuples -- add more later
instance {-# OVERLAPPABLE #-} (Typeable a, SystemResult a, SystemResult b) =>
  SystemResult (a, b) where
  handleResult (f, s) = do
    handleResult f
    handleResult s

allResources :: [Scene Bool] -> Scene Bool
allResources resCheckers = do
  results <- sequence resCheckers
  return $ and results

instance {-# OVERLAPPING #-} (SystemResult a) =>
  SystemFilter (Scene a) where
    paramFilters _ = []
    isGlobal _ = return True

instance {-# OVERLAPPING #-} (SystemParam a) =>
  SystemFilter (a -> b) where
    paramFilters _ = [shouldRun @a]
    isGlobal _ = allResources [isResource (Proxy @a)]

instance {-# OVERLAPPING #-} (SystemParam a, SystemParam b) =>
  SystemFilter (a -> b -> c) where
    paramFilters _ = [shouldRun @a, shouldRun @b]
    isGlobal _ = allResources [isResource (Proxy @a),
                               isResource (Proxy @b)]

instance {-# OVERLAPPING #-} (SystemParam a, SystemParam b, SystemParam c) =>
  SystemFilter (a -> b -> c -> d) where
    paramFilters _ = [shouldRun @a, shouldRun @b, shouldRun @c]
    isGlobal _ = allResources [isResource (Proxy @a),
                               isResource (Proxy @b),
                               isResource (Proxy @c)]

instance {-# OVERLAPPING #-} (SystemParam a, SystemParam b, SystemParam c, SystemParam d) =>
  SystemFilter (a -> b -> c -> d -> e) where
    paramFilters _ = [shouldRun @a, shouldRun @b, shouldRun @c, shouldRun @d]
    isGlobal _ = allResources [isResource (Proxy @a),
                               isResource (Proxy @b),
                               isResource (Proxy @c),
                               isResource (Proxy @d)]