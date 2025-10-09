{-# LANGUAGE AllowAmbiguousTypes, TypeApplications, ScopedTypeVariables,
             FlexibleInstances, UndecidableInstances, FlexibleContexts #-}

module LambdaGame.Systems (
    SystemFilter(..),
    SystemRunner(..),
    system
) where
import LambdaGame.Scene ( Scene, ComponentAccess(..), ReturnType, isResource,
                          SceneState(entityCount, currentEntity), currentEnt )
import Data.Data (Proxy(..))
import Data.Typeable (Typeable)
import Control.Monad.State.Strict hiding (get)

-- | Action for running a System
system :: (SystemFilter f, SystemRunner f) => f -> Scene ()
system f = do
    entities <- gets entityCount
    entitiesToRunOn <- filterM (getFilter f) [0 .. entities - 1]

    liftIO $ putStrLn $ "Entities to run on: " ++ show entitiesToRunOn
    if null entitiesToRunOn then do
        runSystem f
    else do
        forM_ entitiesToRunOn $ \e -> do
            modify $ \s -> s { currentEntity = e }
            runSystem f

class Typeable p => SystemParam p where
    -- | Should a system with this parameter run on this entity?
    shouldRun :: Int -> Scene Bool
    -- | Provides an argument for this parameter for an entity,
    -- returns Nothing if the parameter cannot be satisfied
    -- (shouldn't happen ever after 'shouldRun' returns True).
    argumentFor :: Int -> Scene (Maybe p)

instance Typeable a => SystemParam a where
    shouldRun e = do
        has (e, Proxy @a)

    argumentFor e = do
        get (e, Proxy @a)

instance Typeable a => SystemParam (Maybe a) where
    shouldRun _ = return True
    argumentFor e = do
        component <- get (e, Proxy @a)
        return (Just component)

-- where filters = map (`fst` i) (paramFilters f)

filterFalse :: [(a, Scene Bool)] -> Scene [a]
filterFalse = fmap (map fst) . filterM (fmap not . snd)

class SystemFilter f where
    -- | Takes a system function and returns a "filter" function,
    -- used to determine if a system should run on an entity
    getFilter :: f -> (Int -> Scene Bool)
    getFilter f i = do
        filteredFilters <- filterFalse (paramFilters f)
        let filters = map ($ i) filteredFilters
        results <- sequence filters
        if null results then
            return False
        else
            return $ and results

    paramFilters :: f -> [(Int -> Scene Bool, Scene Bool)]

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
          Nothing -> liftIO $ putStrLn "Couldn't provide argument"

-- | Case where there are no parameters left and we have a result
instance SystemResult a => SystemRunner a where
    runSystem a = do
        handleResult a


class SystemResult r where
    handleResult :: r -> Scene ()

instance {-# OVERLAPPABLE #-} (Typeable a, Typeable (ReturnType a)) => SystemResult a where
    handleResult a = do
        set a

instance {-# OVERLAPPING #-} (Typeable a, Typeable (ReturnType a)) => SystemResult (Scene a) where
    handleResult a = do
        res <- a
        set res

instance {-# OVERLAPPING #-} Typeable a =>
    SystemFilter a where
    paramFilters _ = []

instance {-# OVERLAPPING #-} (SystemParam a, Typeable a) =>
    SystemFilter (a -> b) where
    paramFilters _ = [(shouldRun @a, isResource (Proxy @a))]

instance {-# OVERLAPPING #-}
    (SystemParam a, SystemParam b) =>
    SystemFilter (a -> b -> c) where
    paramFilters _ = [(shouldRun @a, isResource (Proxy @a)),
                      (shouldRun @b, isResource (Proxy @b))]

instance {-# OVERLAPPING #-}
    (SystemParam a, SystemParam b, SystemParam c) =>
    SystemFilter (a -> b -> c -> d) where
    paramFilters _ = [(shouldRun @a, isResource (Proxy @a)),
                      (shouldRun @b, isResource (Proxy @b)),
                      (shouldRun @c, isResource (Proxy @c))]

instance {-# OVERLAPPING #-}
    (SystemParam a, SystemParam b, SystemParam c, SystemParam d) =>
    SystemFilter (a -> b -> c -> d -> e) where
    paramFilters _ = [(shouldRun @a, isResource (Proxy @a)),
                      (shouldRun @b, isResource (Proxy @b)),
                      (shouldRun @c, isResource (Proxy @c)),
                      (shouldRun @d, isResource (Proxy @d))]