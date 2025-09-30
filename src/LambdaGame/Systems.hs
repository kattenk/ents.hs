{-# LANGUAGE AllowAmbiguousTypes #-}
{-# OPTIONS_GHC -Wno-unused-top-binds #-}
{-# LANGUAGE TypeApplications, ScopedTypeVariables #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE FlexibleContexts #-}

module LambdaGame.Systems (
    SystemFilter(..),
    SystemParam(..),
    SystemRunner(..)
) where
import LambdaGame.Scene (Scene, ComponentAccess(..), currentEnt, ReturnType)
import Data.Data (Proxy(..))
import Data.Typeable (Typeable)
import Control.Monad.IO.Class (liftIO)
import Data.Typeable (typeRep)

class SystemParam p where
    -- | Should a system with this parameter run on this entity?
    shouldRun :: Int -> Scene Bool
    -- | Provides an argument for this parameter for an entity
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

class SystemFilter f where
    -- | Takes a system function and returns a "filter" function,
    -- used to determine if a system should run on an entity
    getFilter :: f -> (Int -> Scene Bool)
    getFilter f i = do
        results <- sequence filters
        return $ and results
            where filters = map ($ i) (paramFilters f)

    paramFilters :: f -> [Int -> Scene Bool]

class SystemRunner f where
    runSystem :: f -> Scene ()

instance {-# OVERLAPPING #-} (SystemParam a, SystemRunner b) => SystemRunner (a -> b) where
    runSystem f = do
        cur <- currentEnt
        argument <- argumentFor @a cur
        case argument of
          (Just arg) -> do
            runSystem $ f arg
          Nothing -> liftIO $ putStrLn "Couldn't provide argument"

instance {-# OVERLAPPABLE #-} SystemResult a => SystemRunner a where
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

instance {-# OVERLAPPING #-} (SystemParam a) =>
    SystemFilter (a -> b) where
    paramFilters _ = [shouldRun @a]

instance {-# OVERLAPPING #-} (SystemParam a, SystemParam b) =>
    SystemFilter (a -> b -> c) where
    paramFilters _ = [shouldRun @a, shouldRun @b]

instance {-# OVERLAPPING #-} (SystemParam a, SystemParam b, SystemParam c) =>
    SystemFilter (a -> b -> c -> d) where
    paramFilters _ = [shouldRun @a, shouldRun @b, shouldRun @c]

instance {-# OVERLAPPING #-} (SystemParam a, SystemParam b, SystemParam c, SystemParam d) =>
    SystemFilter (a -> b -> c -> d -> e) where
    paramFilters _ = [shouldRun @a, shouldRun @b, shouldRun @c, shouldRun @d]