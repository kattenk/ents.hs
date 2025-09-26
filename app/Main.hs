{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleContexts #-}
module Main (main) where

class SumArgs r f where
  sumOf :: f -> r

instance (a ~ ()) => SumArgs (IO a) String where
    sumOf x = print x

instance (a ~ ()) => SumArgs (IO a) Char where
    sumOf x = print x

instance SumArgs r String => SumArgs (Char -> r) String where
  sumOf x y = sumOf (x ++ ['x'])

instance SumArgs r String => SumArgs (Char -> r) Char where
  sumOf x y = sumOf ('x' : ['x'])

main :: IO ()
main = do
    putStrLn "Hello"
    sumOf 'h' 'i' 'h'
    putStrLn "Hello"

-- initialState = LambdaGame.SceneState
--   { resources = Map.empty,
--     components = Map.empty,
--     recycleEntityIndices = [],
--     currentEntity = 0
--   }

-- testAction :: Scene ()
-- testAction = do
--   id <- currentEnt

--   set (5 :: Int)

--   val <- get (0 :: Int)
--   liftIO $ print val
--   return ()

-- main = do
--   runScene initialState testAction