{-# LANGUAGE TypeApplications #-}
module Main (main) where

import LambdaGame
import qualified Data.Map.Strict as Map

data MyResource = MyResource {apple :: String, pear :: Int} deriving Show

defaultECS :: SceneState
defaultECS = SceneState { resources = Map.empty
                        , currentEntity = 0}

testAction :: Scene ()
testAction = do
    setResource MyResource { apple = "Test", pear = 15}
    setResource (Just "Test")
    maybeResource <- getResource @MyResource

    liftIO $ print maybeResource

main :: IO ()
main = do
    _ <- runScene defaultECS testAction
    putStrLn "Hello, Haskell"