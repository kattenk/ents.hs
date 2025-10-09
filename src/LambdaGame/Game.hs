{-# LANGUAGE TypeApplications #-}
module LambdaGame.Game ( runGame, gameLoop, ExitGame(..) ) where

import LambdaGame.Scene (Scene, SceneState(..), runScene, resource, get)
import qualified Data.Map as Map
import Data.Data (Proxy(..))
import Control.Monad (unless)
import Control.Monad.IO.Class (liftIO)

initialState :: SceneState
initialState = SceneState
  { resources = Map.empty,
    components = Map.empty,
    entityCount = 0,
    growComponents = return (),
    clearEntity = \_ -> return (),
    reusableIndices = [],
    currentEntity = 0
  }

runGame :: Scene a -> IO ()
runGame game = do
  _ <- runScene initialState game
  return ()

data ExitGame = ExitGame Bool

gameLoop :: Scene a -> Scene ()
gameLoop userLoop = do
  resource $ ExitGame False
  loop where
    loop = do
      _ <- userLoop
      runAgain <- get (Proxy @ExitGame)
      case runAgain of
        (Just (ExitGame exit)) -> unless exit loop
        Nothing -> return ()