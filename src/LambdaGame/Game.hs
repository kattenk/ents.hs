{-# LANGUAGE TypeApplications #-}
module LambdaGame.Game ( runGame, gameLoop ) where

import LambdaGame.Scene (Scene, SceneState(..), runScene, resource, get)
import LambdaGame.Components (Window(..), Backend(..))
import LambdaGame.Backend.Raylib (raylibBackend)
import qualified Data.Map as Map
import Data.Data (Proxy(..))
import Control.Monad (unless, when)
import Data.Time.Clock (getCurrentTime, diffUTCTime)
import Control.Monad.IO.Class (liftIO)
import Control.Concurrent (threadDelay)

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

defaultWindow :: Window
defaultWindow = Window {
  title = "Game",
  size = (640, 480),
  fps = 500,
  backend = raylibBackend,
  exit = False
}

getWindow :: Scene Window
getWindow = do
  maybeWin <- get (Proxy @Window)
  maybe (do resource defaultWindow
            return defaultWindow) return maybeWin

runGame :: Scene a -> IO ()
runGame game = do
  _ <- runScene initialState game
  return ()

gameLoop :: Scene a -> Scene ()
gameLoop userLoop = do
  win <- getWindow
  startBackend (backend win)

  loop where
    loop = do
      win <- getWindow
      updateBackend (backend win)
      let targetFrameTime = 1.0 / fromIntegral (fps win)

      startTime <- liftIO getCurrentTime
      _ <- userLoop
      endTime <- liftIO getCurrentTime

      let elapsed = realToFrac $ diffUTCTime endTime startTime
      let remaining = targetFrameTime - elapsed

      when (remaining > 0) $ do
        liftIO $ threadDelay (round (remaining * 1000000))

      unless (exit win) loop