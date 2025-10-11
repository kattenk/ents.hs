{-# LANGUAGE TypeApplications #-}
module LambdaGame.Backend.Raylib ( raylibBackend ) where

import LambdaGame.Components (Backend(..), Window(..), Time(..))
import LambdaGame.Scene (Scene, get, resource)
import LambdaGame.Systems (system)
import Control.Monad.IO.Class (liftIO)
import Raylib.Core (clearBackground, initWindow, setTargetFPS, windowShouldClose,
                    closeWindow, windowShouldClose, getFrameTime)
import Raylib.Core.Text (drawText)
import Raylib.Util (drawing, WindowResources)
import Raylib.Util.Colors (lightGray, black)
import Data.Data (Proxy(..))
import Control.Monad (when)

startRaylib :: Scene ()
startRaylib = do
  resource $ Time 0

  win <- get (Proxy @Window)
  case win of
    (Just win) -> do
      raylibWindow <- liftIO $ uncurry initWindow (size win) (title win)
      liftIO $ setTargetFPS (fps win)
      resource raylibWindow
    Nothing -> return ()

updateTime :: Scene Time
updateTime = do
  frameTime <- liftIO getFrameTime
  return $ Time frameTime

updateRaylib :: Scene ()
updateRaylib = do
  system updateTime
  window <- get (Proxy @WindowResources)
  _ <- liftIO $ drawing $ do
    clearBackground black
    drawText "Basic raylib window" 30 40 50 lightGray
    return window

  maybeWin <- get (Proxy @Window)
  case maybeWin of
    Nothing -> return ()
    (Just win) -> do
      shouldClose <- liftIO windowShouldClose
      when shouldClose $ do
        resource $ win {exit = True}

stopRaylib :: Scene ()
stopRaylib = do
  raylibWindow <- get (Proxy @WindowResources)
  liftIO $ closeWindow raylibWindow

raylibBackend :: Backend
raylibBackend = Backend { startBackend = startRaylib,
                          updateBackend = updateRaylib,
                          stopBackend = stopRaylib}