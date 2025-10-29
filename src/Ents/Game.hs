{-# LANGUAGE CPP #-}
-- |
-- Module      : Ents.Game
-- License     : MIT
-- 
-- Ties everything together, gives you 'runGame'
-- and 'gameLoop', which facilitate running the 'Scene' monad,
-- invoking the engine's built-in systems and capping the framerate

module Ents.Game (runGame, gameLoop) where
import Ents.Resources
import Ents.Scene
import qualified Ents.Engine as Engine
import Data.Proxy
import Data.Time.Clock (getCurrentTime, diffUTCTime)
import Control.Monad.IO.Class (liftIO)
import Control.Monad (when, unless)
import Control.Concurrent (threadDelay)

getWindow :: Scene Window
getWindow = do
  maybeWin <- get (Proxy :: Proxy Window)
  maybe (do resource defaultWindow
            return defaultWindow) return maybeWin

runGame :: Scene a -> IO ()
runGame game = do
  _ <- runScene defaultSceneState game
  return ()

gameLoop :: Scene a -> Scene ()
gameLoop userLoop = do
  win <- getWindow
  
  Engine.start win

  loop >> Engine.shutdown where
    loop = do
      win <- getWindow
      
      -- Runs the systems that come with the engine,
      -- for rendering Sprites, playing Sounds etc.
      Engine.runSystems win

      startTime <- liftIO getCurrentTime
      _ <- userLoop
      endTime <- liftIO getCurrentTime

      let targetFrameTime = 1.0 / fromIntegral (targetFps win)
          elapsed = realToFrac $ diffUTCTime endTime startTime
          remaining = targetFrameTime - elapsed

      -- We have a frame cap built into the engine,
      -- but it only works outside of the Web, on the web the frame capping
      -- is handled by the Javascript host, we don't want to block in Wasm for it
      -- I am not certain how the frame cap in the engine interacts with Raylib's
      -- 'setTargetFPS', perhaps this is not needed
#ifndef WEB
      when (remaining > (0 :: Double)) $ do
        liftIO $ threadDelay (round (remaining * 1000000))
#endif

      unless (exit win) loop