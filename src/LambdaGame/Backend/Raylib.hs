{-# LANGUAGE TypeApplications #-}

module LambdaGame.Backend.Raylib ( raylibBackend ) where

import LambdaGame.Components (Backend(..), Window(..), Time, Text(..), Position(..), Color (..), Sprite (..))
import LambdaGame.Scene (Scene, get, resource)
import LambdaGame.Systems (system)
import Control.Monad.IO.Class (liftIO)
import Raylib.Core (clearBackground, initWindow, setTargetFPS, windowShouldClose,
                    closeWindow, windowShouldClose, getFrameTime, beginDrawing, endDrawing)
import Raylib.Core.Text (drawText)
import qualified Raylib.Types (Color(..))
import Raylib.Util (WindowResources)
import Raylib.Util.Colors (black)
import Data.Data (Proxy(..))
import Control.Monad (when)
import Data.Map.Strict (Map)
import qualified Data.Map as Map
import Raylib.Core.Textures (loadTexture, drawTexture)
import Raylib.Types (Texture)

data Assets = Assets {
  textures :: Map String Texture,
  sounds :: Map String Int
}

getTextureHandle :: String -> Scene Texture
getTextureHandle fileName = do
  maybeAssets <- get (Proxy @Assets)

  case maybeAssets of
    Nothing -> error "What"
    (Just assets) -> do
      maybe (do handle <- liftIO $ loadTexture fileName
                resource $ assets { textures = Map.insert fileName handle (textures assets)}
                return handle)
        return (Map.lookup fileName (textures assets))


startRaylib :: Scene ()
startRaylib = do
  resource (0 :: Float) -- Time
  resource $ Assets { textures = Map.empty, sounds = Map.empty }

  maybeWin <- get (Proxy @Window)
  case maybeWin of
    (Just win) -> do
      raylibWindow <- liftIO $ uncurry initWindow (size win) (title win)
      liftIO $ setTargetFPS (fps win)
      resource raylibWindow
    Nothing -> return ()

updateTime :: Scene Time
updateTime = do
  liftIO getFrameTime

drawTexts :: Text -> Position -> Color -> Scene ()
drawTexts (Text text) (Pos x y _) (Color r g b) = do
  _ <- liftIO $ do
    drawText text (round x) (round y) 50
      (Raylib.Types.Color (round r)
                          (round g)
                          (round b) 255)
  return ()

drawSprites :: Sprite -> Position -> Scene ()
drawSprites (Sprite spr) (Pos x y _) = do
  texture <- getTextureHandle spr
  _ <- liftIO $ do
    drawTexture texture 0 4 (Raylib.Types.Color 255
                                                255
                                                255 255)
  return ()

updateRaylib :: Scene ()
updateRaylib = do
  system updateTime

  liftIO beginDrawing
  liftIO $ clearBackground black
  system drawTexts
  system drawSprites
  liftIO endDrawing

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