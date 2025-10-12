{-# LANGUAGE TypeApplications #-}

module LambdaGame.Backend.Raylib ( raylibBackend ) where

import LambdaGame.Components (Text(..), Position(..), Color (..), Sprite (..), x, y)
import LambdaGame.Resources (Backend(..), Window(..), Time, windowSize)
import LambdaGame.Scene (Scene, get, resource)
import LambdaGame.Systems (system)
import Control.Monad.IO.Class (liftIO)
import Raylib.Core (clearBackground, initWindow, setTargetFPS, windowShouldClose,
                    closeWindow, windowShouldClose, getFrameTime, beginDrawing, endDrawing, beginTextureMode, endTextureMode, getScreenWidth, getScreenHeight)
import Raylib.Core.Text (drawText)
import qualified Raylib.Types (Color(..))
import Raylib.Util (WindowResources)
import Raylib.Util.Colors (black, white)
import Data.Data (Proxy(..))
import Control.Monad (when)
import Data.Map.Strict (Map)
import qualified Data.Map as Map
import Raylib.Core.Textures (loadTexture, drawTexture, loadRenderTexture, drawTexturePro)
import Raylib.Types (Texture, Rectangle (Rectangle), RenderTexture(..))
import Linear.V2 (V2(..))

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

getScreenTexture :: Window -> Scene RenderTexture
getScreenTexture win = do
  maybeTexture <- get (Proxy @RenderTexture)

  maybe (do screenTexture <- liftIO $ uncurry loadRenderTexture (res win)
            resource screenTexture
            return screenTexture)
        return maybeTexture

startRaylib :: Scene ()
startRaylib = do
  resource (0 :: Float) -- Time
  resource $ Assets { textures = Map.empty, sounds = Map.empty }

  maybeWin <- get (Proxy @Window)
  case maybeWin of
    (Just win) -> do
      screenWidth <- liftIO getScreenWidth
      screenHeight <- liftIO getScreenHeight
      
      raylibWindow <- liftIO $ uncurry
        initWindow (windowSize (screenWidth, screenHeight) win) (title win)
      
      liftIO $ setTargetFPS (targetFps win)
      resource raylibWindow

    Nothing -> return ()

updateTime :: Scene Time
updateTime = do
  liftIO getFrameTime

drawSprites :: Sprite -> Position -> Scene ()
drawSprites (Sprite spr) pos = do
  texture <- getTextureHandle spr
  liftIO $ do
    drawTexture texture (round (x pos))
                        (round (y pos))
                        (Raylib.Types.Color 255
                                            255
                                            255 255)
  return ()

drawTexts :: Text -> Position -> Color -> Scene ()
drawTexts (Text text) pos (Color r g b) = do
  liftIO $ do
    drawText text (round (x pos)) (round (y pos)) 10
      (Raylib.Types.Color (round r)
                          (round g)
                          (round b) 255)
  return ()

updateRaylib :: Scene ()
updateRaylib = do
  maybeWin <- get (Proxy @Window)
  case maybeWin of
    Nothing -> return ()
    (Just win) -> do
      system updateTime

      screenTexture <- getScreenTexture win

      liftIO $ beginTextureMode screenTexture
      liftIO $ clearBackground black
      -- Drawing systems
      system drawSprites
      system drawTexts

      liftIO endTextureMode

      screenWidth <- liftIO getScreenWidth
      screenHeight <- liftIO getScreenHeight
      let fi = fromIntegral
      let resWidth = fi (fst (res win))
      let resHeight = fi (snd (res win))

      let scale = min (fi screenWidth / resWidth)
                      (fi screenHeight / resHeight)
      
      let src = Rectangle 0 0 resWidth (- resHeight)
      let dest = Rectangle ((fi screenWidth - resWidth * scale) / 2)
                           ((fi screenHeight - resHeight * scale) / 2)
                           (resWidth * scale)
                           (resHeight * scale)

      liftIO beginDrawing
      liftIO $ drawTexturePro (renderTexture'texture screenTexture) src dest (V2 0 0) 0 white 
      liftIO endDrawing

      -- Use Raylib's WindowShouldClose function to exit
      -- if escape is pressed or the close button is clicked
      shouldClose <- liftIO windowShouldClose
      when shouldClose $ do
        resource $ win { exit = True }

stopRaylib :: Scene ()
stopRaylib = do
  raylibWindow <- get (Proxy @WindowResources)
  liftIO $ closeWindow raylibWindow

raylibBackend :: Backend
raylibBackend = Backend { startBackend = startRaylib,
                          updateBackend = updateRaylib,
                          stopBackend = stopRaylib }