{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE BlockArguments #-}

module LambdaGame.Backend.Raylib ( raylibBackend ) where

import LambdaGame.Components (Text(..), Position(..), Color (..), Sprite (..), Cube, x, y, z, Rotation (..), Camera3D (..), forward)
import LambdaGame.Resources (Backend(..), Window(..), Time, windowSize, Keyboard (..), Key (..), Mouse (..), TimeElapsed (TimeElapsed))
import LambdaGame.Scene (Scene, get, resource)
import LambdaGame.Systems (system)
import Control.Monad.IO.Class (liftIO)
import Raylib.Core (clearBackground, initWindow, setTargetFPS, windowShouldClose,
                    closeWindow, windowShouldClose, getFrameTime, beginDrawing, endDrawing, getScreenWidth, getScreenHeight, beginMode3D, endMode3D, isKeyDown, isKeyPressed, isKeyReleased, getMousePosition, getMouseDelta, hideCursor, disableCursor)
import Raylib.Core.Text (drawText)
import Raylib.Util (WindowResources)
import Raylib.Util.Colors (black, white)
import Data.Data (Proxy(..))
import Control.Monad (when, filterM)
import Data.Map.Strict (Map)
import qualified Data.Map as Map
import Raylib.Core.Textures (loadTexture, drawTexturePro)
import Linear.V2 (V2(..))
import qualified Raylib.Types as RL
import Linear.V3 (V3(..))
import Raylib.Core.Models (drawCube)
import Data.Maybe (fromMaybe)
import qualified Data.Set as Set
import Raylib.Util.RLGL (rlPushMatrix, rlPopMatrix, rlRotatef, rlTranslatef)
import Data.List (sortOn)

data Assets = Assets {
  textures :: Map String RL.Texture,
  sounds :: Map String Int
}

getTextureHandle :: String -> Scene RL.Texture
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
  resource ([] :: [SpriteCommand])
  resource (TimeElapsed 0)
  resource $ Assets { textures = Map.empty, sounds = Map.empty }
  resource $ Mouse { mousePos = (0, 0),
                     mouseMovement = (0, 0)}
  resource RL.Camera3D {
    RL.camera3D'position = V3 0 0 0,
    RL.camera3D'target = V3 0 0 1,
    RL.camera3D'up = V3 0 1 0,
    RL.camera3D'fovy = 100,
    RL.camera3D'projection = RL.CameraPerspective
  }

  resource $ Keyboard {
    downKeys = Set.empty,
    pressedKeys = Set.empty,
    releasedKeys = Set.empty
  }

  maybeWin <- get (Proxy @Window)
  case maybeWin of
    (Just win) -> do
      screenWidth <- liftIO getScreenWidth
      screenHeight <- liftIO getScreenHeight

      raylibWindow <- liftIO $ uncurry
        initWindow (windowSize (screenWidth, screenHeight) win) (title win)

      liftIO $ setTargetFPS (targetFps win)

      when (captureCursor win) $ do
        liftIO hideCursor
        liftIO disableCursor

      resource raylibWindow

    Nothing -> return ()

updateTime :: Scene Time
updateTime = do
  liftIO getFrameTime

addTime :: TimeElapsed -> Time -> TimeElapsed
addTime (TimeElapsed t) d = TimeElapsed (t + d)

data SpriteCommand = SpriteCommand {
  sprTexture :: RL.Texture,
  sprPosition :: V3 Float,
  sprSize :: V2 Float
}

recordSprites :: [SpriteCommand] -> Sprite -> Position -> Scene ()
recordSprites cmds (Sprite spr) pos = do
  maybeWin <- get (Proxy @Window)
  case maybeWin of
    Nothing -> return ()
    Just win -> do
      texture <- getTextureHandle spr
      screenW <- liftIO getScreenWidth
      screenH <- liftIO getScreenHeight

      let scale = calculateScale (screenW, screenH) win
          texW = fromIntegral (RL.texture'width texture)
          texH = fromIntegral (RL.texture'height texture)

      resource $ SpriteCommand {
        sprTexture = texture,
        sprPosition = V3 ((x pos) * scale) ((y pos) * scale) (z pos),
        sprSize = V2 (texW * scale) (texH * scale)
      } : cmds

drawSprites :: [SpriteCommand] -> Scene ()
drawSprites cmds = do
  let sortedCmds = sortOn ((\(V3 _ _ z) -> z) . sprPosition) cmds
-- DrawTexturePro(Texture2D texture, Rectangle source, Rectangle dest,
               -- Vector2 origin, float rotation, Color tint);
  mapM_ (\cmd -> do
    liftIO $ drawTexturePro (sprTexture cmd)
                            (RL.Rectangle 0 0 (fromIntegral (RL.texture'width (sprTexture cmd)))
                                              (fromIntegral (RL.texture'height (sprTexture cmd))))
                            (RL.Rectangle (x (sprPosition cmd))
                                          (y (sprPosition cmd))
                                          (x (sprSize cmd))
                                          (y (sprSize cmd)))
                            (V2 0 0) 0 (RL.Color 255 255 255 255)) sortedCmds

calculateScale :: (Int, Int) -> Window -> Float
calculateScale (screenW, screenH) win =
  let (winW, _) = windowSize (screenW, screenH) win
      (resW, _) = res win
  in fromIntegral winW / fromIntegral resW

drawTexts :: Text -> Position -> Color -> Scene ()
drawTexts (Text text) pos color = do
  liftIO $ do
    drawText text (round (x pos)) (round (y pos)) 60
      (toRaylibColor color)
  return ()

toRaylibColor :: Color -> RL.Color
toRaylibColor (Color r g b a) =
  RL.Color (round r)
                     (round g)
                     (round b) (round a)

drawCubes :: Cube -> Maybe Position -> Maybe Color -> Maybe Rotation -> Scene ()
drawCubes _ maybePos maybeColor maybeRot = do
  let pos = fromMaybe (Position 0 0 0) maybePos
  let color = fromMaybe (Color 255 255 255 255) maybeColor
  let (Rotation yaw pitch roll) = fromMaybe (Rotation 0 0 0) maybeRot
  camera <- get (Proxy @RL.Camera3D)
  case camera of
    Nothing -> return ()
    Just cam -> liftIO $ do
      beginMode3D cam
      rlPushMatrix
      rlTranslatef (x pos) (y pos) (z pos)
      rlRotatef yaw   0 1 0
      rlRotatef pitch 1 0 0
      rlRotatef roll  0 0 1
      drawCube (V3 0 0 0) 1 1 1 (toRaylibColor color)
      rlPopMatrix
      endMode3D

updateMouse :: Scene Mouse
updateMouse = do
  (RL.Vector2 posX posY) <- liftIO getMousePosition
  (RL.Vector2 moveX moveY) <- liftIO getMouseDelta
  return Mouse { mousePos = (posX, posY), mouseMovement = (moveX, moveY)}

toRaylibKey :: Key -> RL.KeyboardKey
toRaylibKey W = RL.KeyW
toRaylibKey A = RL.KeyA
toRaylibKey S = RL.KeyS
toRaylibKey D = RL.KeyD
toRaylibKey Space =  RL.KeySpace

updateKeyboard :: Scene Keyboard
updateKeyboard = do
  downs <- filterM (liftIO . isKeyDown . toRaylibKey) [minBound .. maxBound]
  pressed <- filterM (liftIO . isKeyPressed . toRaylibKey) [minBound .. maxBound]
  released <- filterM (liftIO . isKeyReleased . toRaylibKey) [minBound .. maxBound]

  return Keyboard {
    downKeys = Set.fromList downs,
    pressedKeys = Set.fromList pressed,
    releasedKeys = Set.fromList released
  }

updateCameras :: Camera3D -> Maybe Position -> Maybe Rotation -> Scene ()
updateCameras cam maybePos maybeRot = do
  let position = fromMaybe (Position 0 0 0) maybePos
      rot = fromMaybe (Rotation 0 0 0) maybeRot
      posV3 = V3 (x position) (y position) (z position)
      target = posV3 + forward rot
      up = V3 0 1 0
      projection = RL.CameraPerspective

  resource RL.Camera3D {
    RL.camera3D'position = V3 (x position) (y position) (z position),
    RL.camera3D'target = target,
    RL.camera3D'up = up,
    RL.camera3D'fovy = fov cam,
    RL.camera3D'projection = projection
  }

updateRaylib :: Scene ()
updateRaylib = do
  maybeWin <- get (Proxy @Window)
  case maybeWin of
    Nothing -> return ()
    (Just win) -> do
      system updateTime
      system addTime
      system updateMouse
      system updateKeyboard
      system updateCameras

      liftIO beginDrawing
      liftIO $ clearBackground black
      -- Drawing systems
      resource ([] :: [SpriteCommand])
      system recordSprites
      system drawSprites
      system drawTexts
      system drawCubes
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