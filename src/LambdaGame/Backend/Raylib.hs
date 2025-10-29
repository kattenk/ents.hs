{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE BlockArguments #-}

module LambdaGame.Backend.Raylib ( raylibBackend ) where

import LambdaGame.Components (Text(..), Position(..), Color (..),
  Sprite (..), Cube, HasXYZ(..), Rotation (..), Camera3D (..), forward, Sound (..), Angle (..), Font (..), TextAlignment (..), Rectangle, Size (..))
import LambdaGame.Resources (Backend(..), Window(..), Time, windowSize,
  Keyboard (..), Key (..), Mouse (..), TimeElapsed (TimeElapsed))
import LambdaGame.Scene (Scene, get, resource)
import LambdaGame.Systems (system)
import Control.Monad.IO.Class (liftIO)
import Raylib.Core (clearBackground, initWindow, setTargetFPS, windowShouldClose,
                    closeWindow, windowShouldClose, getFrameTime, beginDrawing, endDrawing, beginMode3D, endMode3D, isKeyDown, isKeyPressed, isKeyReleased, getMousePosition, getMouseDelta, hideCursor, disableCursor, isMouseButtonDown, isMouseButtonPressed, isMouseButtonReleased, beginBlendMode, endBlendMode, getMonitorWidth, getMonitorHeight)
import Raylib.Core.Text (measureText, loadFont, getFontDefault, drawTextEx)
import Raylib.Util (WindowResources)
import Raylib.Util.Colors (black)
import Data.Data (Proxy(..))
import Control.Monad (when, filterM, unless)
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
import Raylib.Core.Audio (loadSound, playSound, initAudioDevice)
import Raylib.Core.Shapes (drawRectangleRec)

data Assets = Assets {
  textures :: Map String RL.Texture,
  sounds :: Map String RL.Sound,
  fonts :: Map String RL.Font
}

getTextureHandle :: String -> Scene RL.Texture
getTextureHandle fileName = do
  maybeAssets <- get (Proxy @Assets)

  case maybeAssets of
    Nothing -> error "could not get Assets"
    (Just assets) -> do
      maybe (do handle <- liftIO $ loadTexture fileName
                resource $ assets { textures = Map.insert fileName handle (textures assets)}
                return handle)
            return
            (Map.lookup fileName (textures assets))

getSoundHandle :: String -> Scene RL.Sound
getSoundHandle fileName = do
  maybeAssets <- get (Proxy @Assets)

  case maybeAssets of
    Nothing -> error "could not get Assets"
    (Just assets) -> do
      maybe (do handle <- liftIO $ loadSound fileName
                resource $ assets { sounds = Map.insert fileName handle (sounds assets)}
                return handle)
            return
            (Map.lookup fileName (sounds assets))

getFontHandle :: String -> Scene RL.Font
getFontHandle fileName = do
  maybeAssets <- get (Proxy @Assets)

  case maybeAssets of
    Nothing -> error "could not get Assets"
    (Just assets) -> do
      maybe (do handle <- liftIO $ loadFont fileName
                resource $ assets { fonts = Map.insert fileName handle (fonts assets)}
                return handle)
            return
            (Map.lookup fileName (fonts assets))

startRaylib :: Scene ()
startRaylib = do
  resource (0 :: Float) -- Time
  resource ([] :: [SpriteCommand])
  resource (TimeElapsed 0)
  resource $ Assets { textures = Map.empty, sounds = Map.empty, fonts = Map.empty }
  resource $ Mouse { mousePos = (0, 0),
                     mouseMovement = (0, 0),
                     leftMouseDown = False,
                     leftMousePressed = False,
                     leftMouseReleased = False,
                     rightMouseDown = False,
                     rightMousePressed = False,
                     rightMouseReleased = False }
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
      screenWidth <- liftIO $ getMonitorWidth 0
      screenHeight <- liftIO $ getMonitorHeight 0

      raylibWindow <- liftIO $ uncurry
        initWindow (windowSize (screenWidth, screenHeight) win) (title win)

      unless (onWebPlatform win) $ do
        liftIO $ setTargetFPS (targetFps win)
      liftIO initAudioDevice

      -- liftIO $ setTargetFPS 90

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

getScale :: (Int, Int) -> Window -> Float
getScale (screenW, screenH) win =
  let (winW, _) = windowSize (screenW, screenH) win
      (resW, _) = res win
  in fromIntegral winW / fromIntegral resW

data SpriteCommand = SpriteCommand {
  sprTexture :: RL.Texture,
  sprPosition :: V3 Float,
  sprSize :: V2 Float,
  sprAngle :: Float,
  sprTint :: RL.Color
}

-- this is done for Z-sorting
recordSprites :: [SpriteCommand] -> Sprite -> Position -> Maybe Angle -> Maybe Color -> Scene ()
recordSprites cmds (Sprite spr) pos angle tint = do
  maybeWin <- get (Proxy @Window)
  case maybeWin of
    Nothing -> return ()
    Just win -> do
      texture <- getTextureHandle spr
      screenW <- liftIO $ getMonitorWidth 0
      screenH <- liftIO $ getMonitorHeight 0

      let scale = getScale (screenW, screenH) win
          texW = fromIntegral (RL.texture'width texture)
          texH = fromIntegral (RL.texture'height texture)

      resource $ SpriteCommand {
        sprTexture = texture,
        sprPosition = V3 (x pos * scale) (y pos * scale) (z pos),
        sprSize = V2 (texW * scale) (texH * scale),
        sprAngle = case angle of
          Nothing -> 0
          (Just (Angle a)) -> a,
        sprTint = toRaylibColor (fromMaybe (Color 255 255 255 255) tint)
      } : cmds

drawSprites :: [SpriteCommand] -> Scene ()
drawSprites cmds = do
  let sortedCmds = sortOn ((\(V3 _ _ z_) -> z_) . sprPosition) cmds
-- DrawTexturePro(Texture2D texture, Rectangle source, Rectangle dest,
               -- Vector2 origin, float rotation, Color tint);
  liftIO $ beginBlendMode RL.BlendAlpha
  mapM_ (\cmd -> do
    liftIO $ drawTexturePro (sprTexture cmd)
                            (RL.Rectangle 0 0 (fromIntegral (RL.texture'width (sprTexture cmd)))
                                              (fromIntegral (RL.texture'height (sprTexture cmd))))
                            (RL.Rectangle (x (sprPosition cmd) + (x (sprSize cmd) / 2))
                                          (y (sprPosition cmd) + (y (sprSize cmd) / 2))
                                          (x (sprSize cmd))
                                          (y (sprSize cmd)))
                            (V2 (x (sprSize cmd) / 2) (y (sprSize cmd) / 2)) (sprAngle cmd) (sprTint cmd)) sortedCmds

  liftIO $ endBlendMode


    -- void DrawText(const char *text, int posX, int posY, int fontSize, Color color);       // Draw text (using default font)
    -- void DrawTextEx(Font font, const char *text, Vector2 position, float fontSize, float spacing, Color tint); // Draw 

drawTexts :: Text -> Position -> Maybe Color -> Maybe Font -> Scene ()
drawTexts (Text text size alignment) pos color maybeFont = do
  maybeWin <- get (Proxy @Window)
  case maybeWin of
    Nothing -> return ()
    Just win -> do
      screenW <- liftIO $ getMonitorWidth 0
      screenH <- liftIO $ getMonitorHeight 0

      defaultFont <- liftIO getFontDefault
      font <- maybe (return defaultFont) (\(Font a) -> getFontHandle a) maybeFont
      let scale = getScale (screenW, screenH) win
          scaledSize = size * scale

      liftIO $ do
        textWidth <- measureText text (round scaledSize)

        let position = case alignment of
                          AlignLeft -> V2 (x pos * scale) (y pos * scale)
                          AlignCenter -> V2 ((x pos * scale) - (fromIntegral textWidth / 2))
                                    (y pos * scale)
                          AlignRight -> V2 ((x pos * scale) - fromIntegral textWidth)
                                    (y pos * scale)

        drawTextEx font text position scaledSize 3
          (toRaylibColor (fromMaybe (Color 255 255 255 255) color))

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

drawRectangles :: Rectangle -> Position -> Size -> Maybe Color -> Scene ()
drawRectangles _ (Position x y _) (Size sx sy _) maybeColor = do
  maybeWin <- get (Proxy @Window)
  case maybeWin of
    Nothing -> return ()
    Just win -> do
      screenW <- liftIO $ getMonitorWidth 0
      screenH <- liftIO $ getMonitorHeight 0
      let scale = getScale (screenW, screenH) win
      liftIO $ beginBlendMode RL.BlendAlpha
      liftIO $ drawRectangleRec (RL.Rectangle (x * scale) (y * scale) (sx * scale) (sy * scale)) color
        where color = toRaylibColor (fromMaybe (Color 255 255 255 255) maybeColor)

playSounds :: Sound -> Scene Sound
playSounds SoundPlayed = return SoundPlayed
playSounds (Sound fileName) = do
  sound <- getSoundHandle fileName
  liftIO $ playSound sound
  return SoundPlayed

updateMouse :: Scene Mouse
updateMouse = do
  (RL.Vector2 posX posY) <- liftIO getMousePosition
  (RL.Vector2 moveX moveY) <- liftIO getMouseDelta
  leftDown <- liftIO $ isMouseButtonDown RL.MouseButtonLeft
  leftPressed <- liftIO $ isMouseButtonPressed RL.MouseButtonLeft
  leftReleased <- liftIO $ isMouseButtonReleased RL.MouseButtonLeft
  rightDown <- liftIO $ isMouseButtonDown RL.MouseButtonRight
  rightPressed <- liftIO $ isMouseButtonPressed RL.MouseButtonRight
  rightReleased <- liftIO $ isMouseButtonReleased RL.MouseButtonRight
  return Mouse { mousePos = (posX, posY),
                 mouseMovement = (moveX, moveY),
                 leftMouseDown = leftDown,
                 leftMousePressed = leftPressed,
                 leftMouseReleased = leftReleased,
                 rightMouseDown = rightDown,
                 rightMousePressed = rightPressed,
                 rightMouseReleased = rightReleased}

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

      unless (onWebPlatform win) $ do
        liftIO beginDrawing
      
        liftIO $ clearBackground black

      -- Drawing
      resource ([] :: [SpriteCommand])
      system recordSprites
      system drawSprites

      unless (onWebPlatform win) $ do
        system drawTexts

      system drawRectangles
      system drawCubes

      unless (onWebPlatform win) $ do
        liftIO endDrawing

      -- Sound
      system playSounds

      -- Use Raylib's WindowShouldClose function to exit
      -- if escape is pressed or the close button is clicked
      unless (onWebPlatform win) $ do
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