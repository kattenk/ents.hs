{-# LANGUAGE CPP #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# OPTIONS_GHC -Wno-orphans #-}
-- |
-- Module      : Ents.Engine
-- License     : MIT
-- 
-- The underlying Raylib implementation of the core components
-- and resources. displays the Window, renders Sprites, Text,
-- plays Sounds, etc

module Ents.Engine (start, runSystems, shutdown) where
  
import Ents.Resources
import Ents.Components
import Ents.Scene
import Ents.Systems
import Control.Monad.IO.Class (liftIO)
import qualified Raylib.Core as RL
import qualified Raylib.Util as RL
import qualified Raylib.Core.Audio as RL
import Data.Proxy
import Control.Monad (when, filterM)
import qualified Raylib.Util.Colors as RL
import Data.Map (Map)
import qualified Data.Map as Map
import qualified Raylib.Types.Core.Textures as RL
import qualified Raylib.Types.Core.Audio as RL
import qualified Raylib.Types.Core.Text as RL
import Foreign (Ptr, with, Storable (..))
import Linear
import qualified Raylib.Types.Core as RL
import qualified Raylib.Core.Textures as RL
import Data.Maybe (fromMaybe)
import Data.List (sortOn)
import Foreign.C (withCString)
import qualified Raylib.Core.Text as RL
import Raylib.Types (vector2'x)
import qualified Raylib.Types as RL
import qualified Raylib.Util.RLGL as RL
import qualified Raylib.Core.Models as RL
import qualified Raylib.Core.Shapes as RL
import qualified Data.Set as Set

windowPixelSize :: Window -> IO (Int, Int)
windowPixelSize win = do
  monitor <- liftIO RL.getCurrentMonitor
  monitorWidth <- liftIO $ RL.getMonitorWidth monitor

  return $
    case windowSize win of
      ExactWinSize size -> size
      RelativeSize percentage ->
        (width, round ((fromIntegral width * aspectRatio) :: Float))
          where
            width = round ((fromIntegral monitorWidth / 100) :: Float) * percentage
            aspectRatio = fromIntegral (snd (res win)) / fromIntegral (fst (res win))

start :: Window -> Scene ()
start win = do
  resource (0 :: Float) -- Time
  resource ([] :: [SpriteCommand])
  resource $ TimeElapsed 0

  resource $ Assets { textures = Map.empty,
                      sounds = Map.empty,
                      fonts = Map.empty }
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

  rlWindow <- liftIO $ uncurry RL.initWindow (res win) (title win)
  
  resource rlWindow -- Make the Raylib WindowResources available as a Resource

  -- Setup window size
  (width, height) <- liftIO $ windowPixelSize win
  liftIO $ RL.setWindowSize width height

#ifndef WEB
  when (captureCursor win) $ do
    liftIO RL.hideCursor
    liftIO RL.disableCursor
#endif

  -- Setup sound
  liftIO RL.initAudioDevice

  return ()

exitOnClose :: Window -> Scene ()
exitOnClose win = do
  shouldClose <- liftIO RL.windowShouldClose

  when shouldClose $ do
    resource $ win { exit = True }

updateTime :: Scene Time
updateTime = do
  liftIO RL.getFrameTime

addTime :: TimeElapsed -> Time -> TimeElapsed
addTime (TimeElapsed t) d = TimeElapsed (t + d)

deriving instance Bounded RL.KeyboardKey

-- Only includes keys A-Z
updateKeyboard :: Scene Keyboard
updateKeyboard = do
  downs <- filterM (liftIO . RL.isKeyDown) [RL.KeyA .. RL.KeyZ]
  pressed <- filterM (liftIO . RL.isKeyPressed) [RL.KeyA .. RL.KeyZ]
  released <- filterM (liftIO . RL.isKeyReleased) [RL.KeyA .. RL.KeyZ]

  return Keyboard {
    downKeys = Set.fromList downs,
    pressedKeys = Set.fromList pressed,
    releasedKeys = Set.fromList released
  }

updateMouse :: Scene Mouse
updateMouse = do
  (RL.Vector2 posX posY) <- liftIO RL.getMousePosition
  (RL.Vector2 moveX moveY) <- liftIO RL.getMouseDelta
  leftDown <- liftIO $ RL.isMouseButtonDown RL.MouseButtonLeft
  leftPressed <- liftIO $ RL.isMouseButtonPressed RL.MouseButtonLeft
  leftReleased <- liftIO $ RL.isMouseButtonReleased RL.MouseButtonLeft
  rightDown <- liftIO $ RL.isMouseButtonDown RL.MouseButtonRight
  rightPressed <- liftIO $ RL.isMouseButtonPressed RL.MouseButtonRight
  rightReleased <- liftIO $ RL.isMouseButtonReleased RL.MouseButtonRight
  return Mouse { mousePos = (posX, posY),
                 mouseMovement = (moveX, moveY),
                 leftMouseDown = leftDown,
                 leftMousePressed = leftPressed,
                 leftMouseReleased = leftReleased,
                 rightMouseDown = rightDown,
                 rightMousePressed = rightPressed,
                 rightMouseReleased = rightReleased}

--
-- asset loading
--

data Assets = Assets {
  textures :: Map String RL.Texture,
  sounds :: Map String RL.Sound,
  fonts :: Map String (Ptr RL.Font)
}

getAssetHandle :: (String -> IO a) -> (Assets -> Map String a) -> (a -> Assets -> Assets) -> String -> Scene a
getAssetHandle loader field newAssets fileName = do
  maybeAssets <- get (Proxy :: Proxy Assets)

  case maybeAssets of
    Nothing -> error "could not get Assets"
    (Just assets) -> do
      maybe (do handle <- liftIO $ loader fileName
                resource $ newAssets handle assets
                return handle)
            return
            (Map.lookup fileName (field assets))

getTextureHandle :: String -> Scene RL.Texture
getTextureHandle fileName =
  getAssetHandle RL.loadTexture
                 textures
                 (\tex assets ->
                    assets { textures = Map.insert fileName tex (textures assets )})
                 fileName

getFontHandle :: String -> Scene (Ptr RL.Font)
getFontHandle fileName =
  getAssetHandle loadFont'
                 fonts
                 (\font assets ->
                    assets { fonts = Map.insert fileName font (fonts assets )})
                 fileName

toRaylibColor :: Color -> RL.Color
toRaylibColor (Color r g b a) =
  RL.Color (round r)
           (round g)
           (round b) (round a)

getScale :: Window -> IO Float
getScale win@Window { res = (resWidth, _)} = do
  (width, _) <- windowPixelSize win
  return (fromIntegral width / fromIntegral resWidth)

-- this is done for Z-sorting
data SpriteCommand = SpriteCommand {
  sprTexture :: RL.Texture,
  sprPosition :: V3 Float,
  sprSize :: V2 Float,
  sprAngle :: Float,
  sprTint :: RL.Color
}

-- this is done for Z-sorting
recordSprites :: [SpriteCommand] -> Sprite -> Position -> Maybe Angle -> Maybe Color -> Scene ()
recordSprites cmds (Sprite spr) pos maybeAngle tint = do
  maybeWin <- get (Proxy :: Proxy Window)
  case maybeWin of
    Nothing -> return ()
    Just win -> do
      texture <- getTextureHandle spr
      scale <- liftIO $ getScale win

      let
          texW = fromIntegral (RL.texture'width texture)
          texH = fromIntegral (RL.texture'height texture)

      resource $ SpriteCommand {
        sprTexture = texture,
        sprPosition = V3 (x pos * scale) (y pos * scale) (z pos),
        sprSize = V2 (texW * scale) (texH * scale),
        sprAngle = case maybeAngle of
          Nothing -> 0
          (Just (Angle a)) -> a,
        sprTint = toRaylibColor (fromMaybe (Color 255 255 255 255) tint)
      } : cmds

drawSprites :: [SpriteCommand] -> Scene ()
drawSprites cmds = do
  let sortedCmds = sortOn ((\(V3 _ _ z_) -> z_) . sprPosition) cmds
-- DrawTexturePro(Texture2D texture, Rectangle source, Rectangle dest,
               -- Vector2 origin, float rotation, Color tint);
  liftIO $ RL.beginBlendMode RL.BlendAlpha
  mapM_ (\cmd -> do
    liftIO $ RL.drawTexturePro (sprTexture cmd)
                            (RL.Rectangle 0 0 (fromIntegral (RL.texture'width (sprTexture cmd)))
                                              (fromIntegral (RL.texture'height (sprTexture cmd))))
                            (RL.Rectangle (x (sprPosition cmd) + (x (sprSize cmd) / 2))
                                          (y (sprPosition cmd) + (y (sprSize cmd) / 2))
                                          (x (sprSize cmd))
                                          (y (sprSize cmd)))
                            (V2 (x (sprSize cmd) / 2) (y (sprSize cmd) / 2)) (sprAngle cmd) (sprTint cmd)) sortedCmds

  liftIO RL.endBlendMode

loadFont' :: String -> IO (Ptr RL.Font)
loadFont' path = withCString path RL.c'loadFont

-- [t|Ptr Font -> CString -> Ptr Vector2 -> CFloat -> CFloat -> Ptr Color -> IO ()|]),
drawTextEx' :: Ptr RL.Font -> String -> RL.Vector2 -> Float -> Float -> RL.Color -> IO ()
drawTextEx' font text position fontSize spacing color = do
  liftIO $ with position $ \posPtr ->
    with color $ \colorPtr ->
      withCString text $ \cText ->
        RL.c'drawTextEx font cText posPtr (realToFrac fontSize) (realToFrac spacing) colorPtr

drawText :: Text -> Position -> Maybe Color -> Maybe Font -> Scene ()
drawText (Text text size align) pos color maybeFont = do
  maybeWin <- get (Proxy :: Proxy Window)
  case maybeWin of
    Nothing -> return ()
    Just win -> do

      defaultFont <- liftIO RL.c'getFontDefault
      font <- maybe (return defaultFont) (\(Font a) -> getFontHandle a) maybeFont
      scale <- liftIO $ getScale win
      let scaledSize = size * scale
      liftIO $ do
        textWidthPtr <- withCString text $ \cText ->
            RL.c'measureTextEx font cText (realToFrac scaledSize) 3
        textWidthVec <- peek textWidthPtr
        let textWidth = vector2'x textWidthVec
            position = case align of
                          AlignLeft -> V2 (x pos * scale) (y pos * scale)
                          AlignCenter -> V2 ((x pos * scale) - (textWidth / 2))
                                    (y pos * scale)
                          AlignRight -> V2 ((x pos * scale) - textWidth)
                                    (y pos * scale)

        drawTextEx' font text position scaledSize 3
          (toRaylibColor (fromMaybe (Color 255 255 255 255) color))

--
-- 3D stuff
--

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

drawCubes :: Cube -> Maybe Position -> Maybe Size -> Maybe Color -> Maybe Rotation -> Scene ()
drawCubes _ maybePos maybeSize maybeColor maybeRot = do
  let pos = fromMaybe (Position 0 0 0) maybePos
      color = fromMaybe (Color 255 255 255 255) maybeColor
      size = fromMaybe (Size 1 1 1) maybeSize
      (Rotation yaw' pitch' roll') = fromMaybe (Rotation 0 0 0) maybeRot
  
  camera <- get (Proxy :: Proxy RL.Camera3D)
  case camera of
    Nothing -> return ()
    Just cam -> liftIO $ do
      RL.beginMode3D cam
      RL.rlPushMatrix
      RL.rlTranslatef (x pos) (y pos) (z pos)
      RL.rlRotatef yaw'   0 1 0
      RL.rlRotatef pitch' 1 0 0
      RL.rlRotatef roll'  0 0 1
      RL.drawCube (V3 0 0 0) (x size) (y size) (z size) (toRaylibColor color)
      RL.rlPopMatrix
      RL.endMode3D

--
-- 2D shapes
--

drawRectangles :: Rectangle -> Position -> Size -> Maybe Color -> Scene ()
drawRectangles _ (Position x' y' _) (Size sx sy _) maybeColor = do
  maybeWin <- get (Proxy :: Proxy Window)
  case maybeWin of
    Nothing -> return ()
    Just win -> do
      scale <- liftIO $ getScale win
      liftIO $ RL.beginBlendMode RL.BlendAlpha
      liftIO $ RL.drawRectangleRec (RL.Rectangle (x' * scale) (y' * scale) (sx * scale) (sy * scale)) color
        where color = toRaylibColor (fromMaybe (Color 255 255 255 255) maybeColor)

runSystems :: Window -> Scene ()
runSystems _ = do
  system exitOnClose
  system updateTime
  system addTime
  system updateKeyboard
  system updateMouse

  liftIO RL.beginDrawing
  liftIO $ RL.clearBackground RL.black
  resource ([] :: [SpriteCommand])
  system recordSprites
  system drawSprites
  system drawText
  system drawRectangles
  system updateCameras
  system drawCubes
  liftIO RL.endDrawing

  return ()

shutdown :: Scene ()
shutdown =
  get (Proxy :: Proxy RL.WindowResources)
    >>= (liftIO . RL.closeWindow)