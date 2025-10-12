module LambdaGame.Resources (
    Backend(..), Window(..), WindowSize(..), windowSize, RenderMode(..), Time
) where

import LambdaGame.Scene (Scene)
import Data.Set (Set)
import Data.Bifunctor

data Backend = Backend {
  startBackend :: Scene (),  -- ^ Startup action
  updateBackend :: Scene (), -- ^ Action to run each frame
  stopBackend :: Scene ()    -- ^ Action to run when stopping
}

data WindowSize = Automatic | Manual (Int, Int)
data RenderMode = Snap   -- ^ Renders whole screen at 'res' and scales it
                | Smooth -- ^ Scales Sprites etc individually, allowing them to move smoothly

data Window = Window {
  title :: String,          -- ^ Window title
  res :: (Int, Int),        -- ^ Size of the canvas, in pixels
  size :: WindowSize,       -- ^ Actual size of the window, in pixels
  renderMode :: RenderMode, -- ^ Scaling strategy to use
  targetFps :: Int,         -- ^ Target framerate
  backend :: Backend,       -- ^ What backend should the game use?
  exit :: Bool              -- ^ Should the game exit at the end of this frame?
}

windowSize :: (Int, Int)
           -> Window
           -> (Int, Int)
windowSize screenSize window =
  case size window of
    Manual dimensions -> dimensions
    Automatic -> bimap (6 *) (6 *) (res window)

-- | This is called "Time" but it's actually the frame time (delta time)
type Time = Float

data Key = W | A | S | D

data Keyboard = Keyboard {
  downKeys     :: Set Key,
  pressedKeys  :: Set Key,
  releasedKeys :: Set Key
}