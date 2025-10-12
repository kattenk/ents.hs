module LambdaGame.Resources (
    Backend(..), Window(..), Time
) where

import LambdaGame.Scene (Scene)
import Linear.V3
import Data.Set

data Backend = Backend {
  startBackend :: Scene (),  -- ^ Startup action
  updateBackend :: Scene (), -- ^ Action to run each frame
  stopBackend :: Scene ()    -- ^ Action to run when stopping
}

data Window = Window {
  title :: String,      -- ^ Window title
  res :: (Int, Int),    -- ^ Size of the canvas, in pixels
  size :: (Int, Int),   -- ^ Actual size of the window, in pixels
  targetFps :: Int,     -- ^ Target framerate
  backend :: Backend,   -- ^ What back end should the game use?
  exit :: Bool          -- ^ Should the game loop exit at the end of this frame?
}

-- | This is called "Time" but it's actually the frame time (delta time)
type Time = Float

data Key = W | A | S | D

data Keyboard = Keyboard {
  downKeys     :: Set Key,
  pressedKeys  :: Set Key,
  releasedKeys :: Set Key
}