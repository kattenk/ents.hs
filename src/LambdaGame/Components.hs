module LambdaGame.Components ( Window(..), Backend(..), Time(..)) where
import LambdaGame.Scene (Scene)

data Backend = Backend {
  startBackend :: Scene (),  -- ^ Startup action
  updateBackend :: Scene (), -- ^ Action to run each frame
  stopBackend :: Scene ()    -- ^ Action to run when stopping
}

data Window = Window {
  title :: String,      -- ^ Window title
  size :: (Int, Int),   -- ^ Size of the canvas, in pixels
  fps :: Int,           -- ^ Target framerate,
  backend :: Backend,   -- ^ What back end should the game use?
  escapeToExit :: Bool, -- ^ Should the escape key exit the game?
  exit :: Bool          -- ^ Should the game loop exit at the end of this frame?
}

-- This is called "Time" but it's actually "frame time" -- how long
-- did the previous frame take
newtype Time = Time Float