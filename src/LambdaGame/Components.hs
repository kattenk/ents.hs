module LambdaGame.Components ( Window(..), Backend(..)) where
import LambdaGame.Scene (Scene)

data Backend = Backend {
  startBackend :: Scene (),  -- ^ Startup action
  updateBackend :: Scene ()  -- ^ Action to run each frame
}

data Window = Window {
  title :: String,    -- ^ Window title
  size :: (Int, Int), -- ^ Size of the canvas, in pixels
  fps :: Int,         -- ^ Target framerate,
  backend :: Backend, -- ^ What back end should the game use?
  exit :: Bool        -- ^ Should the game loop exit?
}