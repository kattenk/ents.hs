module LambdaGame.Resources (
    Backend(..), Window(..), WindowSize(..), windowSize,
    Time, Key(..), Keyboard(..), Mouse(..), isPressed, TimeElapsed(..), wasPressed
) where

import LambdaGame.Scene (Scene)
import qualified Data.Set as Set
import Data.Bifunctor

data Backend = Backend {
  startBackend :: Scene (),  -- ^ Startup action
  updateBackend :: Scene (), -- ^ Action to run each frame
  stopBackend :: Scene ()    -- ^ Action to run when stopping
}

data WindowSize = Automatic | Manual (Int, Int)

data Window = Window {
  title :: String,          -- ^ Window title
  res :: (Int, Int),        -- ^ Size of the canvas, in pixels
  size :: WindowSize,       -- ^ Actual size of the window, in pixels
  targetFps :: Int,         -- ^ Target framerate
  captureCursor :: Bool,    -- ^ Capture the mouse cursor?
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

type Time = Float -- | This is called "Time" but it's actually the frame time (delta time)
newtype TimeElapsed = TimeElapsed Float -- | The total time elapsed so far

data Key = W | A | S | D | Space deriving (Show, Enum, Eq, Ord, Bounded)

data Keyboard = Keyboard {
  downKeys     :: Set.Set Key,
  pressedKeys  :: Set.Set Key,
  releasedKeys :: Set.Set Key
}

isPressed :: Key -> Keyboard -> Bool
isPressed key board = Set.member key (downKeys board)

wasPressed :: Keyboard -> Key -> Bool
wasPressed board key = Set.member key (pressedKeys board)

data Mouse = Mouse {
  mousePos :: (Float, Float),
  mouseMovement :: (Float, Float),
  leftMouseDown :: Bool,
  leftMousePressed :: Bool,
  leftMouseReleased :: Bool,
  rightMouseDown :: Bool,
  rightMousePressed :: Bool,
  rightMouseReleased :: Bool
} deriving Show