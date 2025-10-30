{-# LANGUAGE StandaloneDeriving #-}
{-# OPTIONS_GHC -Wno-orphans #-}
-- |
-- Module      : Ents.Resources
-- License     : MIT
-- 
-- Resources that come with the engine,
-- resources are global state that can be injected
-- into any system (like Time)

module Ents.Resources (
  Window(..), WindowSize(..), defaultWindow,
  Time, TimeElapsed(..), Keyboard(..), isPressed, wasPressed, Mouse(..)
) where

import qualified Data.Set as Set
import qualified Raylib.Types as RL

data WindowSize
  = ExactWinSize (Int, Int) -- ^ In pixels
  | RelativeSize Int        -- ^ Screen percentage (Horizontal)

data Window = Window {
  title :: String,          -- ^ Window title
  res :: (Int, Int),        -- ^ Resolution of the canvas, in pixels
  windowSize :: WindowSize, -- ^ How much of the screen the window takes up
  targetFps :: Int,         -- ^ Target framerate
  captureCursor :: Bool,    -- ^ Capture the mouse cursor?
  exit :: Bool              -- ^ Should the game exit at the end of this frame?
}

defaultWindow :: Window
defaultWindow = Window {
  title = "no title heheheheheh",
  res = (100, 100),
  windowSize = RelativeSize 30,
  targetFps = 300,
  captureCursor = False,
  exit = False
}

-- data Time = Time { deltaTime :: Float, elapsedTime :: Float }

-- Separate types and delta as a type alias for Float, because I can't be bothered
-- to see if there's any way to do it how i actually want it
type Time = Float -- | This is called "Time" but it's actually the (delta)time
newtype TimeElapsed = TimeElapsed Float -- | The total time elapsed so far

data Keyboard = Keyboard {
  downKeys     :: Set.Set RL.KeyboardKey,
  pressedKeys  :: Set.Set RL.KeyboardKey,
  releasedKeys :: Set.Set RL.KeyboardKey
}

deriving instance Ord RL.KeyboardKey

isPressed :: RL.KeyboardKey -> Keyboard -> Bool
isPressed key board = Set.member key (downKeys board)

wasPressed :: Keyboard -> RL.KeyboardKey -> Bool
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