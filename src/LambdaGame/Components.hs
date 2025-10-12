{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE PatternSynonyms #-}

module LambdaGame.Components (
  Window(..), Backend(..), Time,
  Position(Position, Pos), Size(..),
  Color(..), Text(..), Sprite(..), Mouse(..)
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

-- This is called "Time" but it's actually the frame time (delta time)
type Time = Float

data Mouse = Mouse {
  mousePos  :: (Int, Int),
  mouseMove :: (Int, Int)
}

data Key = W | A | S | D

data Keyboard = Keyboard {
  downKeys     :: Set Key,
  pressedKeys  :: Set Key,
  releasedKeys :: Set Key
}

newtype Position = Position (V3 Float)
  deriving (Eq, Show, Num)

newtype Size = Size (V3 Float)
  deriving (Eq, Show, Num)

{-# COMPLETE Pos #-}
pattern Pos :: Float -> Float -> Float -> Position
pattern Pos x y z = Position (V3 x y z)

data Color = Color Float Float Float
newtype Text = Text String
newtype Sprite = Sprite String