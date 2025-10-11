{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE PatternSynonyms #-}

module LambdaGame.Components (
  Window(..), Backend(..), Time,
  Position(Position, Pos), Size(..),
  Color(..), Text(..), Sprite(..)
) where

import LambdaGame.Scene (Scene)
import Linear.V3

data Backend = Backend {
  startBackend :: Scene (),  -- ^ Startup action
  updateBackend :: Scene (), -- ^ Action to run each frame
  stopBackend :: Scene ()    -- ^ Action to run when stopping
}

data Window = Window {
  title :: String,      -- ^ Window title
  size :: (Int, Int),   -- ^ Size of the canvas, in pixels
  fps :: Int,           -- ^ Target framerate
  backend :: Backend,   -- ^ What back end should the game use?
  exit :: Bool          -- ^ Should the game loop exit at the end of this frame?
}

-- This is called "Time" but it's actually the frame time
type Time = Float

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