{-# LANGUAGE GeneralizedNewtypeDeriving, PatternSynonyms, FlexibleInstances, UndecidableInstances #-}
-- |
-- Module      : Ents.Components
-- License     : MIT
-- 
-- Components that come with the engine

module Ents.Components (
  Position(Pos, Position), Rotation(Rot, Rotation),
  HasXYZ(..), Velocity(Vel, Velocity),
  yaw, pitch, roll, forward, right, Size(Size', Size),
  Color(Color', Color), Angle(..), Text(..), TextAlignment(..),
  Sprite(..), Font(..), Sound(..), Camera3D(..), Cube(..), Rectangle(..)
) where

import Linear

newtype Font = Font String
newtype Sprite = Sprite String
newtype Sound = Sound String

newtype Position = Pos (V3 Float)
  deriving (Eq, Show, Num)

newtype Rotation = Rot (V3 Float)
  deriving (Eq, Show, Num)

newtype Velocity = Vel (V3 Float)
  deriving (Eq, Show, Num)

newtype Color = Color' (V4 Float)
  deriving (Eq, Show, Num)

newtype Angle = Angle Float

newtype Size = Size' (V3 Float)
  deriving (Eq, Show, Num)

{-# COMPLETE Color #-}
pattern Color :: Float -> Float -> Float -> Float -> Color
pattern Color r g b a = Color' (V4 r g b a)

class HasXYZ a where
  x :: a -> Float
  y :: a -> Float
  z :: a -> Float
  toV3 :: a -> V3 Float
  fromV3 :: V3 Float -> a

instance HasXYZ Position where
  x (Pos (V3 x_ _ _)) = x_
  y (Pos (V3 _ y_ _)) = y_
  z (Pos (V3 _ _ z_)) = z_
  toV3 (Pos (V3 x_ y_ z_)) = V3 x_ y_ z_
  fromV3 (V3 x_ y_ z_) = Position x_ y_ z_

instance HasXYZ Rotation where
  x (Rot (V3 x_ _ _)) = x_
  y (Rot (V3 _ y_ _)) = y_
  z (Rot (V3 _ _ z_)) = z_
  toV3 (Rot (V3 x_ y_ z_)) = V3 x_ y_ z_
  fromV3 (V3 x_ y_ z_) = Rotation x_ y_ z_

instance HasXYZ Velocity where
  x (Vel (V3 x_ _ _)) = x_
  y (Vel (V3 _ y_ _)) = y_
  z (Vel (V3 _ _ z_)) = z_
  toV3 (Vel (V3 x_ y_ z_)) = V3 x_ y_ z_
  fromV3 (V3 x_ y_ z_) = Velocity x_ y_ z_

instance HasXYZ Size where
  x (Size' (V3 x_ _ _)) = x_
  y (Size' (V3 _ y_ _)) = y_
  z (Size' (V3 _ _ z_)) = z_
  toV3 (Size' (V3 x_ y_ z_)) = V3 x_ y_ z_
  fromV3 (V3 x_ y_ z_) = Size x_ y_ z_

instance HasXYZ Color where
  x (Color' (V4 x_ _ _ _)) = x_
  y (Color' (V4 _ y_ _ _)) = y_
  z (Color' (V4 _ _ z_ _)) = z_
  toV3 (Color' (V4 x_ y_ z_ _)) = V3 x_ y_ z_
  fromV3 (V3 x_ y_ z_) = Color x_ y_ z_ 255

{-# COMPLETE Position #-}
pattern Position :: Float -> Float -> Float -> Position
pattern Position x y z = Pos (V3 x y z)

{-# COMPLETE Rotation #-}
pattern Rotation :: Float -> Float -> Float -> Rotation
pattern Rotation yaw pitch roll = Rot (V3 yaw pitch roll)

{-# COMPLETE Velocity #-}
pattern Velocity :: Float -> Float -> Float -> Velocity
pattern Velocity x y z = Vel (V3 x y z)

{-# COMPLETE Size #-}
pattern Size :: Float -> Float -> Float -> Size
pattern Size x y z = Size' (V3 x y z)

instance HasXYZ (V3 Float) where
  x (V3 x_ _ _) = x_
  y (V3 _ y_ _) = y_
  z (V3 _  _ x_) = x_
  toV3 = id
  fromV3 = id

instance HasXYZ (V2 Float) where
  x (V2 x_ _) = x_
  y (V2 _ y_) = y_
  z (V2 _  _) = 0
  toV3 (V2 x_ y_) = V3 x_ y_ 0
  fromV3 (V3 x_ y_ _) = V2 x_ y_

yaw :: Rotation -> Float
yaw (Rot (V3 x_ _ _)) = x_

pitch :: Rotation -> Float
pitch (Rot (V3 _ y_ _)) = y_

roll :: Rotation -> Float
roll (Rot (V3 _ _ z_)) = z_

forward :: Rotation -> V3 Float
forward rot = normalize
  (V3 (cos (degToRadians * pitch rot) * sin (degToRadians * yaw rot))
      (sin (degToRadians * pitch rot))
      (cos (degToRadians * pitch rot) * cos (degToRadians * yaw rot)))
      where degToRadians = pi / 180

right :: Rotation -> V3 Float
right rot = normalize (cross (forward rot) (V3 0 1 0))

-- | These are broken.. i think
data TextAlignment = AlignLeft | AlignRight | AlignCenter

data Text = Text {
  textContent :: String,
  textSize :: Float,
  alignment :: TextAlignment
}

data Camera3D = Camera3D {
  fov :: Float
}

-- Shapes
data Cube = Cube
data Rectangle = Rectangle