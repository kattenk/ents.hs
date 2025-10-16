{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE FlexibleInstances #-}

module LambdaGame.Components (
  Position(Pos, Position), Rotation(Rot, Rotation), HasXYZ(..), Velocity(Vel, Velocity),
  yaw, pitch, roll, forward, right, Size(..),
  Color(..), Text(..), Sprite(..), Cube(..), Camera3D(..), Sound(..), Angle(..), TextSize(..)
) where

import Linear.V3
import Linear.Metric (normalize)
import Linear (V2 (..))

newtype Position = Pos (V3 Float)
  deriving (Eq, Show, Num)

newtype Rotation = Rot (V3 Float)
  deriving (Eq, Show, Num)

newtype Velocity = Vel (V3 Float)
  deriving (Eq, Show, Num)

class HasXYZ a where
  x :: a -> Float
  y :: a -> Float
  z :: a -> Float

instance HasXYZ Position where
  x (Pos (V3 x_ _ _)) = x_
  y (Pos (V3 _ y_ _)) = y_
  z (Pos (V3 _ _ z_)) = z_

instance HasXYZ (V3 Float) where
  x (V3 x_ _ _) = x_
  y (V3 _ y_ _) = y_
  z (V3 _  _ x_) = x_

instance HasXYZ (V2 Float) where
  x (V2 x_ _ ) = x_
  y (V2 _ y_ ) = y_
  z (V2 _  _) = 0

instance HasXYZ Velocity where
  x (Vel (V3 x_ _ _)) = x_
  y (Vel (V3 _ y_ _)) = y_
  z (Vel (V3 _ _ z_)) = z_

yaw :: Rotation -> Float
yaw (Rot (V3 x_ _ _)) = x_

pitch :: Rotation -> Float
pitch (Rot (V3 _ y_ _)) = y_

roll :: Rotation -> Float
roll (Rot (V3 _ _ z_)) = z_

forward :: Rotation -> V3 Float
forward rot = normalize (V3 (cos (degToRadians * pitch rot) * sin (degToRadians * yaw rot))
                 (sin (degToRadians * pitch rot))
                 (cos (degToRadians * pitch rot) * cos (degToRadians * yaw rot))
              ) where degToRadians = pi / 180

right :: Rotation -> V3 Float
right rot = normalize (cross (forward rot) (V3 0 1 0))

-- For rotating Sprites
newtype Angle = Angle Float

{-# COMPLETE Position #-}
pattern Position :: Float -> Float -> Float -> Position
pattern Position x y z = Pos (V3 x y z)

{-# COMPLETE Rotation #-}
pattern Rotation :: Float -> Float -> Float -> Rotation
pattern Rotation yaw pitch roll = Rot (V3 yaw pitch roll)

{-# COMPLETE Velocity #-}
pattern Velocity :: Float -> Float -> Float -> Velocity
pattern Velocity x y z = Vel (V3 x y z)

newtype Size = Size (V3 Float)
  deriving (Eq, Show, Num)

-- {-# COMPLETE Size #-}
-- pattern Size :: Float -> Float -> Float -> Position
-- pattern Size x y z = Pos (V3 x y z)

data Color = Color Float Float Float Float
newtype Text = Text String
newtype TextSize = TextSize Float
newtype Sprite = Sprite String
data Sound = Sound String | SoundPlayed deriving Show

data Camera3D = Camera3D {
  fov :: Float
}

-- Shapes
data Cube = Cube