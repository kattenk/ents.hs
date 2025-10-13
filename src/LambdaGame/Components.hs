{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE PatternSynonyms #-}

module LambdaGame.Components (
  Position(Pos, Position), Rotation(Rot, Rotation), x, y, z,
  yaw, pitch, roll, forward, right, Size(..),
  Color(..), Text(..), Sprite(..), Cube(..), Camera3D(..)
) where

import Linear.V3
import Linear.Metric (normalize)

newtype Position = Pos (V3 Float)
  deriving (Eq, Show, Num)

newtype Rotation = Rot (V3 Float)
  deriving (Eq, Show, Num)

x :: Position -> Float
x (Pos (V3 x_ _ _)) = x_

y :: Position -> Float
y (Pos (V3 _ y_ _)) = y_

z :: Position -> Float
z (Pos (V3 _ _ z_)) = z_

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

{-# COMPLETE Position #-}
pattern Position :: Float -> Float -> Float -> Position
pattern Position x y z = Pos (V3 x y z)

{-# COMPLETE Rotation #-}
pattern Rotation :: Float -> Float -> Float -> Rotation
pattern Rotation yaw pitch roll = Rot (V3 yaw pitch roll)

newtype Size = Size (V3 Float)
  deriving (Eq, Show, Num)

-- {-# COMPLETE Size #-}
-- pattern Size :: Float -> Float -> Float -> Position
-- pattern Size x y z = Pos (V3 x y z)

data Color = Color Float Float Float Float
newtype Text = Text String
newtype Sprite = Sprite String

data Camera3D = Camera3D {
  fov :: Float
}

-- Shapes
data Cube = Cube