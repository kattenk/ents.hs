{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE PatternSynonyms #-}

module LambdaGame.Components (
  Position(Pos, Position), x, y, z, Size(..),
  Color(..), Text(..), Sprite(..)
) where

import Linear.V3

newtype Position = Pos (V3 Float)
  deriving (Eq, Show, Num)

x :: Position -> Float
x (Pos (V3 x_ _ _)) = x_

y :: Position -> Float
y (Pos (V3 _ y_ _)) = y_

z :: Position -> Float
z (Pos (V3 _ _ z_)) = z_

{-# COMPLETE Position #-}
pattern Position :: Float -> Float -> Float -> Position
pattern Position x y z = Pos (V3 x y z)

newtype Size = Size (V3 Float)
  deriving (Eq, Show, Num)

-- {-# COMPLETE Size #-}
-- pattern Size :: Float -> Float -> Float -> Position
-- pattern Size x y z = Pos (V3 x y z)

data Color = Color Float Float Float
newtype Text = Text String
newtype Sprite = Sprite String