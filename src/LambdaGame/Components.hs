{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE PatternSynonyms #-}

module LambdaGame.Components (
  Position(Position, Pos), Size(..),
  Color(..), Text(..), Sprite(..)
) where

import Linear.V3

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