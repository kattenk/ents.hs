{-# LANGUAGE GADTs #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE ScopedTypeVariables #-}
module LambdaGame.Animation (Frame(..), Animatable(..), loop) where
import Data.Typeable (Typeable)
import LambdaGame.Scene (Scene)
import LambdaGame.Components (Sprite(..))

class Animatable a where
  tween :: a     -- ^ Start value
        -> a     -- ^ End value
        -> Float -- ^ Progress
        -> a     -- ^ Result

instance Animatable a where
  tween _ a _ = a

data Frame =
  forall a. (Typeable a, Animatable a) => Frame a

data Animation = Animation {
  frames :: [Frame],
  duration :: Float
}

loop :: Float -> Float
loop = negate