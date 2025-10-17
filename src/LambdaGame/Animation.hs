{-# LANGUAGE GADTs #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE ScopedTypeVariables #-}

module LambdaGame.Animation (Frame(..), Animation(..), ToFrame(..), loop, animate) where
import Data.Typeable (Typeable, typeOf, cast)
import LambdaGame.Systems
import LambdaGame.Resources (TimeElapsed (TimeElapsed))
import LambdaGame.Scene
import Control.Monad.IO.Class (liftIO)
import Data.List (nub)
import Data.Fixed (mod')
import Control.Monad (when)
import LambdaGame.Components (Color(..), Position (..), HasXYZ (..), Rotation)
import Data.Maybe (fromMaybe)
import Linear (Additive(lerp), V3 (..))
import qualified Debug.Trace as Debug
import Debug.Trace (trace)

class Animatable a where
  tween :: a     -- ^ Start value
        -> a     -- ^ End value
        -> Float -- ^ Progress
        -> a     -- ^ Result

instance Animatable a where
  tween a _ _ = a

instance {-# OVERLAPPING #-} Animatable Position where
  tween p1 p2 progress =
    fromV3 (lerp progress (toV3 p1) (toV3 p2))

instance {-# OVERLAPPING #-} Animatable Color where
  tween p1 p2 progress =
    fromV3 (lerp progress (toV3 p1) (toV3 p2))

instance {-# OVERLAPPING #-} Animatable Rotation where
  tween p1 p2 progress =
    fromV3 (lerp progress (toV3 p1) (toV3 p2))

type family ValueType t where
  ValueType (a, _, _) = a
  ValueType (a, _) = a
  ValueType a = a

class ToFrame a where
  getValueDurationEasing :: a -> (ValueType a, Maybe Float, Maybe (Float -> Float))

instance {-# OVERLAPPABLE #-} (a ~ ValueType a) => ToFrame a where
  getValueDurationEasing v = (v, Nothing, Nothing)

instance (d ~ Float) => ToFrame (a, d) where
  getValueDurationEasing (v, d) = (v, Just (realToFrac d), Nothing)

instance (d ~ Float) => ToFrame (a, d, Float -> Float) where
  getValueDurationEasing (v, d, e) = (v, Just (realToFrac d), Just e)

data Frame =
  forall a. (Typeable a,
             ToFrame a,
             Typeable (ValueType a),
             Typeable (ReturnType (ValueType a)),
             Animatable (ValueType a)) => Frame a

instance Eq Frame where
  Frame a == Frame b = typeOf a == typeOf b

data Animation = Animation {
  frames :: [Frame],
  duration :: Float
}

-- A negative 'duration' for an 'Animation' means it will
-- last that many seconds, but loop forever
loop :: Float -> Float
loop = negate

newtype Animating = Animating { startTime :: Float }

startAnims :: Animation -> Not Animating -> TimeElapsed -> Scene ()
startAnims _ _ (TimeElapsed t) =
  set $ Animating { startTime = t }

-- | Removes useless 'Animating' component when 'Animation' is removed
cleanupAnimating :: Animating -> Not Animation -> Scene ()
cleanupAnimating a _ = remove a

runAnims :: Animation -> Animating -> TimeElapsed -> Scene ()
runAnims anim (Animating startTime) (TimeElapsed timeElapsed) = do
  -- Find all the unique components involved in the animation,
  -- and 'set' them to their correct values at this point in time
  when stillAnimating $ mapM_ runAnimationTrack (nub (frames anim))
    where
      timeInAnimation :: Float
      timeInAnimation = (timeElapsed - startTime) `mod'` abs (duration anim)

      isLooping :: Bool
      isLooping = duration anim < 0

      stillAnimating :: Bool
      stillAnimating = isLooping || ((timeElapsed - startTime) < duration anim)

      runAnimationTrack (Frame a) = do
        -- Find all the frames of this component
        let trackFrames :: [Frame]
            trackFrames = filter (\(Frame b) -> Frame a == Frame b) (frames anim)

            defaultFrameDuration :: Maybe Float -> Float
            defaultFrameDuration (Just d) = abs (duration anim) * d
            defaultFrameDuration Nothing = abs (duration anim) / fromIntegral (length trackFrames)

            getFrameDuration :: Frame -> Float
            getFrameDuration (Frame a) =
              case getValueDurationEasing a of
                (_, d, _) -> defaultFrameDuration d

            -- Get the current (first, next, progress) triplet
            getCurrentFrameInfo :: Float -> [Frame] -> Maybe (Frame, Frame, Float)
            getCurrentFrameInfo startTime' processFrames =
              case processFrames of
                [] -> Nothing
                (frame:next) ->
                  if (if isLooping then timeInAnimation else timeElapsed)
                        < (startTime' + getFrameDuration frame) then
                    Just (frame, case next of
                                   [] -> if isLooping then
                                           case trackFrames of
                                             (x:_) -> x
                                             [] -> frame
                                         else frame
                                   (x:_) -> x,
                                 (timeInAnimation - startTime') / getFrameDuration frame)
                  else
                    getCurrentFrameInfo (startTime' + getFrameDuration frame) next

        case getCurrentFrameInfo startTime trackFrames of
          (Just (Frame this, Frame next, progress :: Float)) -> do
            let (curVal, _, _) = getValueDurationEasing this
                (nextVal, _, maybeEasing) = getValueDurationEasing next
                easing = fromMaybe id maybeEasing
            curEnt <- currentEnt

            case cast nextVal of
              (Just castedNext) -> do
                let value = tween curVal castedNext (easing progress)
                set (curEnt, value)
              Nothing -> return ()
          -- Not animating
          Nothing -> return ()


animate :: Scene ()
animate = do
  system startAnims
  system cleanupAnimating
  system runAnims