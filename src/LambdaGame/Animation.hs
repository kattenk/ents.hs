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
import Data.List (nub, (!?), findIndex)
import Data.Fixed (mod')
import Control.Monad (when)
import LambdaGame.Components (Color(..), Position (..), HasXYZ (..), Rotation)
import Data.Maybe (fromMaybe)
import Linear (Additive(lerp), V3 (..))
import Debug.Trace (trace)
import Data.Function ((&))

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
  ValueType (Float, a) = a
  ValueType a = a

class ToFrame a where
  toFrame :: a -> (Maybe Float, ValueType a)

instance {-# OVERLAPPABLE #-} (a ~ ValueType a) => ToFrame a where
  toFrame v = (Nothing, v)

instance (p ~ Float) => ToFrame (p, a) where
  toFrame (p, a) = (Just p, a)

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
runAnims (Animation { frames = [] }) _ _ = return ()
runAnims anim@(Animation { frames = (_firstFrame:_) }) (Animating startTime') (TimeElapsed timeElapsed) = do
  -- Find all the unique components involved in the animation,
  -- and 'set' them to their correct values at this point in time
  if stillAnimating then do mapM_ runAnimationTrack (nub (frames anim)) else
    remove anim
    where
      animationPercentage :: Float
      animationPercentage = (((timeElapsed - startTime')
                              `mod'` abs (duration anim)) / abs (duration anim)) * 100

      isLooping :: Bool
      isLooping = duration anim < 0

      stillAnimating :: Bool
      stillAnimating = isLooping || ((timeElapsed - startTime') < duration anim)

      applyAnimation :: Frame -> Frame -> Float -> Scene ()
      applyAnimation (Frame currentFrame) (Frame nextFrame) progress = do
        let (_, currentValue) = toFrame currentFrame
            (_, nextVal) = toFrame nextFrame

        case cast nextVal of
          (Just castedNext) -> do
            curEnt <- currentEnt
            set (curEnt, tween currentValue castedNext (progress * 0.01))
          Nothing -> liftIO $ print "what"

      runAnimationTrack :: Frame -> Scene ()
      runAnimationTrack (Frame a) = do
        -- Find all the frames of this component
        let allFrames = zip [0 ..] (filter (\(Frame b) -> Frame a == Frame b) (frames anim))
            percentages = map (\(index, Frame b) -> case toFrame b of
              (Just p, _) -> p
              (Nothing, _) -> (100 / fromIntegral (length allFrames)) * index) allFrames
            trackFrames = zip percentages (map snd allFrames)
            (previousFrames, nextFrames)
              = splitAt (fromMaybe 0 $ findIndex ((> animationPercentage) . fst) trackFrames) trackFrames

            currentFrame = case previousFrames of
              [] -> error "no previous frames"
              l -> snd (last l)

            nextFrame = case nextFrames of
              [] -> error "no previous frames"
              (f:_) -> snd f

            progress = (animationPercentage - fst (last previousFrames)) / (fst (head nextFrames) - fst (last previousFrames)) * 100

        applyAnimation currentFrame nextFrame progress

animate :: Scene ()
animate = do
  system startAnims
  system cleanupAnimating
  system runAnims