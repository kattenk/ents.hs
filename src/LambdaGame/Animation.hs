{-# LANGUAGE GADTs #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE ScopedTypeVariables #-}

module LambdaGame.Animation (Frame(..), Animation(..), FrameSpec(..), loop, animate) where
import Data.Typeable (Typeable, typeOf, cast)
import LambdaGame.Systems
import LambdaGame.Resources (TimeElapsed (TimeElapsed))
import LambdaGame.Scene
import Data.List (nub, (!?))
import Data.Fixed (mod')
import LambdaGame.Components (Color(..), Position (..), HasXYZ (..), Rotation)
import Data.Maybe (fromMaybe)
import Linear (Additive(lerp))
import Linear.V4 (V4(..))

class Animatable a where
  tween :: a     -- ^ Start value
        -> a     -- ^ End value
        -> Float -- ^ Progress
        -> a     -- ^ Result

  stepped :: a -> Bool
  stepped _ = False

instance Animatable a where
  tween a _ _ = a
  stepped _ = True

instance {-# OVERLAPPING #-} Animatable Position where
  tween p1 p2 progress =
    fromV3 (lerp progress (toV3 p1) (toV3 p2))

instance {-# OVERLAPPING #-} Animatable Color where
  tween (Color sr sg sb sa) (Color er eg eb ea) progress =
    (\(V4 r g b a) -> Color r g b a) (lerp progress (V4 sr sg sb sa) (V4 er eg eb ea) )

instance {-# OVERLAPPING #-} Animatable Rotation where
  tween p1 p2 progress =
    fromV3 (lerp progress (toV3 p1) (toV3 p2))

type family ValueType t where
  ValueType (Float, a) = a
  ValueType a = a

class FrameSpec a where
  time :: a -> Maybe Float
  value :: a -> ValueType a

instance {-# OVERLAPPABLE #-} (a ~ ValueType a) => FrameSpec a where
  time _ = Nothing
  value a = a

instance (t ~ Float) => FrameSpec (t, a) where
  time (t, _) = Just t
  value (_, a) = a

data Frame =
  forall a. (Typeable a,
             FrameSpec a,
             Typeable (ValueType a),
             Typeable (ReturnType (ValueType a)),
             Typeable (ReturnType a),
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
runAnims anim (Animating startTime') (TimeElapsed timeElapsed) = do
  -- Find all the unique components involved in the animation,
  -- and set them to their correct values at this point in time
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

      divisions :: Int -> [Float]
      divisions n = [ fromIntegral i * 100 / fromIntegral n | i <- [0..n] ]

      applyAnimation :: Frame -> Frame -> Float -> Scene ()
      applyAnimation (Frame currentFrame) (Frame nextFrame) progress = do
        let currentValue = value currentFrame
            nextVal = value nextFrame

        case cast nextVal of
          (Just castedNext) -> do
            curEnt <- currentEnt
            set (curEnt, tween currentValue castedNext (progress * 0.01))
          Nothing -> return ()

      runAnimationTrack :: Frame -> Scene ()
      runAnimationTrack (Frame a) = do
        let trackFrames = filter (\(Frame b) -> Frame a == Frame b) (frames anim)
            framesWithTimes = zipWith (curry (\(index, f@(Frame b)) ->
                (case time b of
                  (Just t) -> t
                  Nothing -> if stepped b then
                              100 / fromIntegral (length trackFrames) * fromIntegral index
                             else
                              -- this may be wrong I messed with it
                              divisions (length trackFrames - 1) !! index, f))) [0..] trackFrames
            previousFrames = filter ((< animationPercentage) . fst) framesWithTimes
            nextFrames     = filter ((> animationPercentage) . fst) framesWithTimes

        case framesWithTimes !? 0 of
          (Just firstFrame) ->
            -- this system re-uses the first frame when it shouldn't
            -- which can cause unexpected results
            let (frameStart, currentFrame) = fromMaybe firstFrame $ previousFrames !? (length previousFrames - 1)
                (frameEnd, nextFrame) = fromMaybe (if isLooping then
                                                     firstFrame else (frameStart, currentFrame))
                                                  $ nextFrames !? 0
                progress = (animationPercentage - frameStart)
                              / (frameEnd - frameStart) * 100 in

            applyAnimation currentFrame nextFrame progress
          Nothing -> return ()

animate :: Scene ()
animate = do
  system startAnims
  system cleanupAnimating
  system runAnims