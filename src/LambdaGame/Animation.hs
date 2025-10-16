{-# LANGUAGE GADTs #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE TypeOperators #-}
module LambdaGame.Animation (Frame(..), Animation(..), ToFrame(..), loop, animate) where
import Data.Typeable (Typeable, typeOf)
import LambdaGame.Systems
import LambdaGame.Resources (TimeElapsed (TimeElapsed))
import LambdaGame.Scene
import Control.Monad.IO.Class (liftIO)
import Data.List (nub)
import Data.Fixed (mod')

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
  forall a. (Typeable a, Typeable (ReturnType a),
             ToFrame a,
             Typeable (ValueType a),
             Typeable (ReturnType (ValueType a))) => Frame a

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
  mapM_ runAnimationTrack (nub (frames anim))
    where
      timeInAnimation :: Float
      timeInAnimation = (timeElapsed - startTime) `mod'` abs (duration anim)

      isLooping :: Bool
      isLooping = duration anim < 0

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

            -- Get the current (first, next) frame pair
            getCurrentFramePair :: Float -> [Frame] -> Maybe (Frame, Frame)
            getCurrentFramePair startTime' processFrames =
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
                                   (x:_) -> x)
                  else
                    getCurrentFramePair (startTime' + getFrameDuration frame) next
      
        case getCurrentFramePair startTime trackFrames of
          (Just (Frame this, Frame next)) -> do
            let (value, _, easing) = getValueDurationEasing this
            curEnt <- currentEnt
            set (curEnt, value)
          -- Not animating
          Nothing -> return ()

animate :: Scene ()
animate = do
  system startAnims
  system cleanupAnimating
  system runAnims