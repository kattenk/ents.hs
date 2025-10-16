{-# LANGUAGE GADTs #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE TypeOperators #-}
module LambdaGame.Animation (Frame(..), Animation(..), ToFrame(..), loop, animate) where
import Data.Typeable (Typeable, typeOf)
import LambdaGame.Scene (Scene)
import LambdaGame.Systems
import LambdaGame.Resources (TimeElapsed (TimeElapsed))
import LambdaGame.Scene
import Control.Monad.IO.Class (liftIO)
import Data.List (nub)
import Data.Maybe (fromMaybe)

type family ValueType t where
  ValueType (a, _, _) = a
  ValueType (a, _) = a
  ValueType a = a

class ToFrame a where
  getValueDurationEasing :: a -> (ValueType a, Maybe Float, Maybe (Float -> Float))

instance {-# OVERLAPPABLE #-} (a ~ ValueType a) => ToFrame a where
  getValueDurationEasing v = (v, Nothing, Nothing)

instance (Real d) => ToFrame (a, d) where
  getValueDurationEasing (v, d) = (v, Just (realToFrac d), Nothing)

instance (Real d) => ToFrame (a, d, Float -> Float) where
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

findCurrentFrame :: Float -> Float -> Float -> [Frame] -> Int -> Maybe Frame
findCurrentFrame timeElapsed startTime animDuration frames numFrames =
  case frames of
    [] -> Nothing
    (frame:next) -> case frame of
      (Frame fr) ->
        let frameDuration = case getValueDurationEasing fr of
              (_, d, _) -> fromMaybe (animDuration / fromIntegral numFrames) d in
        if timeElapsed < startTime + frameDuration then
          Just frame
        else
          findCurrentFrame timeElapsed (startTime + frameDuration) animDuration next numFrames

runAnims :: Animation -> Animating -> TimeElapsed -> Scene ()
runAnims anim (Animating startTime) (TimeElapsed timeElapsed) = do
  -- Find all the unique components involved in the animation,
  -- and 'set' them to their correct values at this point in time
  mapM_ runAnimationTrack (nub (frames anim))
    where
      runAnimationTrack (Frame a) = do
        -- Find all the frames of this component
        let trackFrames = filter (\(Frame b) -> Frame a == Frame b) (frames anim)
            currentFrame = findCurrentFrame timeElapsed startTime (duration anim) trackFrames (length trackFrames)
        
        case currentFrame of
          Just (Frame fr) ->
            (case getValueDurationEasing fr of
              (v, _, _) -> do
                curEnt <- currentEnt
                set (curEnt, v))
          Nothing -> return ()
        return ()

animate :: Scene ()
animate = do
  system startAnims
  system cleanupAnimating
  system runAnims