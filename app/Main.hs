{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE DoAndIfThenElse #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE FlexibleContexts #-}

module Main (main) where
import qualified LambdaGame as LG
import Control.Monad (when)
import System.Random (randomRIO)
import Linear.V3 (V3 (..))
import Data.Data (Proxy(..))
import Data.Dynamic (Typeable)
import Linear (Additive(lerp))

data Animation = Animation Float    -- ^ Anim speed
                           [String] -- ^ Frames

animate :: LG.Sprite -> Animation -> LG.TimeElapsed -> LG.Sprite
animate (LG.Sprite spr) (Animation speed frames) (LG.TimeElapsed elapsed) =
  let currentFrame = floor (elapsed / speed) `mod` length frames
  in LG.Sprite (frames !! currentFrame)

main :: IO ()
main = do
  LG.runGame $ do
    LG.resource $ LG.Window {
      LG.title = "Flappy",
      LG.res = (144, 256),
      LG.size = LG.Automatic,
      LG.targetFps = 300,
      LG.captureCursor = False,
      LG.backend = LG.raylibBackend,
      LG.exit = False
    }

    let x = V3 0 0 0
    let y = V3 0 5 0
    let z = lerp (-1) x y
    LG.liftIO $ print z

    LG.spawn (LG.Position 1 0 5)
          (LG.Cube)
          (LG.Color 255 0 0 255)

    LG.spawn (LG.Position 1 3 5)
          (LG.Cube)
          (LG.Color 0 255 0 255)

    LG.spawn (LG.Position 40 80 5)
          (LG.Cube)

          (LG.Animation [LG.Frame (LG.Sprite "birdFlapUp.png"),
                      LG.Frame (LG.Sprite "bird.png"),
                      LG.Frame (LG.Sprite "birdFlapDown.png"),
                      LG.Frame (LG.Sprite "bird.png")] (-0.4))
    
    LG.spawn
      (LG.Position 60 80 5)
      (LG.Sprite "bird.png")
      (Animation 0.1 ["birdFlapUp.png",
                             "bird.png",
                             "birdFlapDown.png",
                             "bird.png"])

    LG.gameLoop $ do
      LG.system LG.animate
      LG.system animate