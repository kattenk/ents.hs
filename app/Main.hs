{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE DoAndIfThenElse #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE FlexibleContexts #-}

module Main (main) where
import LambdaGame
import Control.Monad (when)
import System.Random (randomRIO)
import Linear.V3 (V3 (..))
import Data.Data (Proxy(..))
import Data.Dynamic (Typeable)
import Linear (Additive(lerp))

main :: IO ()
main = do
  runGame $ do
    resource $ Window {
      title = "Flappy",
      res = (144, 256),
      size = Automatic,
      targetFps = 300,
      captureCursor = False,
      backend = raylibBackend,
      exit = False
    }

    let x = V3 0 0 0
    let y = V3 0 5 0
    let z = lerp (-1) x y
    liftIO $ print z

    spawn (Position 1 0 5)
          (Cube)
          (Color 255 0 0 255)

    spawn (Position 1 3 5)
          (Cube)
          (Color 0 255 0 255)

    spawn (Position 0 0 5)
          (Cube)
          (Animation [Frame (0, Position 0 0 5),
                      Frame (50, Position 0 3 5),
                      Frame (100, Position 0 0 5)] (-1))

    gameLoop $ do
      system animate