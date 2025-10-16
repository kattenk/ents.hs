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

    spawn (Position 0 0 5)
          (Cube)
          (Animation [Frame (Position 0 0 5),
                      Frame (Position 0 1 5),
                      Frame (Position 0 2 5),
                      Frame (Position 0 3 5),
                      Frame (Color 0 255 5 255),
                      Frame (Color 255 3 5 255),
                      Frame (Color 0 3 255 255),
                      Frame (Color 255 3 255 255)] 5)

    gameLoop $ do
      system animate