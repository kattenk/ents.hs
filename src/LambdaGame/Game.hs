{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE NamedFieldPuns #-}
module LambdaGame.Game ( runGame, gameLoop, GameLoop(..) ) where

import LambdaGame.Scene (Scene, SceneState(..), runScene, resource, get)
import qualified Data.Map as Map
import Data.Data (Proxy(..))
import Control.Monad (unless, when)
import Data.Time.Clock (getCurrentTime, diffUTCTime)
import Control.Monad.IO.Class (liftIO)
import Control.Concurrent (threadDelay)

initialState :: SceneState
initialState = SceneState
  { resources = Map.empty,
    components = Map.empty,
    entityCount = 0,
    growComponents = return (),
    clearEntity = \_ -> return (),
    reusableIndices = [],
    currentEntity = 0
  }

runGame :: Scene a -> IO ()
runGame game = do
  _ <- runScene initialState game
  return ()

data GameLoop = GameLoop {
  loopRate :: Int,
  exit :: Bool
}

gl :: GameLoop
gl = GameLoop { loopRate = 60, exit = False }

gameLoop :: Scene a -> Scene ()
gameLoop userLoop = do
  resource gl
  loop where
    targetFrameTime = 1.0 / fromIntegral (loopRate gl)
    loop = do
      startTime <- liftIO getCurrentTime
      _ <- userLoop
      endTime <- liftIO getCurrentTime
      let elapsed = realToFrac $ diffUTCTime endTime startTime
      let remaining = targetFrameTime - elapsed
      runAgain <- get (Proxy @GameLoop)
      case runAgain of
        (Just (GameLoop { exit })) -> do
          when (remaining > 0) $ do
            liftIO $ threadDelay (round (remaining * 1000000))
          unless exit loop
        Nothing -> return ()