module LambdaGame.Backend.Raylib ( raylibBackend ) where

import LambdaGame.Components (Backend(..))
import LambdaGame.Scene (Scene)
import Control.Monad.IO.Class (liftIO)

startRaylib :: Scene ()
startRaylib =
  liftIO $ putStrLn "Starting raylib"

updateRaylib :: Scene ()
updateRaylib =
  liftIO $ putStrLn "updating raylib"

raylibBackend :: Backend
raylibBackend = Backend { startBackend = startRaylib,
                          updateBackend = updateRaylib}