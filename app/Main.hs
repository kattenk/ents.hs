{-# LANGUAGE DoAndIfThenElse #-}
{-# LANGUAGE TypeApplications #-}
module Main (main) where
import LambdaGame
import Control.Monad (when)
import System.Random (randomRIO)
import Linear.V3 (V3 (..))
import Data.Data (Proxy(..))

-- Game parameters
scrollSpeed = 55
pipeGap = 50
pipeRate = 1.2
flapForce = -200
gravityForce = 650

-- | Timer for spawning pipes
newtype PipeTimer = PipeTimer Float

-- | A marker component for the bird, not technically necessary here
data Bird = Bird

data Scroll = Scroll           -- ^ Moves leftwards forever
            | ScrollRepeat Int -- ^ Move leftwards until a reset point

data Collider = Collider Float Float Float Float

-- | Applies gravity
gravity :: Velocity -> Time -> Velocity
gravity vel t = vel + Velocity 0 (gravityForce * t) 0

-- | Applies velocity
move :: Velocity -> Position -> Time -> Position
move (Velocity x y z) pos t =
  pos + Position (x * t) (y * t) (z * t)

-- | Flaps the bird
flap :: Bird -> Velocity -> Keyboard -> Velocity
flap _ vel keyboard =
  if wasPressed keyboard Space then
    Velocity 0 flapForce 0
  else
    vel

-- | Moves the pipes and the ground
scroll :: Scroll -> Position -> Time -> Position
scroll Scroll pos t =
  Position (x pos - (scrollSpeed * t)) (y pos) (z pos)
scroll (ScrollRepeat resetPoint) pos t =
  if x pos > fromIntegral resetPoint then
    Position (x pos - (scrollSpeed * t)) (y pos) (z pos)
  else
    Position 0 (y pos) (z pos)

-- | Spawns new pipes when PipeTimer reaches zero
spawnPipes :: PipeTimer -> Time -> Scene ()
spawnPipes (PipeTimer timer) timeStep = do
  resource (PipeTimer (timer - timeStep))
  when (timer <= 0) $ do
    let pipeSprHeight = 200
        pipeEndHeight = 12
    gapCenterY <- liftIO $ randomRIO
      (pipeEndHeight + (pipeGap / 2),
       pipeSprHeight - (pipeEndHeight + (pipeGap / 2)))

    spawn (Sprite "pipeBottom.png")
          (Position 144 (gapCenterY + (pipeGap / 2)) 0.5)
          (Collider 0 0 26 200)
          Scroll

    spawn (Sprite "pipeTop.png")
          (Position 144 ((-pipeSprHeight) + (gapCenterY - (pipeGap / 2))) 0.5)
          (Collider 0 0 26 200)
          Scroll

    resource (PipeTimer pipeRate)

updateColliders :: Position -> Collider -> Collider
updateColliders pos (Collider _ _ w h) = Collider (x pos) (y pos) w h

collision :: Bird -> Collider -> Every Collider -> Time -> Scene ()
collision _ (Collider x1 y1 w1 h1) (Every colliders) t = do
  let overlaps = map (not . \(Collider x2 y2 w2 h2)
        -> x1 + w1 <= x2 ||
           x2 + w2 <= x1 ||
           y1 + h1 <= y2 ||
           y2 + h2 <= y1) colliders
  when (or overlaps) $ do
    liftIO $ print $ "You're dead " ++ show t

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

    resource (PipeTimer pipeRate)

    spawn (Sprite "backdrop.png")
          (Position 0 0 0)

    spawn (Sprite "ground.png")
          (Position 0 200 1)
          (ScrollRepeat (-24))
          (Collider 0 0 168 56)

    spawn Bird
          (Sprite "bird.png")
          (Position 20 116 1)
          (Velocity 0 0 0)
          (Collider 0 0 17 12)

    gameLoop $ do
      system gravity
      system move
      system flap
      system scroll
      system spawnPipes
      system updateColliders
      system collision