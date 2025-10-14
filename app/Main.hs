{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE DoAndIfThenElse #-}

module Main (main) where
import LambdaGame
import Control.Monad (when)
import System.Random (randomRIO)
import Linear.V3 (V3 (..))
import Data.Data (Proxy(..))

-- MUST REMEMBER TO FIX RESOLUTION ON 1080p

-- Game parameters
scrollSpeed = 55
pipeGap = 50
pipeRate = 1.2
flapForce = -200
gravityForce = 650

-- | Timer for spawning pipes
newtype PipeTimer = PipeTimer Float

-- Marker components
data Bird = Bird -- ^ For the bird (when playing)
data Tap = Tap   -- ^ Relating to the "Tap" graphic at the start of the game

data Scroll = Scroll           -- ^ Moves leftwards forever
            | ScrollRepeat Int -- ^ Move leftwards until a reset point

data Collider = Collider Float Float Float Float
data Animation = Animation Float    -- ^ Anim speed
                           [String] -- ^ Frames

isFlapping :: Keyboard -> Mouse -> Bool
isFlapping kb mouse =
  wasPressed kb Space || leftMousePressed mouse

birdAnimation = ["birdFlapUp.png",
                 "bird.png",
                 "birdFlapDown.png",
                 "bird.png"]

-- | Applies gravity
gravity :: Velocity -> Time -> Velocity
gravity vel t = vel + Velocity 0 (gravityForce * t) 0

-- | Applies velocity
move :: Velocity -> Position -> Time -> Position
move (Velocity x y z) pos t =
  pos + Position (x * t) (y * t) (z * t)

-- | Flaps the bird
flap :: Bird -> Velocity -> Keyboard -> Mouse -> Velocity
flap _ vel keyboard mouse =
  if isFlapping keyboard mouse then
    Velocity 0 flapForce 0
  else
    vel

flapSound :: Bird -> Keyboard -> Mouse -> Maybe Sound
flapSound _ keyboard mouse =
  if isFlapping keyboard mouse then
    Just (Sound "flap.ogg")
  else
    Nothing

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
spawnPipes :: Bird -> PipeTimer -> Time -> Scene ()
spawnPipes _ (PipeTimer timer) timeStep = do
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

-- | For despawning any entities that go offscreen to the left
despawnPipes :: Position -> Scene ()
despawnPipes pos = when (x pos < -26) despawn

-- | Updates the (x, y) part of the (x, y, w, h) colliders
updateColliders :: Position -> Collider -> Collider
updateColliders pos (Collider _ _ w h) = Collider (x pos) (y pos) w h

-- | Collision/death system
collision :: Bird -> Collider -> Every Collider -> Time -> Scene ()
collision bird (Collider x1 y1 w1 h1) (Every colliders) t = do
  let overlaps = map (not . \(Collider x2 y2 w2 h2)
        -> x1 + w1 <= x2 ||
           x2 + w2 <= x1 ||
           y1 + h1 <= y2 ||
           y2 + h2 <= y1) colliders
  when (or overlaps) $ do
    remove bird -- remove the Bird component from the bird after death
    system (\(scroll :: Scroll) -> remove scroll) -- stop anything from scrolling
    set $ Sound "death.ogg"

-- | Animates Sprite based on 'Animation' component
animate :: Sprite -> Animation -> TimeElapsed -> Sprite
animate (Sprite spr) (Animation speed frames) (TimeElapsed elapsed) =
  let currentFrame = floor (elapsed / speed) `mod` length frames
  in Sprite (frames !! currentFrame)

startGame :: Tap -> Sprite -> Mouse -> Keyboard -> Scene ()
startGame _ (Sprite spr) mouse keyboard =
  when (isFlapping keyboard mouse) $ case () of
    -- | Tapping to start the game adds the
    -- 'Bird' and 'Velocity' components to the bird
    _ | spr `elem` birdAnimation -> do
          remove Tap
          set Bird
          set (Velocity 0 0 0)
      -- Tapping to start de-spawns the "Tap" graphic
      | spr == "tap.png" -> do despawn
      | otherwise -> pure ()

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

    spawn Tap
          (Sprite "bird.png")
          (Position 20 116 1)
          (Collider 0 0 17 12)
          (Animation 0.1 ["birdFlapUp.png",
                          "bird.png",
                          "birdFlapDown.png",
                          "bird.png"])

    spawn Tap
          (Sprite "tap.png")
          (Position 40 116 1)

    gameLoop $ do
      system startGame
      system gravity
      system move
      system flap
      system flapSound
      system scroll
      system spawnPipes
      system despawnPipes
      system updateColliders
      system collision
      system animate