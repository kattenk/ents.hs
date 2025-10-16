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

-- MUST REMEMBER TO FIX RESOLUTION ON 1080p
-- also remember to fix crash with no entities

-- Game parameters
scrollSpeed = 55
pipeGap = 50
pipeRate = 1.2
flapForce = -210
gravityForce = 820

-- | Timer for spawning pipes
newtype PipeTimer = PipeTimer Float

-- Marker components
data Bird = Bird       -- ^ For the bird (when playing)
data Gravity = Gravity -- ^ Affected by gravity
data Ground = Ground   -- ^ For the ground
data Tap = Tap         -- ^ Relating to the "Tap" graphic at the start of the game
newtype Pipe = Pipe Bool -- ^ For pipes, the Bool is whether this pipe has been passed
data Score = Score Int Int -- ^ For keeping score

data Collider = Collider Float Float Float Float -- x y w h
data Animation = Animation Float    -- ^ Anim speed
                           [String] -- ^ Frames

birdAnimation = ["birdFlapUp.png",
                 "bird.png",
                 "birdFlapDown.png",
                 "bird.png"]

testAnim = Animation [Frame (Sprite "1.png"),
                      Frame (Sprite "1.png")] (loop 0.1)

isFlapping :: Keyboard -> Mouse -> Bool
isFlapping kb mouse =
  wasPressed kb Space || leftMousePressed mouse

-- | Applies gravity
gravity :: Gravity -> Velocity -> Time -> Velocity
gravity _ vel t = vel + Velocity 0 (gravityForce * t) 0

-- | Applies velocity
move :: Velocity -> Position -> Time -> Position
move (Velocity x y z) pos t =
  pos + Position (x * t) (y * t) (z * t)

-- | Resets the grounds X coord when it goes past -24
scrollGround :: Ground -> Position -> Position
scrollGround _ pos =
  if x pos < -24 then
    Position 0 (y pos) 1
  else pos

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

    spawn (Pipe False)
          (Sprite "pipeBottom.png")
          (Position 144 (gapCenterY + (pipeGap / 2)) 0.1)
          (Collider 0 0 26 200)
          (Velocity (-scrollSpeed) 0 0)

    spawn (Pipe False)
          (Sprite "pipeTop.png")
          (Position 144 ((-pipeSprHeight) + (gapCenterY - (pipeGap / 2))) 0.5)
          (Collider 0 0 26 200)
          (Velocity (-scrollSpeed) 0 0.1)

    resource (PipeTimer pipeRate)

-- | For despawning any entities that go offscreen to the left
despawnPipes :: Position -> Scene ()
despawnPipes pos = when (x pos < -26) despawn

-- | Updates the (x, y) part of the (x, y, w, h) colliders
updateColliders :: Position -> Collider -> Collider
updateColliders pos (Collider _ _ w h) = Collider (x pos) (y pos) w h

shrinkBy = 9

-- | Collision/death system
collision :: Bird -> Collider -> Every Collider -> Time -> Scene ()
collision bird (Collider x1 y1 w1 h1) (Every colliders) t = do
  let overlaps = map (not . \(Collider x2 y2 w2 h2)
        -> (x1) + (w1 - shrinkBy) <= x2 ||
           x2 + w2 <= (x1 + shrinkBy) ||
           y1 + h1 <= y2 ||
           y2 + h2 <= y1) colliders
  when (or overlaps) $ do
    remove bird -- remove the Bird component from the bird after death
    system (\(vel :: Velocity) -> Velocity 0 (y vel) 0) -- stop anything from scrolling
    -- 82 y 15 x
    spawn (Sprite "gameover.png")
          (Position 15 72 1)
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
          set Gravity
          set (Velocity 0 0 0)
      -- Tapping to start de-spawns the "Tap" graphic
      | spr == "tap.png" -> despawn
      | otherwise -> return ()

-- | Add score when a pipe that hasn't already been passed
-- goes past a certain point
score :: Pipe -> Position -> Scene (Pipe, Score)
score (Pipe False) pos =
  if x pos < (-6) then do
    set (Sound "score.ogg")
    return (Pipe True, Score 5 0)
  else do
    return (Pipe False, Score 5 0)
score p _ = return (p, Score 5 0)

birdDive :: Bird -> Velocity -> Angle
birdDive _ (Velocity 0 vy _)
  = Angle (max (-30) (min 90 (vy * 0.3)))

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

    spawn Ground
          (Sprite "ground.png")
          (Position 0 200 1)
          (Collider 0 0 168 56)
          (Velocity (-scrollSpeed) 0 0)

    spawn Tap
          (Sprite "bird.png")
          (Position 20 116 0.5)
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
      system scrollGround
      system flap
      system flapSound
      system birdDive
      system spawnPipes
      system despawnPipes
      system updateColliders
      system collision
      system animate
      system score