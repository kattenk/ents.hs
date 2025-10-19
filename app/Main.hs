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
data Pipe = Pipe { wasPassed :: Bool, isTop :: Bool}
data Score = Score Int Int -- ^ For keeping score
data ScoreCounter = ScoreCounter

data Collider = Collider Float Float Float Float -- x y w h

birdAnimation = ["birdFlapUp.png",
                 "bird.png",
                 "birdFlapDown.png",
                 "bird.png"]

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

isFlapping :: Keyboard -> Mouse -> Bool
isFlapping kb mouse =
  wasPressed kb Space || leftMousePressed mouse

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

    spawn (Pipe { wasPassed = False, isTop = False})
          (Sprite "pipeBottom.png")
          (Position 144 (gapCenterY + (pipeGap / 2)) 0.1)
          (Collider 0 0 26 200)
          (Velocity (-scrollSpeed) 0 0)

    spawn (Pipe { wasPassed = False, isTop = True})
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
collision :: Bird -> Collider -> Every Collider -> Time -> Score -> Scene ()
collision bird (Collider x1 y1 w1 h1) (Every colliders) t
          (Score lastScore bestScore) = do
  let overlaps = map (not . \(Collider x2 y2 w2 h2)
        -> (x1) + (w1 - shrinkBy) <= x2 ||
           x2 + w2 <= (x1 + shrinkBy) ||
           y1 + h1 <= y2 ||
           y2 + h2 <= y1) colliders
  when (or overlaps) $ do
    set $ Sound "death.ogg"
    remove bird -- remove the Bird component from the bird after death
    system (\(vel :: Velocity) -> Velocity 0 (y vel) 0) -- stop anything from scrolling
    system (\(_ :: ScoreCounter) -> despawn :: Scene ()) -- remove the score counter

    let onScoreboard x y =
          [Frame (0, Color 255 255 255 0),
           Frame (60, Color 255 255 255 0),
           Frame (80, Color 255 255 255 255),
           Frame (70, Position (15 + x) (210 + y) 1),
           Frame (80, Position (15 + x) (86 + y) 1),
           Frame (100, Position (15 + x) (110 + y) 1)]

    spawn (Sprite "gameOver.png")
          (Animation (onScoreboard 0 (-30)) 1)

    spawn (Sprite "scoreboard.png")
          (Animation (onScoreboard 0 0) 1)

    spawn (Text (show lastScore) 15 AlignRight)
          (Font "font.png")
          (Animation (onScoreboard 102 17) 1)

    spawn (Text (show bestScore) 15 AlignRight)
          (Font "font.png")
          (Animation (onScoreboard 102 37) 1)

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
          spawn ScoreCounter
                (Position 72 30 1)
                (Font "font.png")

      -- Tapping to start de-spawns the "Tap" graphic
      | spr == "tap.png" -> despawn
      | otherwise -> return ()

-- | Add score when a pipe that hasn't already been passed
-- goes past a certain point
score :: Pipe -> Position -> Score -> Scene (Pipe, Score)
score (Pipe { wasPassed = False, isTop = True }) pos (Score cur best) =
  if x pos < 28 then do
    set (Sound "score.ogg")
    return (Pipe True True, Score (cur + 1) (max (cur + 1) best))
  else do
    return (Pipe False True, Score cur best)
score p _ s = return (p, s)

updateScoreCounter :: ScoreCounter -> Score -> Text
updateScoreCounter _ (Score currentScore _) =
  Text (show currentScore) 21 AlignCenter

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
    resource (Score 0 0)

    spawn (Sprite "backdrop.png")
          (Position 0 0 0)

    spawn Ground
          (Sprite "ground.png")
          (Position 0 200 1)
          (Collider 0 0 168 56)
          (Velocity (-scrollSpeed) 0 0)

    -- Bird
    spawn Tap
          (Sprite "bird.png")
          (Position 20 116 0.5)
          (Collider 0 0 17 12)
          (Animation (map (Frame . Sprite) birdAnimation) (-0.4))

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
      system updateScoreCounter