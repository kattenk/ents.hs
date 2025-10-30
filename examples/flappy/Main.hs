{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE DoAndIfThenElse #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE FlexibleContexts #-}

module Main (main) where
import Ents
import Ents.Extra.Animation
import Control.Monad (when)
import System.Random (randomRIO)
import Data.Proxy

-- Game parameters
scrollSpeed = 55
pipeGap = 50
pipeRate = 1.2
flapForce = -210
gravityForce = 820

-- | Timer for spawning pipes
newtype PipeTimer = PipeTimer Float
-- | Timer for resetting the world
newtype ResetTimer = ResetTimer Float

-- Marker components
data Bird = Bird       -- ^ For the bird (when playing)
data Gravity = Gravity -- ^ Affected by gravity
data Ground = Ground   -- ^ For the ground
data Tap = Tap         -- ^ Relating to the "Tap" graphic at the start of the game
newtype Pipe = Pipe { wasPassed :: Bool }
newtype OkButton = OkButton { active :: Bool }
data Score = Score Int Int
data ScoreCounter = ScoreCounter

data Collider = Collider Float Float Float Float -- x y w h

birdAnimation = ["assets/birdFlapUp.png",
                 "assets/bird.png",
                 "assets/birdFlapDown.png",
                 "assets/bird.png"]

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
  wasPressed kb KeySpace || leftMousePressed mouse

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
    Just (Sound "assets/flap.ogg")
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

    spawn (Pipe { wasPassed = False })
          (Sprite "assets/pipeBottom.png")
          (Position 144 (gapCenterY + (pipeGap / 2)) 0.1)
          (Collider 0 0 26 200)
          (Velocity (-scrollSpeed) 0 0)

    spawn (Pipe { wasPassed = False })
          (Sprite "assets/pipeTop.png")
          (Position 144 ((-pipeSprHeight) + (gapCenterY - (pipeGap / 2))) 0.5)
          (Collider 0 0 26 200)
          (Velocity (-scrollSpeed) 0 0.1)

    -- reset the pipe timer
    resource (PipeTimer pipeRate)

-- | For despawning any entities that go offscreen to the left
despawnPipes :: Position -> Scene ()
despawnPipes pos = when (x pos < -26) despawn

-- | Updates the (x, y) part of the (x, y, w, h) colliders
updateColliders :: Position -> Collider -> Collider
updateColliders pos (Collider _ _ w h) = Collider (x pos) (y pos) w h

startGame :: Tap -> Sprite -> Mouse -> Keyboard -> Scene ()
startGame _ (Sprite spr) mouse keyboard =
  when (isFlapping keyboard mouse) $ case () of
    -- | Tapping to start the game adds the
    -- 'Bird' and 'Velocity' components to the bird
    _ | spr `elem` birdAnimation -> do
          remove (Proxy :: Proxy Tap)
          set Bird
          set Gravity
          set (Velocity 0 0 0)
          spawn ScoreCounter
                (Position 72 30 1)
                (Font "assets/font.png")

      -- Tapping to start de-spawns the "Tap" graphic
      | spr == "assets/tap.png" -> despawn
      | otherwise -> return ()

-- | Collision/death system
collision :: Bird -> Collider -> Every Collider -> Time -> Score -> Scene ()
collision bird (Collider posX posY width height) (Every colliders) t
          (Score lastScore bestScore) = do
  let overlaps = map (\(Collider x y w h)
        -> (posX + (width - 3)) >= x && posX < x + w &&
           (posY + height) >= y && posY < y + h) colliders
  when (or overlaps) $ do
    set $ Sound "assets/death.ogg"
    remove (Proxy :: Proxy Bird) -- remove the Bird component from the bird after death
    system (\(vel :: Velocity) -> Velocity 0 (y vel) 0) -- stop anything from scrolling
    system (\(_ :: ScoreCounter) -> despawn :: Scene ()) -- remove the score counter

    let onScoreboard x y =
          [Frame (70, Position (15 + x) (210 + y) 2),
           Frame (80, Position (15 + x) (86 + y) 2),
           Frame (100, Position (15 + x) (110 + y) 2)]

    spawn (Sprite "assets/gameOver.png")
          (Animation (onScoreboard 0 (-30)) 1)

    spawn (Sprite "assets/scoreboard.png")
          (Animation (onScoreboard 0 0) 1)

    spawn (Text (show lastScore) 15 AlignLeft)
          (Font "assets/font.png")
          (Animation (onScoreboard 90 17) 1)

    spawn (Text (show bestScore) 15 AlignLeft)
          (Font "assets/font.png")
          (Animation (onScoreboard 90 37) 1)

    spawn OkButton { active = False }
          (Sprite "assets/ok.png")
          (Position 51 222 4)
          (Animation (onScoreboard 36 112
                        ++ [Frame (80, OkButton { active = False }),
                            Frame (90, OkButton { active = True })]) 1)

restartGame :: OkButton -> Position -> Keyboard -> Mouse -> Scene ()
restartGame (OkButton { active = True }) btnPos keyboard mouse =
  when (isFlapping keyboard mouse) $ do
    set (Animation [Frame (0, btnPos + Position 0 0 0),
                    Frame (20, btnPos + Position 0 4 0),
                    Frame (30, btnPos - Position 0 1 0),
                    Frame (50, btnPos),
                    Frame (100, btnPos)] 0.5)

    remove (Proxy :: Proxy OkButton) -- make it so they can't press again
    system (\(_ :: Rectangle) -> despawn :: Scene ())
    
    set (Sound "assets/swoosh.ogg")

    -- drop the curtain
    spawn Rectangle
          (Color 0 0 0 0)
          (Position 0 0 4)
          (Size 144 256 0)
          (Animation [Frame (0, Color 0 0 0 0),
                      Frame (50, Color 0 0 0 255),
                      Frame (100, Color 0 0 0 0)] 1)
    
    -- reset half-way through the animation
    resource (ResetTimer 0.5)

restartGame _ _ _ _ = pure ()

resetWorld :: ResetTimer -> Time -> Score -> Scene ResetTimer
resetWorld (ResetTimer timer) t (Score _ best) = case () of
  _ | timer > 0 -> return (ResetTimer (timer - t))
    | otherwise -> do
      system (\(_ :: Not Rectangle) -> despawn :: Scene ())
      resource (Score 0 best)
      setupScene
      return (ResetTimer 999999999999)

-- | Add score when a pipe that hasn't already been passed
-- goes past a certain point
score :: Pipe -> Position -> Score -> Scene (Pipe, Score)
score (Pipe { wasPassed = False}) pos (Score cur best) =
  if (x pos < 28) && (y pos < 0) then do
    set (Sound "assets/score.ogg")
    return (Pipe True, Score (cur + 1) (max (cur + 1) best))
  else do
    return (Pipe False, Score cur best)
score p _ s = return (p, s)

scoreCounter :: ScoreCounter -> Score -> Text
scoreCounter _ (Score currentScore _) =
  Text (show currentScore) 21 AlignCenter

birdDive :: Bird -> Velocity -> Angle
birdDive _ (Velocity 0 vy _)
  = Angle (max (-30) (min 90 (vy * 0.3)))

setupScene :: Scene ()
setupScene = do
  spawn (Sprite "assets/backdrop.png")
        (Position 0 0 0)

  spawn Ground
        (Sprite "assets/ground.png")
        (Position 0 200 1)
        (Collider 0 0 168 56)
        (Velocity (-scrollSpeed) 0 0)

  -- Bird
  spawn Tap
        (Sprite "assets/bird.png")
        (Position 20 116 0.5)
        (Collider 0 0 17 12)
        (Animation (map (Frame . Sprite) birdAnimation) (loop 0.4))

  spawn Tap
        (Sprite "assets/tap.png")
        (Position 40 116 1)

main :: IO ()
main = do
  runGame $ do
    resource $ Window {
      title = "Flappy",
      res = (144, 256),
      windowSize = RelativeSize 15,
      targetFps = 300,
      captureCursor = False,
      exit = False
    }

    resource (PipeTimer pipeRate)
    resource (Score 0 0)

    setupScene

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
      system scoreCounter
      system restartGame
      system resetWorld