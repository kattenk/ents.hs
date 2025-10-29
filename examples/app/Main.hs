module Main (main) where
import Ents
import Linear
import qualified Data.Set as Set
import System.Random
import Control.Monad (replicateM_)

newtype Speed = Speed Float
data Spin = Spin
data Orbit = Orbit Float Float Float

-- | Changes color over time
rainbow :: Color -> Time -> Speed -> Color
rainbow (Color r g b a) timeStep (Speed speed)
  | r == 255 && b == 0 && g < 255 =
      Color r (clamp (g + changeAmount)) b a
  | g == 255 && b == 0 && r > 0 =
      Color (clamp (r - changeAmount)) g b a
  | g == 255 && r == 0 && b < 255 =
      Color r g (clamp (b + changeAmount)) a
  | b == 255 && r == 0 && g > 0 =
      Color r (clamp (g - changeAmount)) b a
  | b == 255 && g == 0 && r < 255 =
      Color (clamp (r + changeAmount)) g b a
  | r == 255 && g == 0 && b > 0 =
      Color r g (clamp (b - changeAmount)) a
  | otherwise = Color 0 0 255 a
  where
    changeAmount = (speed * 30) * timeStep
    clamp x = max 0 (min 255 x)

data Fall = Fall

fall :: Position -> Time -> Fall -> Position
fall (Position x y z) ts f =
  Position x (y + (0.3 * ts)) 0

spin :: Rotation -> Time -> Spin -> Speed -> Rotation
spin (Rotation yaw pitch roll) t _ (Speed speed) =
  Rotation (yaw + (speed * t)) (pitch + (speed * t)) (roll + (speed * t))

moveSpeed :: Float
moveSpeed = 5

movement :: Camera3D -> Keyboard -> Position -> Rotation -> Time -> Position
movement _ keys pos rot timeStep =
  pos + ((sum (map dirVec (Set.toList (downKeys keys))))
          * (Position timeStep timeStep timeStep)
          * (Position moveSpeed moveSpeed moveSpeed))
  where dirVec KeyW = toPosition (forward rot)
        dirVec KeyA = toPosition ((right rot) * (-1))
        dirVec KeyS = toPosition ((forward rot) * (-1))
        dirVec KeyD = toPosition (right rot)
        dirVec _ = toPosition 0
        toPosition (V3 x y z) = Position x y z

lookSens :: Float
lookSens = 30

look :: Camera3D -> Mouse -> Rotation -> Time -> Rotation
look _ mouse rot timeStep =
  Rotation newYaw newPitch 0 where
    moveX = (fst (mouseMovement mouse))
    moveY = (snd (mouseMovement mouse))
    newYaw = (yaw rot) - (moveX * lookSens) * timeStep
    newPitch = (max (-89) (min 89 ((pitch rot) - (moveY * lookSens) * timeStep)))

orbit :: Orbit -> Speed -> Position -> TimeElapsed -> Position
orbit (Orbit distance yaw pitch) (Speed speed) pos (TimeElapsed time) = toPosition newPos where
      toPosition (V3 x y z) = Position x y z
      newPos = ((V3 distance distance distance)
                * (forward (Rotation ((yaw * speed) * time) ((pitch * speed) * time) 0)))

random0Or255 :: IO Int
random0Or255 = do
    -- Get a random integer that is either 0 or 1
    r <- randomRIO (0, 1) :: IO Int

    -- Map the result
    return $ if r == 0
        then 0
        else 255

spawnRandomCube :: Scene ()
spawnRandomCube = do
  r <- liftIO random0Or255
  g <- liftIO random0Or255
  b <- liftIO random0Or255

  distance <- liftIO $ randomRIO (15, 30) :: Scene Float
  yaw <- liftIO $ randomRIO (1, -1) :: Scene Float
  pitch <- liftIO $ randomRIO (1, -1) :: Scene Float
  speed <- liftIO $ randomRIO (20, 200) :: Scene Float

  spawn (Cube)
        (Color (fromIntegral r) (fromIntegral g) (fromIntegral b) 255)
        (Orbit distance yaw pitch)
        (Position 0 0 0)
        (Speed speed)

printRot :: Camera3D -> Rotation -> Scene ()
printRot _ rot = liftIO $ print rot

main :: IO ()
main = runGame $ do
	resource $ Window {
		title = "A title here",
		res = (256, 256),
		windowSize = RelativeSize 25,
		targetFps = 300,
		captureCursor = True,
		exit = False
	}

	spawn (Camera3D 100)
				(Position 0 0 0)
				(Rotation (10) (10) 0)

	replicateM_ 200 spawnRandomCube

	gameLoop $ do
		system rainbow
		system printRot
		system fall
		system spin
		system movement
		-- system look
		system orbit