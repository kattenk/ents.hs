module Main (main) where
import LambdaGame

data Bird = Bird

gravity :: Velocity -> Time -> Velocity
gravity vel t = vel + Velocity 0 (gravityForce * t) 0
  where gravityForce = 650

move :: Velocity -> Position -> Time -> Position
move (Velocity x y z) pos t =
  pos + Position (x * t) (y * t) (z * t)

flap :: Bird -> Velocity -> Keyboard -> Velocity
flap _ vel keyboard =
  if wasPressed keyboard Space then
    Velocity 0 (-200) 0
  else
    vel

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

    spawn (Sprite "backdrop.png")
          (Position 0 0 0)
    
    spawn (Sprite "ground.png")
          (Position 0 200 1)

    spawn Bird
          (Sprite "bird.png")
          (Position 20 116 1)
          -- (Velocity 0 0 0)
    
    spawn (Sprite "pipeBottom.png")
          (Position 70 116 0.5)

    gameLoop $ do
      system gravity
      system move
      system flap