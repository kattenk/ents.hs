![Ents Logo](assets/entslogo7x.png)
#
An experimental game engine written in Haskell using Raylib and the ECS design pattern with dependency injection (DI)
```haskell
heal :: Health -> Time -> Health
jump :: Keyboard -> Player -> Velocity -> Velocity
death :: Health -> Scene ()
birdDive :: Bird -> Velocity -> Angle
collision :: Collider -> Every Collider -> Scene ()
drawCubes :: Cube -> Maybe Position -> Maybe Color -> Scene ()
healEnemies :: Not Player -> Health -> Time -> Health
score :: Player -> Position -> Maybe (Score, Sound)
```
Note those systems are mere idealized suggestions to show the different kinds of things you can do, often you will be using the Scene monad, and you will almost always need Time (the delta time) for useful gameplay code

## Features
- Doesn't require component registries or Template Haskell,
  or even an instance of a type class, any data type can be used as a component or resource.
- Has simple yet rich and extensible query system (`Not`, `Every`, `Maybe`)
- Systems are just functions
- Built with Raylib (h-raylib bindings)
- Has been used to make a 2D game that [works on the Web](https://flappy-hs.netlify.app) via Wasm (apparently this is the first Haskell game made with h-raylib to ever properly run on the web) see web export

## Tutorial
The simplest application looks something like this:
```haskell
module Main (main) where
import Ents

main :: IO ()
main = runGame $ do
  gameLoop $ return ()
```
this opens an empty window, to customize the window, you can add a `Window` resource by putting the following after `runGame` but before `gameLoop`:
```haskell
resource $ Window {
  title = "A title here",
  res = (256, 256),
  windowSize = RelativeSize 25,
  targetFps = 300,
  captureCursor = False,
  exit = False
}
```
<img src="assets/tutorial/emptyWindow.png" width="300em">

to add visible things we need to spawn entities, we can spawn a cube by putting this right after the Window resource:
```haskell
spawn Cube (Position 0 0 2)
```
<img src="assets/tutorial/cube.png" width="300em">

Entities are collections of components, `Cube` and `Position` are both built-in components, we can add a second entity with `Text` and `Color`
```haskell
spawn (Text "Hello World" 20 AlignLeft)
      (Position 10 10 0)
      (Color 100 100 255 255)
```
<img src="assets/tutorial/cubePlusText.png" width="300em">

to make anything happen you use Systems, here's a system that changes `Color` over time:
```haskell
rainbow :: Color -> Time -> Color
rainbow (Color r g b a) timeStep
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
    changeAmount = (20 * 30) * timeStep
    clamp x = max 0 (min 255 x)
```
And here's another system that changes `Rotation` and uses a custom component, `Spin`, to customize how fast it goes:
```haskell
newtype Spin = Spin { speed :: Float }

spin :: Rotation -> Time -> Spin -> Rotation
spin (Rotation yaw pitch roll) t (Spin speed) =
  Rotation (yaw + (speed * t))
		   (pitch + (speed * t))
		   (roll + (speed * t))
```
After adding `Rotation` and `Color` and `Spin` to the cube, we can hook these systems up to the game loop with:
```haskell
gameLoop $ do
  system spin
  system rainbow
```

<img src="assets/tutorial/raincube.gif" width="300em">

We can spawn a `Camera3D` to control the location of the 3D camera
```haskell
spawn (Camera3D 100) -- 100 is FOV
      (Position 0 0 0)
      (Rotation 5 0 0)
```
here are some rudimentary systems for moving the camera around using the mouse and keyboard:
```haskell
-- Add these imports, need containers and linear packages
import Linear
import qualified Data.Set as Set

moveSpeed, lookSens :: Float
moveSpeed = 5
lookSens = 30

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

look :: Camera3D -> Mouse -> Rotation -> Time -> Rotation
look _ mouse rot timeStep =
  Rotation newYaw newPitch 0 where
    moveX = (fst (mouseMovement mouse))
    moveY = (snd (mouseMovement mouse))
    newYaw = (yaw rot) - (moveX * lookSens) * timeStep
    newPitch = (max (-89) (min 89 ((pitch rot) - (moveY * lookSens) * timeStep)))
```

For fun I wrote some code that spawns cubes and makes them fly around:
```haskell
replicateM_ 200 spawnRandomCube
```

https://github.com/user-attachments/assets/38d901bb-d850-45a4-b4f2-5956803bbd02
