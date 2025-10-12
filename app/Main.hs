module Main (main) where
import LambdaGame

speed :: Float
speed = 100

-- | Changes color over time
rainbow :: Color -> Time -> Color
rainbow (Color r g b) timeStep
  | r == 255 && b == 0 && g < 255 =
      Color r (clamp (g + changeAmount)) b
  | g == 255 && b == 0 && r > 0 =
      Color (clamp (r - changeAmount)) g b
  | g == 255 && r == 0 && b < 255 =
      Color r g (clamp (b + changeAmount))
  | b == 255 && r == 0 && g > 0 =
      Color r (clamp (g - changeAmount)) b
  | b == 255 && g == 0 && r < 255 =
      Color (clamp (r + changeAmount)) g b
  | r == 255 && g == 0 && b > 0 =
      Color r g (clamp (b - changeAmount))
  | otherwise = Color 0 0 255
  where
    changeAmount = speed * timeStep
    clamp x = max 0 (min 255 x)

-- fall :: Position -> Time -> Position
-- fall (Pos x y z) ts =
--   Pos x (y + (speed * ts)) 0

main :: IO ()
main = do
  runGame $ do
    spawn (Pos 10 10 0)
          (Color 0 0 0)
          (Text "Hello LambdaGame")
  
    spawn (Sprite "backdrop.png")
          (Pos 0 0 0)
    
    spawn (Sprite "bird.png")
          (Pos 20 100 0)
    
    gameLoop $ do
      system rainbow