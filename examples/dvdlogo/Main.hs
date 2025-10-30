{-# LANGUAGE DoAndIfThenElse #-}
module Main (main) where
import Ents
import System.Random (randomRIO)

-- | Applies velocity
move :: Velocity -> Position -> Time -> Position
move (Velocity x y z) pos t =
  pos + Position (x * t) (y * t) (z * t)

dvdLogoSprWidth, dvdLogoSprHeight :: Float
dvdLogoSprWidth = 404
dvdLogoSprHeight = 243

bounce :: Position -> Velocity -> Color -> Scene (Velocity, Color)
bounce pos vel color = do
  let newVel = case () of
        _ | x pos + dvdLogoSprWidth > 1500 || x pos < 0 ->
              vel - Velocity (x vel * 2) 0 0
          | y pos + dvdLogoSprHeight > 1500 || y pos < 0 ->
              vel - Velocity 0 (y vel * 2) 0
          | otherwise -> vel
  if newVel /= vel then do
    r <- liftIO $ randomRIO (0, 255) :: Scene Float
    g <- liftIO $ randomRIO (0, 255) :: Scene Float
    b <- liftIO $ randomRIO (0, 255) :: Scene Float
    return (newVel, Color r g b 255)
  else
    return (vel, color)

main :: IO ()
main = runGame $ do
  resource $ Window {
    title = "DVD Logo",
    res = (1500, 1500),
    windowSize = RelativeSize 30,
    targetFps = 300,
    captureCursor = False,
    exit = False
  }

  spawn (Position 450 450 0)
        (Sprite "assets/dvdLogo.png")
        (Color 255 255 255 255)
        (Velocity 400 200 0)

  gameLoop $ do
    system move
    system bounce