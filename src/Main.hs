module Main where

import Codec.Picture
import Text.Printf

import Fractal

--genImage :: Julia -> Image PixelRGB8
--genImage f = generateImage (genFractal f) w h
--  where
--    h = height f
--    w = width f

black :: Image PixelRGB8
black = generateImage (\x y -> PixelRGB8 0 0 0) 640 480

red :: Image PixelRGB8
red = generateImage (\x y -> PixelRGB8 255 0 0) 640 480

white :: Image PixelRGB8
white = generateImage (\x y -> PixelRGB8 255 255 255) 640 480

quarterNotes = map (\x -> round (x * ((3600.0/60.0)))) [0..20000]
sixteenthNotes = map (\x -> round (x * ((3600.0/60.0)/4))) [0..20000]

getFrame :: Int -> Image PixelRGB8
getFrame frame
  | frame `elem` quarterNotes = red
-- | frame `elem` sixteenthNotes = white
  | otherwise = black

genFrames :: Int -> Int -> IO ()
genFrames frame frameCount
  | frame == frameCount = return ()
  | otherwise = do
      writePng (printf "out/frame%03d.png" frame) $ getFrame frame
      putStrLn $ printf "Rendering frame %d/%d" frame frameCount
      genFrames (frame + 1) frame

main :: IO ()
main = genFrames 0 (60*30)
  where fractal = Julia 360 480 1 (-1) 0.124123 100
