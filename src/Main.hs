module Main where

import Codec.Picture
import Text.Printf
import System.Random
import System.Process

import Fractal

quarterNotes = map (\x -> round (x * ((3600.0/130.0)))) [0..20000]
sixteenthNotes = map (\x -> round (x * ((3600.0/130.0)/16.0))) [0..20000]

genFrame :: Julia -> Int -> Image PixelRGB8
genFrame f frame = generateImage (genFractal f) w h
  where
    h = height f
    w = width f

genFrames :: Int -> Int -> Int -> Double -> IO ()
genFrames frame frameCount depth zoom
  | frame == frameCount = return ()
  | otherwise = do
      --randA <- randomRIO (-1, 1):: IO Double
      --randB <- randomRIO (-1, 1):: IO Double
      let f = Julia 360 480 newZoom (cos (-nFrame * 0.01 + 0.5)) (sin (nFrame * 0.01 - 0.5)) depth
      writePng (printf "out/frame%04d.png" frame) $ genFrame f frame
      putStrLn $ printf "Rendering frame %d/%d" (frame + 1) frameCount
      genFrames (frame + 1) frameCount newDepth newZoom
  where
    nFrame = fromIntegral frame
    newDepth = if frame `elem` quarterNotes then depth + 1 else depth
    newZoom = if frame `elem` quarterNotes then (zoom + 0.03) else (zoom + 0.001)
    
main :: IO ()
main = do
  createProcess (shell "rm out/*")
  genFrames 0 500 5 0.3
  putStrLn "done"
  
