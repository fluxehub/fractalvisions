module Main where

import Codec.Picture

import Fractal

genImage :: Julia -> Image PixelRGB8
genImage f = generateImage (genFractal f) w h
  where
    h = height f
    w = width f

main :: IO ()
main = writePng "test.png" $ genImage fractal 
  where fractal = Julia 360 480 1 (-1) 0.124123 100
