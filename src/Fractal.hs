module Fractal (
    Julia(Julia),
    height,
    width,
    zoom,
    cX,
    cY,
    depth,
    fractalLoop,
    genFractal
) where

import Data.Bits
import Codec.Picture

data Julia = 
     Julia { height :: Int 
           , width :: Int
           , zoom :: Double
           , cX :: Double
           , cY :: Double 
           , depth :: Int
           } 
           deriving (Eq,Show,Ord)

fractalLoop :: Double -> Double -> Double -> Double -> Int -> Int
fractalLoop zx zy cX cY d
  | zx**2 + zy**2 >= 4 || d <= 1 = d
  | otherwise = fractalLoop tmp czy cX cY (d - 1)
    where tmp = zx**2 - zy**2 + cX
          czy = 2*zx*zy + cY


genFractal :: Julia -> Int -> Int -> PixelRGB8
genFractal fract x y = PixelRGB8 r g b
  where 
    h  = fromIntegral $ height fract
    w  = fromIntegral $ width fract
    z  = zoom fract 
    cx = cX fract 
    cy = cY fract
    dx = fromIntegral x
    dy = fromIntegral y
    zx = 1.5*(dx-w/2)/(0.5*z*w)
    zy = (dy-h/2)/(0.5*z*h)

    i = fractalLoop zx zy cx cy $ depth fract

    r = fromIntegral $ (i * 20)
    g = fromIntegral $ (i)
    b = fromIntegral $ (i * 8)