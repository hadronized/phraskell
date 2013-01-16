module Fractal where

import Data.Complex

type FComplex = Complex Float
type Equation = (FComplex -> FComplex -> FComplex)

-- for x and y, evaluate the fractal equation
evalFrac :: Equation -> FComplex -> FComplex -> Integer -> Integer
evalFrac e z1 xy1 m = go z1 xy1 0
  where go z@(rp :+ ip) xy i
          | i > m = -1
          | rp^2 + ip^2 > 4.0 = i
          | otherwise = go (e z xy) xy (i+1)

type Pixel = (Float,Float)
type Frame = [[Pixel]]

screen :: Float -> Float -> Frame
screen w h = map (\x -> [ (x,y) | y <- [0..h-1] ]) [0..w-1]

toCart :: Float -> Float -> Float -> Pixel -> Pixel
toCart w h r (x,y) = (2 * r*x / w - 1, 1 - 2 * y / h)

oZoom :: Float -> Pixel -> Pixel
oZoom z (x,y) = (x/z,y/z)

offsets :: Float -> Float -> Pixel -> Pixel
offsets rx ry (x,y) = (x+rx,y+ry)

type IterFrame = [[Integer]]

-- make the iterations frame
-- take w, h, rx, ry, z and the fractal equation
mkIterFrame :: Float -> Float -> Float -> Float -> Float -> Equation -> IterFrame
mkIterFrame w h rx ry z e =
  let frame = (map . map $ (offsets rx ry) . (oZoom z) . (toCart w h ratio)) $ screen w h
      eval  = (map . map $ (\(x,y) -> (evalFrac e (fromInteger 0) (x :+ y) 500)))
      ratio = w / h
  in eval frame
