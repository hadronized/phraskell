module Fractal where

import Data.Complex

type FComplex = Complex Float
type Equation = (FComplex -> FComplex -> FComplex)

-- mandelbrot sequence
mandelbrot :: FComplex -> FComplex -> FComplex
mandelbrot x1 c = x1 ^ 2 + c

-- for x and y, evaluate the fractal equation
evalFrac :: Equation -> FComplex -> FComplex -> Integer -> Integer
evalFrac e xy f m = go xy f 0
    where go xy f i
            | i >= m = m
            | realPart (abs xy) > 2.0 = i
            | otherwise = go (e f xy) xy (i+1)

type Pixel = (Float,Float)
type Frame = [[Pixel]]

screen :: Float -> Float -> Frame
screen w h = map (\x -> [ (x,y) | y <- [0..h-1] ]) [0..w-1]

toCart :: Float -> Float -> Pixel -> Pixel
toCart w h (x,y) = (2 * x / (w-1) - 1, 1 - 2 * y / (h-1))

oZoom :: Float -> Pixel -> Pixel
oZoom z (x,y) = (x/z,y/z)

offsets :: Float -> Float -> Pixel -> Pixel
offsets rx ry (x,y) = (x+rx,y+ry)

type IterFrame = [[Integer]]

-- make the iterations frame
-- take w, h, rx, ry, z and the fractal equation
mkIterFrame :: Float -> Float -> Float -> Float -> Float -> Equation -> IterFrame
mkIterFrame w h rx ry z e =
    let frame = (map . map $ (offsets rx ry) . (oZoom z) . (toCart w h)) $ screen w h
        eval  = (map . map $ (\(x,y) -> (evalFrac e (x :+ y) (fromInteger 0) 100)))
    in eval frame
