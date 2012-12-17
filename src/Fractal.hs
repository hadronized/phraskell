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

foreachPixel = map . map

toCart :: Float -> Float -> Frame -> Frame
toCart w h s = foreachPixel (\(x,y) -> (2 * x / (w-1) - 1, 1 - 2 * y / (h-1))) s

oZoom :: Float -> Frame -> Frame
oZoom z s = foreachPixel (\(x,y) -> (x/z,y/z)) s

offsets :: Float -> Float -> Frame -> Frame
offsets rx ry s = foreachPixel (\(x,y) -> (x+rx,y+ry)) s

type IterFrame = [[Int]]

-- take a fractal sequence, the x and y offsets, a zoom factor and evaluate the fractal sequence
mkIterFrame :: Equation -> Int -> Int -> Float -> Frame -> IterFrame
mkIterFrame rx ry z s e =  oZoom z $ offsets rx ry s
