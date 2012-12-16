module Fractal where

import Data.Complex

type FComplex = Complex Float

-- mandelbrot sequence
mandelbrot :: FComplex -> FComplex -> FComplex
mandelbrot x1 c = x1 ^ 2 + c

-- for x and y, evaluate the fractal equation
evalFrac :: (FComplex -> FComplex -> FComplex) -> FComplex -> FComplex-> FComplex
evalFrac e xy f = evalFrac (e) (e f xy) xy

type Pixel = (Float,Float)
type Screen = [[Pixel]]

screen :: Float -> Float -> Screen
screen w h = map (\x -> [ (x,y) | y <- [0..h-1] ]) [0..w-1]

foreachPixel :: Screen -> (Pixel -> Pixel) -> Screen
foreachPixel s f = map ( map (\(x,y) -> f (x,y)) ) s

toCart :: Float -> Float -> Screen -> Screen
toCart w h s = foreachPixel s (\(x,y) -> (2 * x / (w-1) - 1, 1 - 2 * y / (h-1)))
