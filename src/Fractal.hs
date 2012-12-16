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

foreachPixel = map . map

-- convert a standard-coordinates screen into a cartesian-coordinates (origin-centered) one
toCart :: Float -> Float -> Screen -> Screen
toCart w h s = foreachPixel (\(x,y) -> (2 * x / (w-1) - 1, 1 - 2 * y / (h-1))) s

-- fractal zoom, take a zoom factor (2.0 means to zoom in with a coef of 2), and zoom around the origin
oZoom :: Float -> Screen -> Screen
oZoom z s = foreachPixel (\(x,y) -> (x/z, y/z)) s

-- fractal translation
translate :: Float -> Float -> Screen -> Screen
translate rx ry s = foreachPixel (\(x,y) -> (x+rx,y+ry)) s
