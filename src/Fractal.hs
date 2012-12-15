module Fractal where

import Data.Complex

type FComplex = Complex Float

-- mandelbrot sequence
mandelbrot :: FComplex -> FComplex -> FComplex
mandelbrot x1 c = x1 ^ 2 + c

-- for x and y, evaluate the fractal equation
evalFrac :: (FComplex -> FComplex -> FComplex) -> FComplex -> FComplex-> FComplex
evalFrac e xy f = evalFrac (e) (e f xy) xy

-- Iteration frame
iterFrame :: Int -> Int -> [[(Int,Int)]]
iterFrame w h = map (\x -> [ (x,y) | y <- [0..h-1] ]) [0..w-1]
