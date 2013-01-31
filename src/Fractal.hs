module Fractal where

import Data.Complex

type FComplex = Complex Float
type FractalProgression = (FComplex -> FComplex -> FComplex)

-- mandelbrot progression
mandelbrot :: FractalProgression
mandelbrot z c = z^2 + c
