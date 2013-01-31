module Equations where

import Fractal (FractalProgression)

-- mandelbrot sequence
mandelbrotEquation :: FractalProgression
mandelbrotEquation z c = z^2 + c
