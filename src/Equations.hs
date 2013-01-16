module Equations where

import Fractal (Equation)

-- mandelbrot sequence
mandelbrotEquation :: Equation
mandelbrotEquation z c = z^2 + c
