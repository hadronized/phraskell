module Equations where

import Fractal (Equation)

-- mandelbrot sequence
mandelbrotEquation :: Equation
mandelbrotEquation x1 c = x1 ^ 2 + c
