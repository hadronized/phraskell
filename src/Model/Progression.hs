module Model.Progression
  (
    module Data.Complex
  , FComplex
  , FractalProgression(..)
  , mandelbrot
  ) where

import Data.Complex

type FComplex = Complex Double

data FractalProgression = FractalProgression {
    progression :: FComplex -> FComplex -> FComplex
  , start       :: FComplex
}

-- TODO: make FractalProgression an instance of Read

-- mandelbrot progression
mandelbrot :: FractalProgression
mandelbrot = FractalProgression (\z c -> z^2 + c) (fromIntegral 0)
