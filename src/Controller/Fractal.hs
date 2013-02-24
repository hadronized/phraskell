module Controller.Fractal 
  (
    module Controller.Fractal.Hard
  , ModelProcessor(..)
  , computeModel
  ) where

import Controller.Fractal.Hard
import Model.Fractal
import Model.Progression
import View.Fractal

-- ModelProcessor, used to determine how to run the model computation.
data ModelProcessor
  = SoftModelProcessor
  | HardModelProcessor ShaderProgram

computeModel :: ModelProcessor -> FractalModel -> FractalProgression -> Double -> Double -> Double -> Double -> Double -> Integer -> IO FractalModel
computeModel SoftModelProcessor m p w h x y z mi = case m of
  IterFrame _ -> return $ mkIterFrame p w h x y z mi
