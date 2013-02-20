module Controller.Fractal where

import Model.Fractal
import Model.Progression
import View.Fractal

updateModel :: FractalProgression -> FractalModel -> Double -> Double -> Double -> Double -> Double -> Integer -> IO FractalModel
updateModel p m w h x y z mi = do
  case m of
    IterFrame _ -> return $ mkIterFrame p w h x y z mi
