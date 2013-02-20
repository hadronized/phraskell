module Controller.Fractal where

import Model.Fractal
import Model.Progression
import View.Fractal

data FractalController = FractalController {
    progression :: FractalProgression
  , zoomFactor  :: Double
  , model       :: FractalModel
  , view        :: FractalView
}

runFractalCtrl :: FractalController -> IO ()
runFractalCtrl frac = do
  expose (model frac) (view frac)

regenModel :: FractalController -> Double -> Double -> Double -> Double -> Double -> Integer -> IO FractalController
regenModel fc w h x y z mi = do
  let p = progression fc
      m = model fc
  case m in
    IterFrame _ -> return fc { model = mkIterFrame p w h x y z i }
