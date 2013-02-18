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
