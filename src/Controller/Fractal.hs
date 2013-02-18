module Controller.Fractal where

import Model.Fractal
import Model.Progression
import View.FractalView

data FractalController = FractalController {
    progression :: FractalProgression
  , model       :: FractalModel
  , view        :: FractalView
}

runFractalCtrl :: FractalController -> IO ()
runFractalCtrl frac = do
  expose (model frac) (view frac)
