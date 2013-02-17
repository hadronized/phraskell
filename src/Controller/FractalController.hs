module Controller.FractalController where

data FractalController = FractalController {
    progression :: FractalProgression
  , model       :: FractalModel
  , view        :: FractalView
}

runFractalCtrl :: FractalController -> IO ()
runFractalCtrl frac = do
  runFractalView (view frac) (model frac)
