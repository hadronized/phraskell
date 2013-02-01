module Viewer where

import Default
import Fractal (FractalProgression, mandelbrot)

data Viewer = Viewer {
    viewerWidth        :: Float -- current width value
  , viewerHeight       :: Float -- current height value
  , viewerZoom         :: Float -- current zoom value
  , viewerX            :: Float -- current x displacement value
  , viewerY            :: Float -- current y displacement value
  , viewerColorSeed    :: Int   -- current color seed value
  , viewerProgression  :: FractalProgression -- current fractal equation
}

instance Show Viewer where
  show viewer = "["
              ++ show (viewerWidth viewer) ++ ","
              ++ show (viewerHeight viewer) ++ ","
              ++ show (viewerX viewer) ++ ","
              ++ show (viewerY viewer) ++ ","
              ++ show (viewerZoom viewer) ++ "]"

instance Default Viewer where
  def = Viewer 800 600 1 0 0 0 mandelbrot
