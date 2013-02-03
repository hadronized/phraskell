module Viewer where

import Default
import Fractal (FractalProgression, mandelbrot)

data Viewer = Viewer {
    viewerWidth        :: Double -- current width value
  , viewerHeight       :: Double -- current height value
  , viewerZoom         :: Double -- current zoom value
  , viewerX            :: Double -- current x displacement value
  , viewerY            :: Double -- current y displacement value
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
  def = Viewer 800 600 0.8 (-0.5) 0 0 mandelbrot
