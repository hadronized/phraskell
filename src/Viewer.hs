module Viewer where

import Default
import Fractal (FractalProgression, mandelbrot)

data Viewer = Viewer {
    viewerWidth        :: Double             -- current width value
  , viewerHeight       :: Double             -- current height value
  , viewerZoom         :: Double             -- current zoom value
  , viewerZoomf        :: Double             -- current zoof factor value
  , viewerX            :: Double             -- current x displacement value
  , viewerY            :: Double             -- current y displacement value
  , viewerMaxIter      :: Integer            -- current max iteration value
  , viewerColorSeed    :: Int                -- current color seed value
  , viewerProgression  :: FractalProgression -- current fractal equation
}

instance Show Viewer where
  show viewer = "["
              ++ "w:" ++ show (viewerWidth viewer) ++ " "
              ++ "h:" ++ show (viewerHeight viewer) ++ " "
              ++ "x:" ++ show (viewerX viewer) ++ " "
              ++ "y:" ++ show (viewerY viewer) ++ " "
              ++ "z:" ++ show (viewerZoom viewer) ++ " "
              ++ "i:" ++ show (viewerMaxIter viewer) ++ " "
              ++ "]"

instance Default Viewer where
  def = Viewer 800 600 0.8 0.5 (-0.5) 0 500 0 mandelbrot
