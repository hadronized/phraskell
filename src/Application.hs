module Application where

import FractalModel
import Viewer
import Graphics.UI.SDL

-- TODO: add fullscreen support
data App = App {
    appViewer       :: Viewer       -- application viewer
  , appIterFrame    :: FractalModel -- iteration frame
  , appScreen       :: Surface      -- screen surface
  , appFractalFrame :: Surface      -- fractal surface
  }

instance Show App where
  show = show . appViewer
