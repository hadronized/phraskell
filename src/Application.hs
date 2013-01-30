module Application where

import Viewer

-- TODO: add fullscreen support
data App = App {
    appViewer       :: Viewer    -- application viewer
  , appIterFrame    :: IterFrame -- iteration frame
  , appScreen       :: Surface   -- screen surface
  , appFractalFrame :: Surface   -- fractal surface
  }

instance Show App where
  show = show . appViewer
