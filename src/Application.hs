module Application where

import FractalModel
import Viewer
import Graphics.UI.SDL
import GUI

-- TODO: add fullscreen support
data App = App {
    appViewer       :: Viewer       -- application viewer
  , appIterFrame    :: FractalModel -- iteration frame
  , appScreen       :: Surface      -- screen surface
  , appFractalFrame :: Surface      -- fractal surface
  , appVisibleGUI   :: Bool         -- is GUI visiable?
  , appGUI          :: GUI          -- GUI data
  }

instance Show App where
  show = show . appViewer

renderGUI :: App -> Int -> Int -> IO ()
renderGUI app x y = do
  let v  = appViewer app
      rw = floor $ viewerWidth v / zf
      rh = floor $ viewerHeight v / zf
      rx = x - rw `div` 2
      ry = y - rh `div` 2
      zf = viewerZoomf v
      g  = appGUI app
  blitSurface (guiZoomArea g) Nothing (appScreen app) (Just $ Rect rx ry rw rh)
  return ()
