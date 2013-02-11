module GUI where

import Control.Monad.Trans
import Control.Monad.Trans.Maybe
import Graphics.UI.SDL
import Viewer

data GUI = GUI {
    guiZoomArea :: Surface
}

createGUI :: Viewer -> MaybeT IO GUI
createGUI v = do
  zoomArea <- tryCreateZoomArea v
  return $ GUI zoomArea

-- This function creates a Surface according to the mouse position.
tryCreateZoomArea :: Viewer -> MaybeT IO Surface
tryCreateZoomArea v = do
  let rw = floor $ viewerWidth v / zf
      rh = floor $ viewerHeight v / zf
      zf = viewerZoomf v
  zoomArea <- MaybeT $ tryCreateRGBSurface [HWSurface] rw rh 32 0 0 0 0
  lift $ setAlpha zoomArea [SrcAlpha] 127 -- TODO: Bool, what for?
  pixel <- lift $ mapRGB (surfaceGetPixelFormat zoomArea) 60 60 60
  lift $ fillRect zoomArea Nothing pixel
  return zoomArea

-- This function updates the zoom area according to the mouse position
-- and the zoom factor.
updateGUIZoomArea :: GUI -> Viewer -> IO GUI
updateGUIZoomArea gui v = do
  maybeZoomArea <- runMaybeT $ tryCreateZoomArea v
  case maybeZoomArea of
    Nothing       -> return gui
    Just zoomArea -> do
      freeSurface $ guiZoomArea gui
      return gui { guiZoomArea = zoomArea }
