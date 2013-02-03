module UI.Impl where

import Application
import Data.Complex
import FractalModel
import Graphics.UI.SDL as SDL
import Render
import Viewer

-- Take the position of the mouse, the zoom factor, the application, and regenerate
-- the fractal frame (also update the app’s viewer).
onIterFrameUpdate :: Double -> Double -> Double -> App -> IO App
onIterFrameUpdate x y zf app = do
  let cviewer     = appViewer app
      cz          = viewerZoom cviewer
      (rx :+ ry)  = toCart (viewerWidth cviewer) (viewerHeight cviewer) (x :+ y)
      (nx,ny)     = (viewerX cviewer + rx/cz,viewerY cviewer + ry/cz)
      newViewer   = cviewer { viewerX = nx, viewerY = ny, viewerZoom = cz*zf }
  onFractalFrameUpdate $ app { appViewer = newViewer }
 
onFractalFrameUpdate :: App -> IO App
onFractalFrameUpdate app = do
  let viewer = appViewer app
      iterf  = mkIterFrame viewer
  putStr $ "updating fractal " ++ show app ++ "... "
  pixelizeSurface iterf (appFractalFrame app)
  putStrLn "done!"
  return app { appViewer = viewer, appIterFrame = iterf }
 
onMouseMotion :: Int -> Int -> Double -> App -> IO App
onMouseMotion mx my zf app = do
  -- first, create the Rect that defines the cursor position according to the mouse position
  let viewer = appViewer app
      rw = floor $ viewerWidth viewer / zf
      rh = floor $ viewerHeight viewer / zf
      rx = mx - rw `div` 2
      ry = my - rh `div` 2
  maybeZoomSurf <- tryCreateRGBSurface [HWSurface] rw rh 32 0 0 0 0
  case maybeZoomSurf of
    Nothing -> return app
    Just zoomSurf -> do
      setAlpha zoomSurf [SrcAlpha] 127 -- TODO: Bool, what for?
      pixel <- mapRGB (surfaceGetPixelFormat zoomSurf) 60 60 60
      fillRect zoomSurf Nothing pixel
      blitSurface (appFractalFrame app) Nothing (appScreen app) Nothing
      blitSurface zoomSurf Nothing (appScreen app) (Just $ Rect rx ry rw rh)
      freeSurface $ zoomSurf
      return app
