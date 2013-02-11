module UI.Impl where

import Application
import Data.Complex
import FractalModel
import Graphics.UI.SDL as SDL
import Render
import Viewer

-- Take the position of the mouse, the zoom factor, the application, and regenerate
-- the fractal frame (also update the app’s viewer).
onIterFrameUpdate :: Double -> Double -> App -> IO App
onIterFrameUpdate x y app = do
  let cviewer     = appViewer app
      cz          = viewerZoom cviewer
      zf          = viewerZoomf cviewer
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
