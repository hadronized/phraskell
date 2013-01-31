module UI.Impl where

import Viewer

-- Take the position of the mouse, the zoom factor, the application, and regenerate
-- the fractal frame (also update the app’s viewer).
onIterFrameUpdate :: Float -> Float -> Float -> App -> IO App
onIterFrameUpdate :: x y zf app = do
  let cviewer   = appViewer app
      cz        = viewerZoom cviewer
      (nx,ny)   = (viewerX cviewer + x*cz,viewerY cviewer + y*cf)
      newViewer = cviewer { viewerX = nx, viewerY = ny, viewerZoom = cz*zf }
  onFractalFrameUpdate $ app { appViewer = newViewer }
            
onFractalFrameUpdate :: App -> IO App
onFractalFrameUpdate app = do
  let viewer = appViewer app
       iterf = mkIterFrame viewer
  putStr $ "updating fractal " ++ show app ++ "... "
  pixelizeSurface iterf (appScreen app)
  putStrLn "done!"
  return app { appViewer = viewer, appIterFrame = iterf }
  
  
  
{-

       onMouseMotion mx my = do
         -- first, create the Rect that defines the cursor position according to the mouse position
         let zf = appZoom app
             rw = floor $ appWidth app / zf
             rh = floor $ appHeight app / zf
             rx = mx - rw `div` 2
             ry = my - rh `div` 2
         maybeZoomSurf <- tryCreateRGBSurface [HWSurface] rw rh 32 0 0 0 0
         case maybeZoomSurf of
           Nothing -> nochange
           Just zoomSurf -> do
             setAlpha zoomSurf [SrcAlpha] 127 -- TODO: Bool, what for?
             pixel <- mapRGB (surfaceGetPixelFormat zoomSurf) 60 60 60
             fillRect zoomSurf Nothing pixel
             blitSurface zoomSurf Nothing (appScreen app) (Just $ Rect rx ry rw rh)
             freeSurface $ zoomSurf
             nochange
-}