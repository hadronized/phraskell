module Controller.GUI where

import View.GUI

data GUIController = GUIController {
    guiCtrlVisible :: Bool
  , guiView        :: GUIView
}

tryCreateZoomWindow :: Int -> Int -> Double -> MaybeT IO Surface
tryCreateZoomWindow w h zf = do
  zoom <- MaybeT $ tryCreateRGBSurface [HWSurface] rw rh 32 0 0 0 0
  lift $ do
    setAlpha zoom [SrcAlpha] 127 -- TODO: Bool, what for?
    pixel <- mapRGB (surfaceGetPixelFormat zoom) 60 60 60
    fillRect zoom Nothing pixel
  return zoom
