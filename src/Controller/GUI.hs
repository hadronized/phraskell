module Controller.GUI where

import Control.Monad.Trans
import Control.Monad.Trans.Maybe
import View.GUI
import Graphics.UI.SDL

tryCreateZoomWindow :: Int -> Int -> Double -> MaybeT IO Surface
tryCreateZoomWindow w h zf = do
  zoom <- MaybeT $ tryCreateRGBSurface [HWSurface] w h 32 0 0 0 0
  lift $ setAlpha zoom [SrcAlpha] 127 -- TODO: Bool, what for?
  pixel <- lift $ mapRGB (surfaceGetPixelFormat zoom) 60 60 60
  lift $ fillRect zoom Nothing pixel
  return zoom

toggle :: Bool -> Bool
toggle = not
