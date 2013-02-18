module CrossInit where

import Control.Monad.Trans.Maybe
import Controller.Bootstrap

title :: String
title = "phraskell 0.1.3"

crossInit :: Bootstrap -> MaybeT IO AppController
crossInit b = do
  lift SDL.init [InitVideo] -- first thing to do, init SDL
  let w           = bootWidth b
      h           = bootHeight b
      model       = bootModel b
      progression = mandelbrot --bootProgression b
      zf          = 0.5
  -- AppController init: acquiring a screen Surface
  screen <- tryGetScreen w h 32 title
  -- AppController init: building the FractalController
  --   FractalController init: building the view
  --     FractalView init: acquiring a surface
  fractalFrame <- tryCreateSurface w h 32
  -- AppController init: building the GUIController
  --   GUIController init: building the view
  --     GUIView init: acquiring a surface
  zoomWindow <- tryCreateZoomWindow w h zf
  let fractalView = F.StandardView screen fractalFrame
      fractalCtrl = FractalController progression zf model fractalView
      guiView     = G.StandardView screen zoomWindow
      guiCtrl     = GUIController True guiView
  return $ AppController screen fractalCtrl guiCtrl
  
tryCreateSurface :: Int -> Int -> Int -> MaybeT IO Surface
tryCreateSurface w h d = MaybeT $ SDL.tryCreateRGBSurface [HWSurface] w h d 0 0 0 0

tryGetScreen :: Int -> Int -> Int -> String -> MaybeT IO Surface
tryGetScreen w h d t = do
  screen <- MaybeT $ SDL.trySetVideoMode w h d [HWSurface, DoubleBuf]
  SDL.setCaption t [] -- we don’t give a fuck about the title icon
  return screen
