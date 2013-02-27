module Controller.Init where

import Control.Monad.Trans
import Control.Monad.Trans.Maybe
import Controller.App
import Controller.Fractal
import Controller.GUI
import Controller.Bootstrap
import Graphics.UI.SDL as SDL
import Model.Progression
import Model.Fractal
import View.Fractal as F
import View.GUI as G

title :: String
title = "phraskell 0.2.0"

init :: Bootstrap -> IO (Maybe AppController)
init b = do
  SDL.init [InitVideo] -- first thing to do, init SDL
  let w     = bootWidth b
      h     = bootHeight b
      x     = bootX b
      y     = bootY b
      z     = bootZoom b
      mi    = bootMaxIter b
      model = bootModel b
      p     = mandelbrot -- bootProgression b
      zf    = 2 -- bootZoomFactor
  runMaybeT $ do
    screen <- tryGetScreen w h 32 title
    fractalFrame <- tryCreateSurface w h 32
    zoomWindow <- tryCreateZoomWindow w h zf
    let fractalView = F.StandardView screen fractalFrame
        guiView     = G.StandardView screen zoomWindow
    mp <- lift $ createModelProc $ bootModelProc b
    return $ AppController screen p (fromIntegral w) (fromIntegral h) x y z zf mi model fractalView True guiView mp  
tryCreateSurface :: Int -> Int -> Int -> MaybeT IO Surface
tryCreateSurface w h d = MaybeT $ SDL.tryCreateRGBSurface [HWSurface] w h d 0 0 0 0

tryGetScreen :: Int -> Int -> Int -> String -> MaybeT IO Surface
tryGetScreen w h d t = do
  screen <- MaybeT $ SDL.trySetVideoMode w h d [HWSurface, DoubleBuf]
  lift $ SDL.setCaption t [] -- we don’t give a fuck about the title icon
  return screen

createModelProc :: Bool -> IO ModelProcessor
createModelProc soft = do
  return SoftModelProcessor
