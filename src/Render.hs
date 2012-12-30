module Render where

import Control.Monad
import Foreign
import Fractal
import Graphics.UI.SDL as SDL

-- init the render
initRender :: Int -> Int -> Int -> String -> IO Surface
initRender w h d t = do
  SDL.init [InitVideo]
  screen <- SDL.setVideoMode w h d [HWSurface, DoubleBuf]
  -- test whether the surface isn’t correctly created
  SDL.setCaption t [] -- we don’t give a fuck about the title icon
  return screen

-- finalize the render
destroyRender :: IO ()
destroyRender = SDL.quit

-- put a single pixel in a surface
putPixel :: Int -> Int -> SDL.Pixel -> Surface -> IO ()
putPixel u v p s = do
  pixels <- castPtr `liftM` surfaceGetPixels s
  pokeElemOff pixels (u + v*surfaceGetWidth s) p

updateFrame :: IterFrame -> Surface -> IO ()
updateFrame iterf screen =
  
