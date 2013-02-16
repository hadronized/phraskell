module Render where

import Data.Bits
import Control.Monad
import Foreign
import FractalModel
import Graphics.UI.SDL as SDL

tryGetScreen :: Int -> Int -> Int -> String -> IO (Maybe Surface)
tryGetScreen w h d t = do
  SDL.init [InitVideo]
  screen <- SDL.trySetVideoMode w h d [HWSurface, DoubleBuf]
  SDL.setCaption t [] -- we don’t give a fuck about the title icon
  return screen

-- destroy the render
destroyRender :: IO ()
destroyRender = SDL.quit


