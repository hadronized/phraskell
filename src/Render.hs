module Render where

import Fractal
import Graphics.UI.SDL as SDL

initRender :: Int -> Int -> Int -> String -> IO Surface
initRender w h d t = do
    SDL.init [InitVideo]
    screen <- SDL.setVideoMode w h d [HWSurface, DoubleBuf]
    -- test whether the surface isn’t correctly created
    SDL.setCaption t [] -- we don’t give a fuck about the title icon
    return screen

destroyRender :: IO ()
destroyRender = SDL.quit
{-
putPixel :: Surface -> Pixel -> Int -> Int -> Surface
putPixel s p u v =
-}
