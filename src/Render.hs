module Render where

import Data.Bits
import Control.Monad
import Foreign
import Fractal
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

type UV = (Int,Int)
type UVs = [[UV]]

-- put a single pixel in a surface
putPixel :: SDL.Pixel -> Surface -> UV -> IO ()
putPixel p s (u,v) = do
  pixels <- castPtr `liftM` surfaceGetPixels s
  pokeElemOff pixels (u + v*surfaceGetWidth s) p

{- We simply need to traverse the iteration frame, and foreach iteration, transform it into
 - a pixel using the pixelize function, then write that pixel in the surface at the correct addrees.
 - We are about to need some functions:
 -  · the first one will be used to put a pixel in a surface; it takes the pixel to output, the surface
 -    and where in the surface we wanna put the pixel; ALREADY DONE
 -  · the second one is pixelize; we’ll use the nextColor function for that job
 -  · then, we need a function that apply the pixelize function on every pixels using the given iteration frame
 -    and surface
-}

-- pixelize an iteration value
pixelize :: Integer -> SDL.Pixel
pixelize i = Pixel $ (shift r 16) + (shift g 8) + b
  where (r,g,b) = toWord32 $ nextColor i red
        toWord32 (r,g,b) = (fromIntegral r, fromIntegral g, fromIntegral b)

-- pixelize an entire SDL Surface
pixelizeSurface :: IterFrame -> Surface -> IO ()
pixelizeSurface iterf surface = do
  foldM_ (\row line -> foldM_ (\col x -> f (row,col) x >> return (col+1)) 0 line >> return (row+1)) 0 iterf
    where width = surfaceGetWidth surface
          height = surfaceGetHeight surface
          f uv x = putPixel (pixelize x) surface uv

-- to move in some utils module
type RGBColor = (Word8,Word8,Word8)

red :: RGBColor
red = (255,0,0)

nextColor :: Integer -> RGBColor -> RGBColor
nextColor 0 (r,g,b) = (r,g,b)
nextColor i (r,g,b) =
  case (r,g,b) of
    -- remarkable values
    (255,0,0)   -> (255,iw,0)
    (255,255,0) -> (255-iw,255,0)
    (0,255,0)   -> (0,255,iw)
    (0,255,255) -> (0,255-iw,255)
    (0,0,255)   -> (iw,0,255)
    (255,0,255) -> (255,0,255-iw)
    -- other values
    (255,g,0)   -> (255,g+iw,0)
    (r,255,0)   -> (r-iw,255,0)
    (0,255,b)   -> (0,255,b+iw)
    (0,g,255)   -> (0,g-iw,255)
    (r,0,255)   -> (r+iw,0,255)
    (255,0,b)   -> (255,0,b-iw)
  where iw = fromIntegral i
