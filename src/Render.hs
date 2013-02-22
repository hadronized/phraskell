module Render where

import Data.Bits
import Control.Monad
import Foreign
import FractalModel
import Graphics.UI.SDL as SDL

tryGetScreen :: Int -> Int -> Int -> String -> IO (Maybe Surface)
tryGetScreen w h d t = do
  SDL.init [InitVideo]
  scr <- SDL.trySetVideoMode w h d [HWSurface, DoubleBuf]
  SDL.setCaption t [] -- we don’t give a fuck about the title icon
  return scr

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

-- pixelize an iteration value
pixelize :: Integer -> SDL.Pixel
pixelize (-1) = Pixel (0 :: Word32)
pixelize i  = Pixel $ (shift r 16) + (shift g 8) + b
{-
  where (r,g,b) = toWord32 $ (0,0,imod256)
        imod256 = i `mod` 256
        toWord32 (r,g,b) = (fromIntegral r, fromIntegral g, fromIntegral b)
-}
  where (r,g,b) = toWord32 $ decodeColor (6*i) 
        toWord32 (x,y,z) = (fromIntegral x, fromIntegral y, fromIntegral z)

-- pixelize an entire SDL Surface
pixelizeSurface :: FractalModel -> Surface -> IO ()
pixelizeSurface (IterFrame iterf) surface = do
  foldM_ (\row line -> foldM_ (\col x -> f (row,col) x >> return (col+1)) 0 line >> return (row+1)) 0 iterf
    where f uv x = putPixel (pixelize x) surface uv

-- to move in some utils module
type RGBColor = (Word8,Word8,Word8)

-- decode a rainbow color
decodeColor :: Integer -> RGBColor
decodeColor i
  | isRed c    = (255,contrib,0)
  | isYellow c = (ncontrib,255,0)
  | isGreen c  = (0,255,contrib)
  | isAzure c  = (0,ncontrib,255)
  | isBlue  c  = (contrib,0,255)
  | otherwise  = (255,0,ncontrib)
    where cr        = 255 -- color range - 1
          contrib   = fromIntegral $ (c `mod` cr)
          ncontrib  = fromIntegral $ 255 - contrib
          c         = i `mod` (6*cr)
          isRed     = (<cr)
          isYellow  = (<2*cr)
          isGreen   = (<3*cr)
          isAzure   = (<4*cr)
          isBlue    = (<5*cr)
