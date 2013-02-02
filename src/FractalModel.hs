module FractalModel where

import Data.Complex
import Fractal
import Viewer

type FractalRef = [[FComplex]]

screen :: Float -> Float -> (FComplex -> FComplex) -> FractalRef
screen w h f = map (\x -> [ f $ toCart w h (x :+ y) | y <- [0..h-1] ]) [0..w-1]

toCart :: Float -> Float -> FComplex -> FComplex
toCart w h (x :+ y) = (2 * r*x / w - 1) :+ (1 - 2 * y / h)
  where r = w / h

oZoom :: Float -> FComplex -> FComplex
oZoom z (x :+ y) = (x/z :+ y/z)

offsets :: Float -> Float -> FComplex -> FComplex
offsets rx ry (x :+ y) = (x+rx) :+ (y+ry)

data FractalModel
  = IterFrame [[Integer]]

  -- for x and y, evaluate the fractal equation
evalFrac :: FractalProgression -> FComplex -> FComplex -> Integer -> Integer
evalFrac e z1 xy1 m = go z1 xy1 0
  where go z@(rp :+ ip) xy i
          | i > m = -1
          | rp^2 + ip^2 > 4.0 = i
          | otherwise = go (e z xy) xy (i+1)

-- make the iterations frame
-- TODO: add maxiter considerations
mkIterFrame :: Viewer -> FractalModel
mkIterFrame v =
  let w     = viewerWidth v
      h     = viewerHeight v
      rx    = viewerX v
      ry    = viewerY v
      z     = viewerZoom v
      p     = viewerProgression v
      ref   = screen w h $ offsets rx ry . oZoom z
      eval  = map $ map (\(x :+ y) -> evalFrac p (fromInteger 0) (x :+ y) 500)
  in IterFrame $ eval ref
