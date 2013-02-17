module Model.Fractal where

import Model.Progression

type FractalRef = [[FComplex]]

screen :: Double -> Double -> (FComplex -> FComplex) -> FractalRef
screen w h f = map (\x -> [ f $ toCart w h (x :+ y) | y <- [0..h-1] ]) [0..w-1]

toCart :: Double -> Double -> FComplex -> FComplex
toCart w h (x :+ y) = r*(2 * x / w - 1) :+ (1 - 2 * y / h)
  where r = w / h

oZoom :: Double -> FComplex -> FComplex
oZoom z (x :+ y) = (x/z) :+ (y/z)

offsets :: Double -> Double -> FComplex -> FComplex
offsets rx ry (x :+ y) = (x+rx) :+ (y+ry)

data FractalModel
  = IterFrame [[Integer]]
  -- | Buddhabrot ...

-- for x and y, evaluate the fractal equation to generate a single element of the IterFrame model
evalFrac :: FractalProgression -> FComplex -> FComplex -> Integer -> Integer
evalFrac e z1 xy1 m = go z1 xy1 0
  where go z@(rp :+ ip) xy i
          | i > m = -1
          | rp^2 + ip^2 > 4.0 = i
          | otherwise = go ((progression e) z xy) xy (i+1)

-- TODO: to simplify
mkIterFrame :: FractalProgression -> Double -> Double -> Double -> Double -> Double -> Integer -> FractalModel
mkIterFrame p w h xd yd z i =
  let ref  = screen w h $ offsets xd yd . oZoom z
      eval = map $ map (\(x :+ y) -> evalFrac p (start p) (x :+ y) i)
  in IterFrame $ eval ref
