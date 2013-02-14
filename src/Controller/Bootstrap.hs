module Controller.Bootstrap where

import Controller.CLI (CLIFlag)
import Model.Fractal

data Bootstrap = Bootstrap {
    bootFullscreen :: Bool
  , bootWidth      :: Int
  , bootHeight     :: Int
  , bootX          :: Float
  , bootY          :: Float
  , bootZoom       :: Float
  , bootMaxIter    :: Integer
  , bootModel      :: FractalModel
}

def :: Bootstrap
def = Bootstrap True 800 600 0 0 1 50

bootstrap :: [CLIFlag] -> Bootstrap
bootstrap f = foldl alterBootstrap def

alterBootstrap :: CLIFlag -> Bootstrap -> Bootstrap
alterBootstrap f b = case f of
  CLIFullscreen  -> b { bootFullscreen = True }
  CLIWidth w     -> b { bootWidth = w }
  CLIHeight h    -> b { bootHeight = h }
  CLIX x         -> b { bootX = x }
  CLIY y         -> b { bootY = y }
  CLIZoom z      -> b { bootZoom = z }
  CLIMayIter i   -> b { bootMaxIter = i }
  CLIModel str   -> b { bootModel = string2Model str } -- TODO: wat
  _              -> b

string2Model :: String -> FractalModel
string2Model "iter" = IterFrame []
string2Model _      = IterFrame []
