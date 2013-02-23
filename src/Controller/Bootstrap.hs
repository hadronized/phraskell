module Controller.Bootstrap where

import Controller.CLI
import Controller.Fractal
import Model.Fractal

data Bootstrap = Bootstrap {
    bootFullscreen :: Bool
  , bootWidth      :: Int
  , bootHeight     :: Int
  , bootX          :: Double
  , bootY          :: Double
  , bootZoom       :: Double
  , bootMaxIter    :: Integer
  , bootModel      :: FractalModel
  , bootModelProc  :: ModelProcessor
}

def :: Bootstrap
def = Bootstrap True 800 600 (-0.5) 0 1 50 (IterFrame []) SoftModelProcessor

bootstrap :: [CLIFlag] -> Bootstrap
bootstrap = foldl alterBootstrap def

alterBootstrap :: Bootstrap -> CLIFlag -> Bootstrap
alterBootstrap b f = case f of
  CLIFullscreen  -> b { bootFullscreen = True }
  CLIWidth w     -> b { bootWidth = w }
  CLIHeight h    -> b { bootHeight = h }
  CLIX x         -> b { bootX = x }
  CLIY y         -> b { bootY = y }
  CLIZoom z      -> b { bootZoom = z }
  CLIMaxIter i   -> b { bootMaxIter = i }
  CLIModel str   -> b { bootModel = string2Model str } -- TODO: wat
  --CLIHard        -> b { bootModelProc =  }
  _              -> b

string2Model :: String -> FractalModel
string2Model "iter" = IterFrame []
string2Model _      = IterFrame []
