module Controller.Bootstrap where

import Controller.CLI (CLIFlag)

data Bootstrap = Bootstrap {
    bootFullscreen :: Bool
  , bootWidth      :: Int
  , bootHeight     :: Int
  , bootX          :: Float
  , bootY          :: Float
  , bootZoom       :: Float
  , bootMaxIter    :: Integer
}

def :: Bootstrap
def = Bootstrap True 800 600 0 0 1 50

bootstrap :: [CLIFlag] -> Bootstrap
bootstrap f = foldl alterBootstrap def

alterBootstrap :: CLIFlag -> Bootstrap -> Bootstrap
alterBootstrap f b = case f of
  CLIFullscreen  -> b { bootFullscreen = True }
  CLIWidth w     -> b { bootWidth = w }
  CLIHegiht h    -> b { bootHegiht = h }
  CLIX x         -> b { bootX = x }
  CLIY y         -> b { bootY = y }
  CLIZoom z      -> b { bootZoom = z }
  CLIMayIter i   -> b { bootMaxIter = i }
  _              -> b
