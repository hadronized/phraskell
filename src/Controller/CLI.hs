module Controller.CLI where

import Control.Monad
import Control.Monad.Trans.Maybe
import System.Console.GetOpt

-- CLI flag used to customize the applications behavior
data CLIFlag
  = CLIVersion            -- version of the program
  | CLIFullscreen         -- should the app in fullscreen mode?
  | CLIWidth      Int     -- width of the screen
  | CLIHeight     Int     -- heigth of the sceen
  | CLIX          Float   -- x value
  | CLIY          Float   -- y value
  | CLIZoom       Float   -- zoom value
  | CLIMaxIter    Integer -- max iteration value
  | CLIModel      String  -- model flag

-- Display some usage informantion on standard output
usage :: String
usage = usageInfo "usage: phraskell [OPTIONS]" options

-- All possible CLI options
options :: [OptDescr CLIFlag]
options =
  [ Option ['v','?'] ["version", "about"] (NoArg   CLIVersion)                 "show version"
  , Option ['w']     ["width"]            (ReqArg (CLIWidth . read) "WIDTH")   "width of window"
  , Option ['h']     ["heigth"]           (ReqArg (CLIHeight . read) "HEIGHT") "height of the window"
  , Option ['x']     ["rx","relx"]        (ReqArg (CLIX . read) "X")           "x displacement"
  , Option ['y']     ["ry","rely"]        (ReqArg (CLIY . read) "Y")           "y displacement"
  , Option ['z']     ["zoom"]             (ReqArg (CLIZoom . read) "ZOOM")     "zoom factor"
  , Option ['f']     ["fullscreen"]       (NoArg   CLIFullscreen)              "launch in fullscreen"
  , Option ['m']     ["model"]            (ReqArg  CLIModel "iter")            "select the fracta model to use"
  ]

-- Parse options and maybe return the CLI flags
parseOpts :: [String] -> MaybeT IO [CLIFlag]
parseOpts args =
  case getOpt Permute options args of
    (o,n,[])   -> return o
    _          -> mzero
