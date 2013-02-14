module Controller.CLI where

import System.Consol.GetOpt (NoArg,Option,OptDescr,ReqArg,usageInfo)

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

-- Display some usage informantion on standard output
usage :: String
usage = usageInfo "usage: phraskell [OPTIONS]" options

-- All possible CLI options
options :: [OptDescr CLIFlag]
options =
  [ Option ['v','?'] ["version", "about"] (NoArg CLIVersion)           "show version"
  , Option ['w']     ["width"]            (ReqArg CLIWidth "WIDTH")    "width of window"
  , Option ['h']     ["heigth"]           (ReqArg CLIHeight "HEIGHT")  "height of the window"
  , Option ['x']     ["rx","relx"]        (ReqArg CLIX "X")            "x displacement"
  , Option ['y']     ["ry","rely"]        (ReqArg CLIY "Y")            "y displacement"
  , Option ['z']     ["zoom"]             (ReqArg CLIZoom "ZOOM")      "zoom factor"
  , Option ['f']     ["fullscreen"]       (NoArg CLIFullscreen)        "launch in fullscreen"
  ]

-- Parse options and maybe return the CLI flags
parseOpts :: [String] -> MaybeT IO [CLIFlag]
parseOpts args =
  case getOpt Permute options args of
    (o,n,[])   -> return o
    _          -> mzero
