module Controller.CLI where

import System.Console.GetOpt

-- CLI flag used to customize the applications behavior
data CLIFlag
  = CLIVersion            -- version of the program
  | CLIFullscreen         -- should the app in fullscreen mode?
  | CLIWidth      Int     -- width of the screen
  | CLIHeight     Int     -- heigth of the sceen
  | CLIX          Double  -- x value
  | CLIY          Double  -- y value
  | CLIZoom       Double  -- zoom value
  | CLIMaxIter    Integer -- max iteration value
  | CLIModel      String  -- model flag
  | CLIHard               -- have we to run the model computation on hardware?

-- Display some usage information on standard output
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
  , Option ['m']     ["model"]            (ReqArg  CLIModel "MODEL")           "select the fractal model to use"
  , Option ['i']     ["iter"]             (ReqArg (CLIMaxIter . read) "ITER")  "maximum iteration"
  , Option ['a']     ["hard"]             (NoArg CLIHard)                      "use hardware acceleration"
  ]

-- Parse options and maybe return the CLI flags
parseOpts :: [String] -> IO (Maybe [CLIFlag])
parseOpts args =
  case getOpt Permute options args of
    (o,n,[])   -> return $ Just o
    _          -> return Nothing
