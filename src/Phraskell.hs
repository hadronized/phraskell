import Data.Maybe (maybe)
import Control.Monad.Trans.Maybe
import Controller.App (runCtrl)
import Controller.Bootstrap
import Controller.CLI
import Controller.CrossInit
import System.IO
import System.Environment

main = do
  hSetBuffering stdout NoBuffering

  args     <- getArgs
  cliflags <- runMaybeT $ parseOpts args
  maybe (putStrLn usage) entrypoint cliflags

entrypoint :: [CLIFlag] -> IO ()
entrypoint f = do
  app <- runMaybeT $ crossInit (bootstrap f)
  maybe (putStrLn "failed to init") runCtrl app
