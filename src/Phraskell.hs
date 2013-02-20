import Data.Maybe (maybe)
import Control.Monad.Trans.Maybe
import Controller.App
import Controller.Bootstrap
import Controller.CLI
import Controller.Init as C
import System.IO
import System.Environment

main = do
  hSetBuffering stdout NoBuffering

  args     <- getArgs
  cliflags <- parseOpts args
  maybe (putStrLn usage) entrypoint cliflags

entrypoint :: [CLIFlag] -> IO ()
entrypoint f = do
  app <- C.init (bootstrap f)
  maybe (putStrLn "failed to init") run app
