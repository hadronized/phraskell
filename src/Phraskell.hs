import Data.Maybe (maybe)
import Controller.AppController
import Controller.CLI (CLIFlag,CLIVersion,options,parseOpts,usage)

main = do
  hSetBuffering stdout NoBuffering

  args     <- getArgs
  cliflags <- parseOpts args
  maybe (putStrLn usage) entrypoint cliflags

entrypoint :: AppController -> IO ()
entrypoint app = do
  return ()
