import Data.Maybe (maybe)
import Controller.App (runCtrl)
import Controller.CLI (CLIFlag,CLIVersion,options,parseOpts,usage)

main = do
  hSetBuffering stdout NoBuffering

  args     <- getArgs
  cliflags <- parseOpts args
  maybe (putStrLn usage) runCtrl cliflags
