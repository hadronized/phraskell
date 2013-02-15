import Data.Maybe (maybe)
import CLI (CLIFlag,CLIVersion,options,parseOpts,usage)

main = do
  hSetBuffering stdout NoBuffering

  args     <- getArgs
  cliflags <- parseOpts args
  maybe (putStrLn usage) ??? cliflags
