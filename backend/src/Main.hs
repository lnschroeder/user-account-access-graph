import CommandHandler (queryGraphName)
import RESTController (controller)
import System.Environment (getArgs)
import Utils (query)

main :: IO ()
main = do
  putStrLn $ query "Welcome to DILMA (Did I Lose My Account?)!"
  args <- getArgs
  case args of
    ["rest"] -> RESTController.controller
    _ -> CommandHandler.queryGraphName
