import CommandHandler (queryGraphName)
import RESTController (controller)
import System.Environment

main :: IO ()
main = do
  putStrLn "\ESC[32mWelcome to DILMA (Did I Lose My Account?)!"
  args <- getArgs
  case args of
    ["rest"] -> RESTController.controller
    _ -> CommandHandler.queryGraphName
