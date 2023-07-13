import CommandHandler (queryGraphName)

main :: IO ()
main = do
  putStrLn "\ESC[32mWelcome to DILMA (Did I Lose My Account?)!"
  CommandHandler.queryGraphName
