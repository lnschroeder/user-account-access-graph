import CommandHandler (queryOutputFile)

main :: IO ()
main = do
  putStrLn "Welcome to DILMA ('Did I Lose My Account?'). Use help for a list of commands."
  CommandHandler.queryOutputFile
