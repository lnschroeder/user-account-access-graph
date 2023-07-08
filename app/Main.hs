import AccountAccessGraph (Graph, Node (..), addNode, addProtectedBy)
import CommandHandler (commandHandler)

main :: IO ()
main = do
  putStrLn "Welcome to DILMA ('Did I Lose My Account?'). Use help for a list of commands."
  CommandHandler.commandHandler []
