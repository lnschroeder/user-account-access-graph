import AccountAccessGraph (Graph, Node (..), addNode, addProtectedBy)
import CommandHandler (commandHandler)

main :: IO ()
main = do
  putStrLn "Welcome to DILMO ('Did I Lock Myself Out?'). Use help for a list of commands."
  CommandHandler.commandHandler []
