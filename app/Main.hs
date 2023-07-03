import AccountAccessGraph (Node(..), Graph, addNode, addProtectedBy)
import CommandHandler

main :: IO ()
main = do
    let 
        initialGraph = []
    putStrLn "Welcome to your account access graph. Use help for a list of commands."
    CommandHandler.commandHandler initialGraph
