import AccountAccessGraph (Node(..), Graph, addNode, addProtectedBy)
import CommandHandler ( commandHandler )

main :: IO ()
main = do
    putStrLn "Welcome to your account access graph. Use help for a list of commands."
    CommandHandler.commandHandler []
