module CommandHandler (
    commandHandler
) where
    
import Data.List (isPrefixOf)
import Graphviz (printGraph)
import qualified AccountAccessGraph as AAG

extractCommandParameter :: String -> String -> String
extractCommandParameter cmd = drop (length cmd)

    -- writeFile "graph.dot" (printGraph updatedGraph6)

invoke :: String -> AAG.Graph -> (String, AAG.Graph)
invoke cmd graph
    | "add node " `isPrefixOf` cmd = do
        let name = extractCommandParameter "add node " cmd
        ("Added node " ++ name, AAG.addNode name graph)
    | otherwise = ("Unknown command", graph)


commandHandler :: AAG.Graph -> IO ()
commandHandler g = do
    name <- getLine
    let (message, graph) = invoke name g
    writeFile "graph.dot" (printGraph graph)
    putStrLn message
    commandHandler graph



    
