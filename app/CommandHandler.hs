module CommandHandler (
    commandHandler
) where
    
import Data.List (isPrefixOf)
import Data.List.Split (splitOn)
import Graphviz (printGraph)
import qualified AccountAccessGraph as AAG
import System.IO

extractCommandParameters :: String -> Int -> [String]
extractCommandParameters cmd l = drop l (splitOn " " cmd)

invoke :: String -> AAG.Graph -> (String, AAG.Graph)
invoke cmd graph
    | "add node " `isPrefixOf` cmd = do
        let name = head $ extractCommandParameters cmd 2
        ("Added node " ++ name, AAG.addNode name graph)
    | "add access " `isPrefixOf` cmd = do
        let nodes = extractCommandParameters cmd 2 
        ("Added access " ++ show nodes, AAG.addProtectedBy (head nodes) (tail nodes) graph)
    | "select " `isPrefixOf` cmd = do
        let nodes = extractCommandParameters cmd 1 
        ("Selected node(s) " ++ show nodes, foldl (flip AAG.selectNode) graph nodes)
    | "unselect" == cmd = do
        let nodes = map AAG.name (filter AAG.selected graph)
        ("Unselected node(s) " ++ show nodes, foldl (flip AAG.unselectNode) graph nodes)
    | "unselect " `isPrefixOf` cmd = do
        let nodes = extractCommandParameters cmd 1 
        ("Unselected all nodes", foldl (flip AAG.unselectNode) graph nodes)
    | "canAccess " `isPrefixOf` cmd = do
        let nodes = extractCommandParameters cmd 1 
        (if AAG.isNodeAccessible (head nodes) graph then "y" else "n", graph)
    | "example" == cmd = do
        ("Generated example graph", AAG.example)
    | "clear" == cmd = do
        ("Cleared graph", [])
    | "help" == cmd = do
        ("available commands:\n" ++
            "  add node <node name>\n" ++
            "    - adds a new node to the graph\n" ++
            "  add access <node name> <protector1> <protector2> ...\n" ++
            "    - adds a list of nodes (protectors) to a given node as access\n" ++
            "  select <node name 1> <node name 2> ...\n" ++
            "    - selects the given nodes\n" ++
            "  unselect [<node name 1> <node name 2> ...]\n" ++
            "    - unselects the given nodes\n" ++
            "    - or unselects all nodes if none were specified\n" ++
            "  example\n" ++
            "    - generates an example graph\n" ++
            "  clear\n" ++
            "   - reset the graph", graph)
    | otherwise = ("Unknown command", graph)

commandHandler :: AAG.Graph -> IO ()
commandHandler g = do
    putStr "> "
    hFlush stdout
    name <- getLine
    let (message, graph) = invoke name g
    writeFile "graph.dot" (printGraph graph)
    putStrLn message
    commandHandler graph



    
