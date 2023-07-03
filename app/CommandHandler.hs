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



    
