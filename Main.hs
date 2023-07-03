import Data.List (find)
import Data.Maybe (isJust, fromJust)

data Node = Node {
    name :: String,
    protectedBy :: [[Node]]
} deriving (Show, Eq)

type Graph = [Node]

addNode :: Node -> Graph -> Graph
addNode n g = g ++ [n]

getNode :: String -> Graph -> Maybe Node
getNode nname = find (\x -> name x == nname)

getNodes ::[String] -> Graph ->  [Node]
getNodes nnames g = map (\(Just x) -> x) $ filter (/= Nothing)  $ map (`getNode` g) nnames

addProtectedBy :: String -> [String] -> Graph -> Graph 
addProtectedBy nname nnames g = map (\x -> if name x == nname then updatedNode else x) g
    where
        accesses = getNodes nnames g
        existingNode = fromJust $ getNode nname g
        updatedNode = Node nname (protectedBy existingNode ++ [accesses])

printMermaidProtectedBy :: [Node] -> String -> String
printMermaidProtectedBy xs nname = concatMap (\ (Node xname _) -> xname ++ " --> " ++ nname ++ "\n") xs

printMermaidNode :: Node -> String
printMermaidNode (Node nname xs) = 
    "%% " ++ nname ++ "\n" ++ 
    nname ++ "\n\n" ++ 
    concatMap (\ x -> printMermaidProtectedBy x nname ++ "\n") xs

printMermaidGraph :: Graph  -> String
printMermaidGraph = concatMap printMermaidNode

printMermaidContent :: Graph -> String
printMermaidContent g = 
    "flowchart TD\n\n" ++ 
    printMermaidGraph g

main :: IO ()
main = do
    let 
        initialGraph = []
        updatedGraph1 = addNode Node { name = "pw_Bitwarden", protectedBy = [] } initialGraph 
        updatedGraph2 = addNode Node { name = "Phone", protectedBy = [] } updatedGraph1 
        updatedGraph3 = addNode Node { name = "Finger", protectedBy = [] } updatedGraph2 
        updatedGraph4 = addNode Node { name = "Bitwarden", protectedBy = [] } updatedGraph3 
        updatedGraph5 = addProtectedBy "Bitwarden" ["pw_Bitwarden"] updatedGraph4
        updatedGraph6 = addProtectedBy "Bitwarden" ["Finger", "Phone"] updatedGraph5
    print updatedGraph6
    writeFile "graph.mmd" (printMermaidContent updatedGraph6)
