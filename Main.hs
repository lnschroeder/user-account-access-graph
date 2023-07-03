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
getNode s = find (\x -> name x == s)

getNodes ::[String] -> Graph ->  [Node]
getNodes s g = map (\(Just x) -> x) $ filter (/= Nothing)  $ map (`getNode` g) s

addProtectedBy :: String -> [String] -> Graph -> Graph 
addProtectedBy nname nnames g = map (\x -> if name x == nname then updatedNode else x) g
    where
        accesses = getNodes nnames g
        existingNode = fromJust $ getNode nname g
        updatedNode = Node nname (protectedBy existingNode ++ [accesses])

printMermaidProtectedBy :: [Node] -> String -> String
printMermaidProtectedBy [] n = ""
printMermaidProtectedBy (x:xs) n = name x ++ " --> " ++ n ++ "\n" ++ printMermaidProtectedBy xs n

printMermaidNode :: Node -> String
printMermaidNode (Node n []) = ""
printMermaidNode n = printMermaidProtectedBy x nname ++ "\n" ++ printMermaidNode (Node nname xs) 
    where 
        (x:xs) = protectedBy n
        nname = name n

printMermaidGraph :: Graph  -> String
printMermaidGraph = concatMap printMermaidNode

printMermaidContent :: Graph -> String
printMermaidContent g = "flowchart TD\n\n" ++ printMermaidGraph g

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
    writeFile "graph.mmd" (printMermaidContent updatedGraph6)
