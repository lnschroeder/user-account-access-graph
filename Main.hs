import Data.List (find)

data Node = Node {
    name :: String,
    protectedBy :: [[Node]]
} deriving (Show, Eq)

type Graph = [Node]

addNode :: Graph -> Node -> Graph
addNode g n = g ++ [n]

getNode :: String -> Graph -> Maybe Node
getNode s = find (\x -> name x == s)

getNodes :: Graph -> [String] -> [Node]
getNodes g s = map (\(Just x) -> x) $ filter (/= Nothing)  $ map (`getNode` g) s
    
-- adds nodes to the protectedBy field for a given node
addProtectedBy :: Graph -> String -> [String] -> Graph
addProtectedBy g = addProtectedBy' g []

addProtectedBy' :: Graph -> Graph -> String -> [String] -> Graph 
addProtectedBy' [] g _ _ = g
addProtectedBy' (x:xs) new nname nnames
    | name x == nname = addProtectedBy' xs (new ++ [Node nname (protectedBy x ++ [accesses])]) nname nnames
    | otherwise = addProtectedBy' xs (new ++ [x]) nname nnames
    where
        accesses = getNodes ((x:xs) ++ new) nnames

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
        updatedGraph1 = addNode initialGraph Node { name = "pw_Bitwarden", protectedBy = [] }
        updatedGraph2 = addNode updatedGraph1 Node { name = "Phone", protectedBy = [] }
        updatedGraph3 = addNode updatedGraph2 Node { name = "Finger", protectedBy = [] }
        updatedGraph4 = addNode updatedGraph3 Node { name = "Bitwarden", protectedBy = [] }
        updatedGraph5 = addProtectedBy updatedGraph4 "Bitwarden" ["pw_Bitwarden"]
        updatedGraph6 = addProtectedBy updatedGraph5 "Bitwarden" ["Finger", "Phone"]
        -- putStrLn "Graph1:"
    -- putStrLn "Test"
    -- print initialGraph
    -- print updatedGraph1
    -- print updatedGraph2
    -- print updatedGraph3
    -- print updatedGraph4
    -- print updatedGraph5
    putStrLn (printMermaidContent updatedGraph6)
    
    -- updatedGraph2 = test updatedGraph Node { name = "Finger" }

-- Graph {nodes = [Node {name = "pw_Bitwarden", protectedBy = [[]]},Node {name = "Phone", protectedBy = [[]]},Node {name = "Finger", protectedBy = [[]]}]}
