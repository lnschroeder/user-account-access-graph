data Node = Node {
    name :: String,
    protectedBy :: [[Node]]
} deriving (Show)

newtype Graph = Graph {
    nodes :: [Node]
} deriving (Show)

addNode :: Graph -> Node -> Graph
addNode (Graph g) n = Graph (g ++ [n])

-- adds nodes to the protectedBy field for a given node
addProtectedBy :: Graph -> String -> [Node] -> Graph
addProtectedBy g = addProtectedBy' g (Graph [])

addProtectedBy' :: Graph -> Graph -> String -> [Node] -> Graph 
addProtectedBy' (Graph []) g _ _ = g
addProtectedBy' (Graph (x:xs)) (Graph new) nname accesses
    | name x == nname = addProtectedBy' (Graph xs) (Graph (new ++ [Node nname (protectedBy x ++ [accesses])])) nname accesses
    | otherwise = addProtectedBy' (Graph xs) (Graph (new ++ [x])) nname accesses

main :: IO ()
main = do
    let 
        initialGraph = Graph []
        updatedGraph1 = addNode initialGraph Node { name = "pw_Bitwarden", protectedBy = [[]] }
        updatedGraph2 = addNode updatedGraph1 Node { name = "Phone", protectedBy = [[]] }
        updatedGraph3 = addNode updatedGraph2 Node { name = "Finger", protectedBy = [[]] }
        updatedGraph4 = addNode updatedGraph3 Node { name = "Bitwarden", protectedBy = [[]] }
        updatedGraph5 = addProtectedBy updatedGraph4 "Bitwarden" [Node { name = "pw_Bitwarden", protectedBy = [[]] }]
        -- putStrLn "Graph1:"
    putStrLn "Test"
    print initialGraph
    print updatedGraph1
    print updatedGraph2
    print updatedGraph3
    print updatedGraph4
    print updatedGraph5

    -- updatedGraph2 = test updatedGraph Node { name = "Finger" }

-- Graph {nodes = [Node {name = "pw_Bitwarden", protectedBy = [[]]},Node {name = "Phone", protectedBy = [[]]},Node {name = "Finger", protectedBy = [[]]}]}
