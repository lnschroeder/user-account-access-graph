{-# Language DuplicateRecordFields #-}

data NodeType = Account | Device | Biometric | Knowledge deriving (Show)
data Node = Node {
    name :: String,
    ofType :: NodeType,
    protectedBy :: [Access]
} deriving (Show)


data AccessType = Primary | Recovery deriving (Show)
data Access = Access {
    nodes :: [Node],
    ofType :: AccessType
} deriving (Show)

newtype Graph = Graph {
    nodes :: [Node]
} deriving (Show)

addNode :: Graph -> Node -> Graph
-- TODO Urs 

addAccess :: Graph -> Node -> Access -> Graph
addAccess graph node access = 

-- main function
main :: IO ()
main = do 
    putStrLn "Hello World"
