module AccountAccessGraph (
    Node(..),
    Graph,
    addNode,
    addProtectedBy
) where

import Data.List (find)
import Data.Maybe (fromJust)

data Node = Node {
    name :: String,
    protectedBy :: [[Node]]
} deriving (Show, Eq)

type Graph = [Node]

nodeHasName ::Node -> String -> Bool
nodeHasName (Node nname _) s = s == nname

addNode :: String -> Graph -> Graph
addNode nname g = g ++ [Node nname []]

getNode :: String -> Graph -> Maybe Node
getNode nname = find (`nodeHasName` nname)

getNodes ::[String] -> Graph ->  [Node]
getNodes nnames g = map (\(Just x) -> x) $ filter (/= Nothing)  $ map (`getNode` g) nnames

addProtectedBy :: String -> [String] -> Graph -> Graph 
addProtectedBy nname nnames g = map (\x -> if x `nodeHasName` nname then updatedNode else x) g
    where
        accesses = getNodes nnames g
        existingNode = fromJust $ getNode nname g
        updatedNode = Node nname (protectedBy existingNode ++ [accesses])
