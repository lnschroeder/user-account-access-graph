module Graphviz (
    printGraph
) where

import qualified AccountAccessGraph as AAG (Node(..), Graph)

data Access = Access {
    names :: [String],
    color :: String
} deriving (Show, Eq)

data Node = Node {
    name :: String,
    protectedBy :: [Access]
} deriving (Show, Eq)

type Graph = [Node]

toAccess :: [AAG.Node] -> Access
toAccess ns = Access names (concat names)
    where
        names = map AAG.name ns

toNode :: AAG.Node -> Node
toNode n = Node (AAG.name n) (map toAccess (AAG.protectedBy n))

toGraph :: AAG.Graph -> Graph
toGraph = map toNode

printAccess :: String -> Access -> String
printAccess nname (Access ns color) = concatMap (\ n -> "\t" ++ n ++ " -> " ++ nname ++ " [color=\"" ++ color ++ "\"]" ++ "\n") ns
    
printNode :: Node -> String
printNode (Node nname xs) = 
    "\t# " ++ nname ++ "\n" ++ 
    "\t" ++ nname ++ "\n\n" ++ 
    concatMap (\ x -> printAccess nname x ++ "\n") xs

printGraph :: AAG.Graph -> String
printGraph g = 
    "digraph {\n\n" ++ 
    concatMap printNode (toGraph g) ++
    "}"
