module Graphviz (
    printGraph
) where

import qualified AccountAccessGraph as AAG (Node(..), Graph)

data Access = Access {
    names :: [String],
    color :: Int
} deriving (Show, Eq)

data Node = Node {
    name :: String,
    protectedBy :: [Access],
    selected :: Bool
} deriving (Show, Eq)

type Graph = [Node]

toAccess :: ([String], Int) -> Access
toAccess (ns, colors) = Access ns colors

toNode :: AAG.Node -> Node
toNode n = Node (AAG.name n) (zipWith (curry toAccess) (AAG.protectedBy n) [1 .. ]) (AAG.selected n)  

toGraph :: AAG.Graph -> Graph
toGraph = map toNode

printAccess :: String -> Access -> String
printAccess nname (Access ns color) = concatMap (\ n -> "\t" ++ n ++ " -> " ++ nname ++ " [color=\"" ++ show color ++ "\"]" ++ "\n") ns
    
printNode :: Node -> String
printNode (Node nname xs selected) =
    "\t# " ++ nname ++ "\n" ++ 
    "\t" ++ nname ++ "[style=\"" ++ style ++ "\"]" ++ "\n\n" ++ 
    concatMap (\ x -> printAccess nname x ++ "\n") xs
    where style = if selected then "bold" else "solid"

printGraph :: AAG.Graph -> String
printGraph g = 
    "digraph {\n" ++ 
    "\tedge [colorscheme=\"dark28\"]\n\n" ++
    concatMap printNode (toGraph g) ++
    "}"
