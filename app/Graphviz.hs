module Graphviz (
    printGraph
) where

import qualified AccountAccessGraph as AAG (Node(..), Graph)
import Data.Hashable (hash)
import Numeric (showHex)

data Access = Access {
    names :: [String],
    color :: String
} deriving (Show, Eq)

data Node = Node {
    name :: String,
    protectedBy :: [Access]
} deriving (Show, Eq)

type Graph = [Node]

stringToHexColor :: String -> String
stringToHexColor s = "#" ++ showHex (hash s `mod` 16777216) ""

toAccess :: [String] -> Access
toAccess ns = Access ns (stringToHexColor (concat ns))

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
