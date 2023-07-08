module Graphviz (
    printGraph
) where

import qualified AccountAccessGraph as AAG (Node(..), Graph, CompromisionType (..))

data CompromisionType = Automatic | User | NotCompromised deriving (Show, Eq)

data Access = Access {
    names :: [String],
    color :: Int
} deriving (Show, Eq)

data Node = Node {
    name :: String,
    protectedBy :: [Access],
    compromisionType :: CompromisionType
} deriving (Show, Eq)

type Graph = [Node]

toCompromisionType :: AAG.CompromisionType -> CompromisionType
toCompromisionType AAG.Automatic = Automatic
toCompromisionType AAG.User = User
toCompromisionType AAG.NotCompromised = NotCompromised

toAccess :: ([String], Int) -> Access
toAccess (ns, colors) = Access ns colors

toNode :: AAG.Node -> Node
toNode n = Node (AAG.name n) (zipWith (curry toAccess) (AAG.protectedBy n) [1 .. ]) (toCompromisionType (AAG.compromisionType n))  

toGraph :: AAG.Graph -> Graph
toGraph = map toNode

printAccess :: String -> Access -> String
printAccess nname (Access ns color) = concatMap (\ n -> "\t" ++ n ++ " -> " ++ nname ++ " [color=\"" ++ show color ++ "\"]" ++ "\n") ns

printCompromisionType :: CompromisionType -> String
printCompromisionType Automatic = "dotted"
printCompromisionType User = "dashed"
printCompromisionType NotCompromised = "solid"

printNode :: Node -> String
printNode (Node nname xs compromisionType) =
    "\t# " ++ nname ++ "\n" ++ 
    "\t" ++ nname ++ "[style=\"" ++ printCompromisionType compromisionType ++ "\"]" ++ "\n\n" ++ 
    concatMap (\ x -> printAccess nname x ++ "\n") xs

printGraph :: AAG.Graph -> String
printGraph g = 
    "digraph {\n" ++ 
    "\tedge [colorscheme=\"dark28\"]\n\n" ++
    concatMap printNode (toGraph g) ++
    "}"
