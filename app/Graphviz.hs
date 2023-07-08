module Graphviz
  ( printGraph,
  )
where

import qualified AccountAccessGraph as AAG (CompromisionType (..), Graph, Node (..))
import qualified Data.Set as Set
import Text.Printf (printf)

data CompromisionType = Automatic | User | NotCompromised deriving (Show, Eq)

data Access = Access
  { names :: [String],
    color :: Int
  }
  deriving (Show, Eq)

data Node = Node
  { name :: String,
    protectedBy :: [Access],
    compromisionType :: CompromisionType
  }
  deriving (Show, Eq)

type Graph = [Node]

toProtectedBy :: Set.Set (Set.Set String) -> [Access]
toProtectedBy p = zipWith (Access . Set.toList) (Set.toList p) [1 ..]

toCompromisionType :: AAG.CompromisionType -> CompromisionType
toCompromisionType AAG.Automatic = Automatic
toCompromisionType AAG.User = User
toCompromisionType AAG.NotCompromised = NotCompromised

toNode :: AAG.Node -> Node
toNode n =
  Node (AAG.name n) (toProtectedBy (AAG.protectedBy n)) (toCompromisionType (AAG.compromisionType n))

toGraph :: AAG.Graph -> Graph
toGraph = map toNode

printAccess :: String -> Access -> String
printAccess nname (Access ns color) =
  concatMap
    ( \n ->
        printf
          "\t%s -> %s [color = \"%s\";];\n"
          n
          nname
          (show color)
    )
    ns

printCompromisionType :: CompromisionType -> String
printCompromisionType Automatic = "dashed,bold"
printCompromisionType User = "bold"
printCompromisionType NotCompromised = "solid"

printNode :: Node -> String
printNode (Node nname xs compromisionType) =
  printf
    ( "\t# %s\n"
        ++ "\t%s [style = \"%s\";];\n\n"
        ++ "%s"
    )
    nname
    nname
    (printCompromisionType compromisionType)
    (concatMap (\x -> printAccess nname x ++ "\n") xs)

printGraph :: AAG.Graph -> String
printGraph g =
  "digraph {\n"
    ++ "\tedge [colorscheme = \"dark28\";];\n\n"
    ++ concatMap printNode (toGraph g)
    ++ "}"
