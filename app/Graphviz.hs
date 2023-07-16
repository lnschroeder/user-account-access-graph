module Graphviz
  ( saveToFile,
  )
where

import qualified AccountAccessGraph as AAG (CompromisionType (..), Graph, Node (..))
import Data.List (intercalate, sort)
import qualified Data.Set as Set
import Text.Printf (printf)

data CompromisionType = Automatic | User | NotCompromised deriving (Show, Eq)

colorscheme :: String
colorscheme = "dark28"

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
toCompromisionType ct = case ct of
  AAG.Automatic -> Automatic
  AAG.User -> User
  AAG.NotCompromised -> NotCompromised

toNode :: AAG.Node -> Node
toNode n = Node (AAG.name n) (toProtectedBy (AAG.protectedBy n)) (toCompromisionType (AAG.compromisionType n))

toGraph :: AAG.Graph -> Graph
toGraph = map toNode

printAccess :: String -> Access -> String
printAccess nname (Access ns color) =
  printf
    "\t{\"%s\"} -> \"%s\" [color = \"%s\";];"
    (intercalate "\" \"" ns)
    nname
    (show color)

printAccesses :: Node -> String
printAccesses (Node nname accesses _) = intercalate "\n" (map (printAccess nname) accesses)

printCompromisionType :: CompromisionType -> String
printCompromisionType Automatic = "[style = \"dashed,bold\"; color = red;]"
printCompromisionType User = "[style = \"bold\"; color = red;]"
printCompromisionType NotCompromised = "[style = \"solid\";]"

printNodeHeader :: Node -> String
printNodeHeader (Node nname _ compromisionType) = printf "\"%s\" %s;" nname (printCompromisionType compromisionType)

printNode :: Node -> String
printNode node =
  printf
    ( "\t# %s\n"
        ++ "\t%s"
        ++ ( if null (protectedBy node)
               then ""
               else "\n\n"
           )
        ++ printAccesses node
    )
    (name node)
    (printNodeHeader node)

printNodes :: [Node] -> String
printNodes nodes = intercalate "\n\n" (map printNode nodes)

printGraph :: Graph -> String
printGraph g =
  "\t// Main Graph\n"
    ++ "\tedge [colorscheme = \""
    ++ colorscheme
    ++ "\";];\n\n"
    ++ printNodes g

getAllColors :: Graph -> Set.Set Int
getAllColors graph = Set.fromList $ concatMap (map color . protectedBy) graph

printColor :: Int -> String
printColor c = "\t\t\"" ++ show c ++ "\" [color = \"" ++ show c ++ "\";];"

printColors :: Set.Set Int -> String
printColors cs = intercalate "\n" (map printColor (sort $ Set.toList cs))

printLegend :: Graph -> String
printLegend graph =
  "\t// Legend\n"
    ++ "\tsubgraph {\n"
    ++ "\t\tnode [colorscheme = \""
    ++ colorscheme
    ++ "\"; style = filled;];\n"
    ++ printColors (getAllColors graph)
    ++ "\n"
    ++ "\t}"

startScreen :: String
startScreen =
  "digraph {\n"
    ++ "\t\"DILMA?\" [shape = doubleoctagon;style = \"bold,filled\";fillcolor = orange;];\n"
    ++ "}"

printDotContent :: AAG.Graph -> String
printDotContent g =
  "digraph {\n"
    ++ printLegend graph
    ++ "\n\n"
    ++ printGraph graph
    ++ "\n}"
  where
    graph = toGraph g

saveToFile :: FilePath -> AAG.Graph -> IO ()
saveToFile path graph
  | null graph = writeFile path startScreen
  | otherwise = writeFile path (printDotContent graph)
