module AccountAccessGraph (
    Node(..),
    Graph
) where

data Node = Node {
    name :: String,
    protectedBy :: [[Node]]
} deriving (Show, Eq)

type Graph = [Node]
