module Mermaid (
    printMermaidGraph
) where

import AccountAccessGraph (Node(..), Graph)

printMermaidProtectedBy :: String -> [Node] -> String
printMermaidProtectedBy nname = concatMap (\ (Node xname _) -> xname ++ " --> " ++ nname ++ "\n")

printMermaidNode :: Node -> String
printMermaidNode (Node nname xs) = 
    "%% " ++ nname ++ "\n" ++ 
    nname ++ "\n\n" ++ 
    concatMap (\ x -> printMermaidProtectedBy nname x ++ "\n") xs

printMermaidGraph :: Graph -> String
printMermaidGraph g = 
    "flowchart TD\n\n" ++ 
    concatMap printMermaidNode g