import AccountAccessGraph (Node(..), Graph, addNode, addProtectedBy)
import Graphviz (printGraph)

main :: IO ()
main = do
    let 
        initialGraph = []
        updatedGraph1 = addNode Node { name = "pw_Bitwarden", protectedBy = [] } initialGraph 
        updatedGraph2 = addNode Node { name = "Phone", protectedBy = [] } updatedGraph1 
        updatedGraph3 = addNode Node { name = "Finger", protectedBy = [] } updatedGraph2 
        updatedGraph4 = addNode Node { name = "Bitwarden", protectedBy = [] } updatedGraph3 
        updatedGraph5 = addProtectedBy "Bitwarden" ["pw_Bitwarden"] updatedGraph4
        updatedGraph6 = addProtectedBy "Bitwarden" ["Finger", "Phone"] updatedGraph5
    writeFile "graph.dot" (printGraph updatedGraph6)
