module CommandHandler (
    commandHandler
) where
    
import Data.List (isPrefixOf)
import Data.List.Split (splitOn)
import Graphviz (printGraph)
import qualified AccountAccessGraph as AAG
import System.IO

exampleGraph :: AAG.Graph
exampleGraph = AAG.addProtectedBy "OTPApp Recovery" ["USB Stick"] $
    AAG.addProtectedBy "OTPApp" ["OTPApp_Recovery"] $
    AAG.addProtectedBy "OTPApp" ["Phone", "Finger"] $
    AAG.addProtectedBy "OTPApp" ["pw_OTPApp", "Phone"] $
    AAG.addProtectedBy "otp_Posteo" ["OTPApp"] $
    AAG.addProtectedBy "otp_Posteo" ["YubiKey"] $
    AAG.addProtectedBy "Bitwarden" ["Finger", "Phone"] $
    AAG.addProtectedBy "Bitwarden" ["pw_Bitwarden"] $
    AAG.addProtectedBy "pw_Posteo" ["Bitwarden"] $
    AAG.addProtectedBy "Posteo" ["pw_Posteo", "otp_Posteo"] $
    AAG.addNode "Phone" $
    AAG.addNode "YubiKey" $
    AAG.addNode "Finger" $ 
    AAG.addNode "Posteo" $
    AAG.addNode "Bitwarden" $
    AAG.addNode "OTPApp" $
    AAG.addNode "OTPApp_Recovery" $
    AAG.addNode "otp_Posteo" $
    AAG.addNode "pw_Posteo" $
    AAG.addNode "pw_OTPApp" $
    AAG.addNode "pw_Bitwarden" []
            
extractCommandParameters :: String -> Int -> [String]
extractCommandParameters cmd l = drop l (splitOn " " cmd)

invoke :: String -> AAG.Graph -> (String, AAG.Graph)
invoke cmd graph
    | "add node " `isPrefixOf` cmd = do
        let name = head $ extractCommandParameters cmd 2
        ("Added node " ++ name, AAG.addNode name graph)
    | "add access " `isPrefixOf` cmd = do
        let nodes = extractCommandParameters cmd 2 
        ("Added access " ++ show nodes, AAG.addProtectedBy (head nodes) (tail nodes) graph)
    | "example" `isPrefixOf` cmd = do
        ("Generated example graph", exampleGraph)
    | "clear" `isPrefixOf` cmd = do
        ("Cleared graph", [])
    | "help" `isPrefixOf` cmd = do
        ("available commands:\n" ++
            "  add node <node name>\n" ++
            "    - adds a new node to the graph\n" ++
            "  add access <node name> <protector1> <protector2> ...\n" ++
            "    - adds a list of nodes (protectors) to a given node as access\n" ++
            "  example\n" ++
            "    - generates an example graph\n" ++
            "  clear\n" ++
            "   - reset the graph", graph)
    | otherwise = ("Unknown command", graph)


commandHandler :: AAG.Graph -> IO ()
commandHandler g = do
    putStr "> "
    hFlush stdout
    name <- getLine
    let (message, graph) = invoke name g
    writeFile "graph.dot" (printGraph graph)
    putStrLn message
    commandHandler graph



    
