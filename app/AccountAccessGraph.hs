module AccountAccessGraph (
    Node(..),
    Graph,
    addNode,
    addProtectedBy,
    example,
    selectNode,
    unselectNode,
    isNodeAccessible
) where

import Data.List (find)
import Data.Maybe (fromJust)
import qualified Data.Set as Set

data Node = Node {
    name :: String,
    protectedBy :: [[String]],
    selected :: Bool
} deriving (Show, Eq)

type Graph = [Node]

nodeHasName ::Node -> String -> Bool
nodeHasName (Node nname _ _) s = s == nname

addNode :: String -> Graph -> Graph
addNode nname g = g ++ [Node nname [] False]

getNode :: String -> Graph -> Maybe Node
getNode nname = find (`nodeHasName` nname)

getNodes :: [String] -> Graph -> [Node]
getNodes nnames g = map (\(Just x) -> x) $ filter (/= Nothing)  $ map (`getNode` g) nnames

addProtectedBy :: String -> [String] -> Graph -> Graph 
addProtectedBy nname nnames g = map (\x -> if x `nodeHasName` nname then updatedNode else x) g
    where
        accesses = getNodes nnames g
        existingNode = fromJust $ getNode nname g
        updatedNode = Node nname (protectedBy existingNode ++ [map name accesses]) (selected existingNode)

setSelected :: Bool -> String -> Graph -> Graph
setSelected b nname = map (\x -> if x `nodeHasName` nname then Node (name x) (protectedBy x) b else x)

selectNode :: String -> Graph -> Graph
selectNode = setSelected True

unselectNode :: String -> Graph -> Graph
unselectNode = setSelected False

isSubsetOf :: [String] -> [String] -> Bool
x `isSubsetOf` y = Set.fromList x `Set.isSubsetOf` Set.fromList y  

isNodeAccessible :: String -> Graph -> Bool
isNodeAccessible nname g = any (`isSubsetOf` selectedNodes) accesses
    where 
        accesses = protectedBy (fromJust (getNode nname g))
        selectedNodes = map name (filter selected g)

-- showCompromised :: Graph -> Graph
-- showCompromised g = map 
--     where 
--         selectedNodes = map name (filter selected g)

example :: Graph
example = addProtectedBy "OTPApp Recovery" ["USB Stick"] $
    addProtectedBy "OTPApp" ["OTPApp_Recovery"] $
    addProtectedBy "OTPApp" ["Phone", "Finger"] $
    addProtectedBy "OTPApp" ["pw_OTPApp", "Phone"] $
    addProtectedBy "otp_Posteo" ["OTPApp"] $
    addProtectedBy "otp_Posteo" ["YubiKey"] $
    addProtectedBy "Bitwarden" ["Finger", "Phone"] $
    addProtectedBy "Bitwarden" ["pw_Bitwarden"] $
    addProtectedBy "pw_Posteo" ["Bitwarden"] $
    addProtectedBy "Posteo" ["pw_Posteo", "otp_Posteo"] $
    addNode "Phone" $
    addNode "YubiKey" $
    addNode "Finger" $ 
    addNode "Posteo" $
    addNode "Bitwarden" $
    addNode "OTPApp" $
    addNode "OTPApp_Recovery" $
    addNode "otp_Posteo" $
    addNode "pw_Posteo" $
    addNode "pw_OTPApp" $
    addNode "pw_Bitwarden" []
