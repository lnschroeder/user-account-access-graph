module AccountAccessGraph (
    CompromisionType(..),
    Node(..),
    Graph,
    addNode,
    addProtectedBy,
    example,
    compromiseNodes,
    resetNode,
    isCompromised,
    compromiseAllPossibleNodes
) where

import Data.List (find)
import Data.Maybe (fromJust, isNothing)
import qualified Data.Set as Set

data CompromisionType = Automatic | User | NotCompromised deriving (Show, Eq)

data Node = Node {
    name :: String,
    protectedBy :: [[String]],
    compromisionType :: CompromisionType
} deriving (Show, Eq)

type Graph = [Node]

isCompromised :: Node -> Bool
isCompromised n = NotCompromised /= compromisionType n

nodeHasName ::Node -> String -> Bool
nodeHasName (Node nname _ _) s = s == nname

addNode :: String -> Graph -> Graph
addNode nname g = g ++ [Node nname [] NotCompromised]

getNode :: String -> Graph -> Maybe Node
getNode nname = find (`nodeHasName` nname)

getNodes :: [String] -> Graph -> [Node]
getNodes nnames g = map (\(Just x) -> x) $ filter (/= Nothing)  $ map (`getNode` g) nnames

addProtectedBy :: String -> [String] -> Graph -> Graph 
addProtectedBy nname nnames g = map (\x -> if x `nodeHasName` nname then updatedNode else x) g
    where
        accesses = getNodes nnames g
        existingNode = fromJust $ getNode nname g
        updatedNode = Node nname (protectedBy existingNode ++ [map name accesses]) (compromisionType existingNode)

setIsCompromised :: CompromisionType -> String -> Graph -> Graph
setIsCompromised c nname = map (\x -> if x `nodeHasName` nname then x { compromisionType = c } else x)

compromiseNodes :: CompromisionType -> [String] -> Graph -> Graph
compromiseNodes c xs g = foldl (flip (setIsCompromised c)) g xs

resetNode :: String -> Graph -> Graph
resetNode = setIsCompromised NotCompromised

isSubsetOf :: [String] -> [String] -> Bool
x `isSubsetOf` y = Set.isSubsetOf (Set.fromList x) (Set.fromList y)  

canBeCompromised :: String -> Graph -> Bool
canBeCompromised nname g 
    | isNothing maybeNode = False
    | otherwise = any (`isSubsetOf` selectedNodes) accesses
    where 
        maybeNode = getNode nname g
        accesses = protectedBy (fromJust maybeNode)
        selectedNodes = map name (filter isCompromised g)

getCompromisableNodes :: Graph -> [String]
getCompromisableNodes g = map name (filter (\x -> not (isCompromised x) && name x `canBeCompromised` g) g)

compromiseAllPossibleNodes :: Graph -> Graph
compromiseAllPossibleNodes g 
    | null compromisableNodes = g
    | otherwise = compromiseAllPossibleNodes (compromiseNodes Automatic compromisableNodes g)
    where 
        compromisableNodes = getCompromisableNodes g

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
