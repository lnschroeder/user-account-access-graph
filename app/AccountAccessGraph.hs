module AccountAccessGraph
  ( CompromisionType (..),
    Node (..),
    Graph,
    addNode,
    addProtectedBy,
    removeNode,
    example,
    compromiseNodes,
    resetAllNode,
    compromiseAllPossibleNodes,
    loadFromFile,
    saveToFile,
    hasNode,
  )
where

import Data.List (find)
import Data.Maybe (fromJust, isNothing)
import qualified Data.Set as Set
import Debug.Trace (trace)
import System.IO.Unsafe (unsafePerformIO)

data CompromisionType = Automatic | User | NotCompromised deriving (Show, Eq, Read)

data Node = Node
  { name :: String,
    protectedBy :: Set.Set (Set.Set String),
    compromisionType :: CompromisionType
  }
  deriving (Show, Eq, Read)

type Graph = [Node]

nodeHasName :: Node -> String -> Bool
nodeHasName (Node nname _ _) s = s == nname

addNode :: String -> Graph -> Graph
addNode nname g = g ++ [Node nname Set.empty NotCompromised]

getNode :: String -> Graph -> Maybe Node
getNode nname = find (`nodeHasName` nname)

getNodes :: [String] -> Graph -> [Maybe Node]
getNodes nnames g = map (`getNode` g) nnames

removeProtectionFromNode :: String -> Node -> Node
removeProtectionFromNode nname n = n {protectedBy = updatedProtectedBy}
  where
    updatedProtectedBy = Set.filter (not . null) $ Set.map (Set.delete nname) (protectedBy n)

removeNode :: String -> Graph -> Graph
removeNode nname g = map (removeProtectionFromNode nname) (filter (\x -> name x /= nname) g)

hasNode :: String -> Graph -> Bool
hasNode nname g = nname `elem` map name g

addProtectedBy :: String -> [String] -> Graph -> Graph
addProtectedBy nname nnames g
  | isNothing maybeNode =
      trace
        ( "\ESC[33mNode with name " ++ nname ++ " does not exist. Access was not added!"
        )
        g
  | any isNothing maybeNodes =
      trace
        ( "\ESC[33mNode with name " ++ head nnames ++ " does not exist. Access was not added!"
        )
        g
  | any (\x -> Set.fromList nnames == x) (protectedBy (fromJust maybeNode)) =
      trace
        ( "\ESC[33mAccess already exists "
            ++ show nnames
            ++ " for node "
            ++ nname
            ++ ". Access was not added!"
        )
        g
  | otherwise =
      map
        ( \x ->
            if x `nodeHasName` nname
              then x {protectedBy = Set.insert (Set.fromList nnames) (protectedBy x)}
              else x
        )
        g
  where
    maybeNode = getNode nname g
    maybeNodes = getNodes nnames g

getAllCompromisedNodeNames :: Graph -> [String]
getAllCompromisedNodeNames = map name . filter isCompromised

isCompromised :: Node -> Bool
isCompromised n = NotCompromised /= compromisionType n

setIsCompromised :: CompromisionType -> String -> Graph -> Graph
setIsCompromised c nname =
  map
    ( \x ->
        if x `nodeHasName` nname
          then x {compromisionType = c}
          else x
    )

compromiseNodes :: CompromisionType -> [String] -> Graph -> Graph
compromiseNodes c xs g = foldr (setIsCompromised c) g xs

resetNode :: String -> Graph -> Graph
resetNode = setIsCompromised NotCompromised

resetAllNode :: Graph -> Graph
resetAllNode g = foldr resetNode g (getAllCompromisedNodeNames g)

canBeCompromised :: String -> Graph -> Bool
canBeCompromised nname g
  | isNothing maybeNode = False
  | otherwise = any (`Set.isSubsetOf` Set.fromList selectedNodes) accesses
  where
    maybeNode = getNode nname g
    accesses = protectedBy $ fromJust maybeNode
    selectedNodes = getAllCompromisedNodeNames g

getCompromisableNodes :: Graph -> [String]
getCompromisableNodes g = [name x | x <- g, not (isCompromised x) && name x `canBeCompromised` g]

compromiseAllPossibleNodes :: Graph -> Graph
compromiseAllPossibleNodes g
  | null compromisableNodes = g
  | otherwise = compromiseAllPossibleNodes $ compromiseNodes Automatic compromisableNodes g
  where
    compromisableNodes = getCompromisableNodes g

saveToFile :: FilePath -> Graph -> IO ()
saveToFile filePath graph = writeFile filePath (show graph)

loadFromFile :: FilePath -> Graph
loadFromFile filePath = unsafePerformIO $ do
  contents <- readFile filePath
  return (read contents)

example :: Graph
example =
  addProtectedBy "pw_Netflix" ["OTPApp"] $
    addProtectedBy "Netflix" ["pw_Netflix"] $
      addProtectedBy "Netflix" ["Gmail"] $
        addProtectedBy "OTPApp_Recovery" ["USB_Stick"] $
          addProtectedBy "OTPApp" ["OTPApp_Recovery"] $
            addProtectedBy "OTPApp" ["Phone", "Finger"] $
              addProtectedBy "OTPApp" ["pw_OTPApp", "Phone"] $
                addProtectedBy "otp_Gmail" ["OTPApp"] $
                  addProtectedBy "otp_Gmail" ["YubiKey"] $
                    addProtectedBy "Bitwarden" ["Finger", "Phone"] $
                      addProtectedBy "Bitwarden" ["pw_Bitwarden"] $
                        addProtectedBy "pw_Gmail" ["Bitwarden"] $
                          addProtectedBy "Gmail" ["pw_Gmail", "otp_Gmail"] $
                            addNode "Netflix" $
                              addNode "pw_Netflix" $
                                addNode "USB_Stick" $
                                  addNode "Phone" $
                                    addNode "YubiKey" $
                                      addNode "Finger" $
                                        addNode "Gmail" $
                                          addNode "Bitwarden" $
                                            addNode "OTPApp" $
                                              addNode "OTPApp_Recovery" $
                                                addNode "otp_Gmail" $
                                                  addNode "pw_Gmail" $
                                                    addNode "pw_OTPApp" $
                                                      addNode "pw_Bitwarden" []
