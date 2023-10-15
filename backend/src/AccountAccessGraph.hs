{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}

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
    loadFromString,
    loadFromFile,
    saveToFile,
    hasNode,
    hasNodes,
    hasAccess,
    removeProtectedBy,
  )
where

import Data.Aeson (FromJSON, ToJSON)
import Data.List (find)
import Data.Maybe (fromJust, isJust, isNothing)
import qualified Data.Set as Set
import Debug.Trace (trace)
import GHC.Generics
import System.Directory (doesFileExist)
import System.IO.Unsafe (unsafePerformIO)
import Text.Read (readMaybe)

data CompromisionType = Automatic | User | NotCompromised deriving (Show, Eq, Read, Generic, ToJSON, FromJSON)

data Node = Node
  { name :: String,
    protectedBy :: Set.Set (Set.Set String),
    compromisionType :: CompromisionType
  }
  deriving (Show, Eq, Read, Generic, ToJSON, FromJSON)

type Graph = [Node]

nodeHasName :: Node -> String -> Bool
nodeHasName (Node nname _ _) s = s == nname

-- TODO rename to _addNode and add a function addNode that checks if the node already
--      exists (same for other functions) returning Either String Graph or Maybe Graph
addNode :: String -> Graph -> Graph
addNode nname g = g ++ [Node nname Set.empty NotCompromised]

getNode :: String -> Graph -> Maybe Node
getNode nname = find (`nodeHasName` nname)

getNodes :: [String] -> Graph -> [Maybe Node]
getNodes nnames g = map (`getNode` g) nnames

hasNode :: String -> Graph -> Bool
hasNode nname g = isJust (getNode nname g)

hasNodes :: [String] -> Graph -> Bool
hasNodes nnames g = all (`hasNode` g) nnames

hasAccess :: String -> [String] -> Graph -> Bool
hasAccess nname nnames g = Set.fromList nnames `Set.member` protectedBy (fromJust maybeNode)
  where
    maybeNode = getNode nname g

removeProtectionFromNode :: String -> Node -> Node
removeProtectionFromNode nname n = n {protectedBy = updatedProtectedBy}
  where
    updatedProtectedBy = Set.filter (not . null) $ Set.map (Set.delete nname) (protectedBy n)

removeNode :: String -> Graph -> Graph
removeNode nname g = map (removeProtectionFromNode nname) (filter (\x -> name x /= nname) g)

addProtectedBy :: String -> [String] -> Graph -> Graph
addProtectedBy nname nnames =
  map
    ( \x ->
        if x `nodeHasName` nname
          then x {protectedBy = Set.insert (Set.fromList nnames) (protectedBy x)}
          else x
    )

removeProtectedBy :: String -> [String] -> Graph -> Graph
removeProtectedBy nname nnames =
  map
    ( \x ->
        if x `nodeHasName` nname
          then x {protectedBy = Set.delete (Set.fromList nnames) (protectedBy x)}
          else x
    )

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

loadFromString :: String -> Maybe Graph
loadFromString = readMaybe

loadFromFile :: FilePath -> Maybe Graph
loadFromFile filePath = loadFromString fileContent
  where
    fileContent = unsafePerformIO $ do
      fileExists <- doesFileExist filePath
      if fileExists
        then readFile filePath
        else return ""

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
