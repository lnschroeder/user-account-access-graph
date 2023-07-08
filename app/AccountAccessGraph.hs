module AccountAccessGraph
  ( CompromisionType (..),
    Node (..),
    Graph,
    addNode,
    addProtectedBy,
    example,
    compromiseNodes,
    resetAllNode,
    compromiseAllPossibleNodes,
  )
where

import Data.List (find)
import Data.Maybe (fromJust, isNothing)
import qualified Data.Set as Set
import Debug.Trace (trace)

data CompromisionType = Automatic | User | NotCompromised deriving (Show, Eq)

data Node = Node
  { name :: String,
    protectedBy :: Set.Set (Set.Set String),
    compromisionType :: CompromisionType
  }
  deriving (Show, Eq)

type Graph = [Node]

nodeHasName :: Node -> String -> Bool
nodeHasName (Node nname _ _) s = s == nname

addNode :: String -> Graph -> Graph
addNode nname g = g ++ [Node nname Set.empty NotCompromised]

getNode :: String -> Graph -> Maybe Node
getNode nname = find (`nodeHasName` nname)

getNodes :: [String] -> Graph -> [Maybe Node]
getNodes nnames g = map (`getNode` g) nnames

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

example :: Graph
example =
  addProtectedBy "OTPApp_Recovery" ["USB_Stick"] $
    addProtectedBy "OTPApp" ["OTPApp_Recovery"] $
      addProtectedBy "OTPApp" ["Phone", "Finger"] $
        addProtectedBy "OTPApp" ["pw_OTPApp", "Phone"] $
          addProtectedBy "otp_Posteo" ["OTPApp"] $
            addProtectedBy "otp_Posteo" ["YubiKey"] $
              addProtectedBy "Bitwarden" ["Finger", "Phone"] $
                addProtectedBy "Bitwarden" ["pw_Bitwarden"] $
                  addProtectedBy "pw_Posteo" ["Bitwarden"] $
                    addProtectedBy "Posteo" ["pw_Posteo", "otp_Posteo"] $
                      addNode "USB_Stick" $
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
