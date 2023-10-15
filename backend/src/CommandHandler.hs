module CommandHandler
  ( queryGraphName,
  )
where

import qualified AccountAccessGraph as AAG
import Data.List (isPrefixOf, isSuffixOf)
import Data.List.Split (splitOn)
import Data.Maybe (isNothing)
import qualified Graphviz as Representation
import System.Directory (doesFileExist)
import System.IO (hFlush, stdout)
import Utils (info, query, warn)
import System.Directory.Internal.Prelude (fromMaybe)

extractArgs :: String -> Int -> [String]
extractArgs cmd l = drop l (words cmd)

addNode :: [String] -> AAG.Graph -> (String, AAG.Graph)
addNode args graph
  | not syntaxOk =
      ( warn "Add one node at a time and don't use spaces",
        graph
      )
  | nodeExists =
      ( warn "Node " ++ name ++ " already exists",
        graph
      )
  | otherwise =
      ( info "Added node " ++ name,
        AAG.addNode name graph
      )
  where
    name = head args
    syntaxOk = length args == 1
    nodeExists = AAG.hasNode name graph

removeNode :: [String] -> AAG.Graph -> (String, AAG.Graph)
removeNode args graph
  | not syntaxOk =
      ( warn "Remove one node at a time and don't use spaces",
        graph
      )
  | not nodeExists =
      ( warn "Node " ++ name ++ " not in Graph",
        graph
      )
  | otherwise =
      ( info "Removed node " ++ name,
        AAG.removeNode name graph
      )
  where
    syntaxOk = length args == 1
    name = head args
    nodeExists = AAG.hasNode name graph

addAccess :: [String] -> AAG.Graph -> (String, AAG.Graph)
addAccess args graph
  | not syntaxOk =
      ( warn "Too few arguments provided",
        graph
      )
  | not (null missingNodes) =
      ( warn "Node with name "
          ++ head missingNodes
          ++ " does not exist. Access was not added!",
        graph
      )
  | accessExists =
      ( warn "Access already exists "
          ++ show names
          ++ " for node "
          ++ name
          ++ ". Access was not added!",
        graph
      )
  | otherwise =
      ( info "Added access " ++ show names ++ " for node " ++ name,
        AAG.compromiseAllPossibleNodes $ AAG.addProtectedBy name names graph
      )
  where
    syntaxOk = length args > 1
    name = head args
    names = tail args
    missingNodes = filter (\x -> not (AAG.hasNode x graph)) (name : names)
    accessExists = AAG.hasAccess name names graph

removeAccess :: [String] -> AAG.Graph -> (String, AAG.Graph)
removeAccess args graph
  | not syntaxOk = 
      ( warn "Too few arguments provided",
        graph
      )
  | not (null missingNodes) = 
     ( warn "Node with name "
          ++ head missingNodes
          ++ " does not exist. Access was not removed!",
        graph
      )
  | not accessExists = 
     ( warn "Access does not exists "
          ++ show names
          ++ " for node "
          ++ name
          ++ ". Access was not removed!",
        graph
      )
  | otherwise =
      ( info "Removed access " ++ show names ++ " for node " ++ name,
        AAG.compromiseAllPossibleNodes $ AAG.removeProtectedBy name names graph
      )
  where
    syntaxOk = length args > 1
    name = head args
    names = tail args
    missingNodes = filter (\x -> not (AAG.hasNode x graph)) (name : names)
    accessExists = AAG.hasAccess name names graph

compromiseNodes :: [String] -> AAG.Graph -> (String, AAG.Graph)
compromiseNodes args graph
  | not syntaxOk =
      ( warn "Too few arguments provided",
        graph
      )
  | otherwise =
      ( info "Compromised node(s) " ++ show names,
        AAG.compromiseAllPossibleNodes $ AAG.compromiseNodes AAG.User names graph
      )
  where
    syntaxOk = not (null args)
    names = args

resetGraph :: AAG.Graph -> (String, AAG.Graph)
resetGraph graph =
  ( info "Graph was resetted",
    AAG.resetAllNode graph
  )

loadExampleGraph :: AAG.Graph -> (String, AAG.Graph)
loadExampleGraph graph =
  ( info "Generated example graph",
    AAG.example
  )

clearGraph :: AAG.Graph -> (String, AAG.Graph)
clearGraph graph =
  ( info "Cleared graph",
    []
  )

showHelp :: AAG.Graph -> (String, AAG.Graph)
showHelp graph =
  ( info "available commands:\n"
      ++ "  add node <node name>\n"
      ++ "    - adds a new node to the graph\n"
      ++ "  remove node <node name>\n"
      ++ "    - removes an existing node from the graph\n"
      ++ "  add access <node name> <protector1> <protector2> ...\n"
      ++ "    - adds a list of nodes (protectors) to a given node as access\n"
      ++ "  remove access <node name> <protector1> <protector2> ...\n"
      ++ "    - removes an existing access (protectors) from a given node\n"
      ++ "  compromise <node name 1> <node name 2> ...\n"
      ++ "    - compromises the given nodes\n"
      ++ "  reset\n"
      ++ "    - Resets graph i.e. show all nodes as not compromised\n"
      ++ "  example\n"
      ++ "    - generates an example graph\n"
      ++ "  clear\n"
      ++ "   - reset the graph",
    graph
  )

unkownCommand :: String -> AAG.Graph -> (String, AAG.Graph)
unkownCommand cmd graph =
  ( warn "Unknown command. Use 'help' for a list of commands",
    graph
  )

invoke :: String -> AAG.Graph -> (String, AAG.Graph)
invoke cmd
  | "add node" == cmd || "add node " `isPrefixOf` cmd = do
      let args = extractArgs cmd 2
      addNode args
  | "add access" == cmd || "add access " `isPrefixOf` cmd = do
      let args = extractArgs cmd 2
      addAccess args
  | "remove node" == cmd || "remove node " `isPrefixOf` cmd = do
      let args = extractArgs cmd 2
      removeNode args
  | "remove access" == cmd || "remove access " `isPrefixOf` cmd = do
      let args = extractArgs cmd 2
      removeAccess args
  | "compromise" == cmd || "compromise " `isPrefixOf` cmd = do
      let args = extractArgs cmd 1
      compromiseNodes args
  | "reset" == cmd = do
      resetGraph
  | "example" == cmd = do
      loadExampleGraph
  | "clear" == cmd = do
      clearGraph
  | "help" == cmd = do
      showHelp
  | otherwise =
      unkownCommand cmd

commandHandler :: String -> AAG.Graph -> IO ()
commandHandler filename g = do
  let aagFile = filename ++ ".aag"
  let dotFile = filename ++ ".dot"

  AAG.saveToFile aagFile g
  Representation.saveToFile dotFile g

  putStr (query "> ")
  hFlush stdout
  cmd <- getLine
  let (message, graph) = invoke cmd g
  putStrLn message
  commandHandler filename graph

queryOverwriteOrLoad :: String -> IO ()
queryOverwriteOrLoad filename = do
  putStrLn (warn "File already exists: " ++ aagFilename)
  putStr (info "Load (l) or overwrite (o):")
  putStr (query " ")
  hFlush stdout
  arg <- getLine
  case arg of
    "o" -> commandHandler filename []
    "l" -> g `seq` commandHandler filename g -- necessary to avoid `resource busy`
    _ -> queryOverwriteOrLoad filename
  where
    aagFilename = filename ++ ".aag"
    g = fromMaybe [] $ AAG.loadFromFile aagFilename

queryGraphName :: IO ()
queryGraphName = do
  putStr (info "Enter the name of the graph:")
  putStr (query " ")
  hFlush stdout
  filename <- getLine
  fileExists <- doesFileExist (filename ++ ".aag")
  if fileExists
    then queryOverwriteOrLoad filename
    else commandHandler filename []
