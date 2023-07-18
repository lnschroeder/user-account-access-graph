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

extractArgs :: String -> Int -> [String]
extractArgs cmd l = drop l (words cmd)

getResettedGraph :: String -> AAG.Graph
getResettedGraph filename = AAG.loadFromFile ("persistence/" ++ filename ++ ".aag")

crackLevel2Quest :: [String] -> String -> String -> AAG.Graph -> (String, AAG.Graph)
crackLevel2Quest solution n filename graph 
  | n == "Eistruhe" && null solution = ("--- Quest Eistruhe ---\nUm an die ... TODO\nCrack it with 'crack <node_name> <quest solution>'", graph)
  | n == "Eistruhe" && unwords solution == "hmmmm lecker lecker" = ("Congrats! You solved the quest.", AAG.setIsCompromised AAG.User n graph)
  | n == "Augenarztpraxis_Dr.Keuch" && null solution = ("--- Quest Augenarztpraxis_Dr.Keuch ---\nTODO\nUse the following format: Framstag 18:42-24:30\nCrack it with 'crack <node_name> <quest solution>'", graph)
  | n == "Augenarztpraxis_Dr.Keuch" && unwords solution == "Freitag 12:00-13:00" = ("Congrats! You solved the quest.", AAG.setIsCompromised AAG.User n graph)
  | n == "Meine_lieblings_Frage" && null solution = ("--- Quest Meine_lieblings_Frage ---\nTODO\nCrack it with 'crack <node_name> <quest solution>'", graph)
  | n == "Meine_lieblings_Frage" && unwords solution == "Wie viel Uhr ist es?" = ("Congrats! You solved the quest.", AAG.setIsCompromised AAG.User n graph)
  | n == "Name_meines_ersten_Haustiers" && null solution = ("--- Quest Name_meines_ersten_Haustiers ---\nTODO\nCrack it with 'crack <node_name> <quest solution>'", graph)
  | n == "Name_meines_ersten_Haustiers" && (unwords solution == "Fisch" || unwords solution == "Wimbo") = ("Congrats! You solved the quest.", AAG.setIsCompromised AAG.User n graph)
  | otherwise = ("Incorrect. Try again", graph)

crackLevel2 :: String -> String -> AAG.Graph -> (String, AAG.Graph)
crackLevel2 n filename graph
  | n == "Eistruhe" = ("You have already unlocked this Quest", graph)
  | n == "pw_Farmerama" && solved_pw_Farmerama = ("You made it! Here is the password: schuhean", AAG.setIsCompromised AAG.Solved n graph)
  | n == "pw_Farmerama" = ("Well done, unlock " ++ show pw_Farmerama_dependencies ++ " first to continue here!", AAG.setIsCompromised AAG.Pending n graph)
  | n == "Farmerama" = ("Exceptional work, we are proud of you! Here is the username: klaus.schuhe.an", AAG.setIsCompromised AAG.Solved n graph)
  | n == "Venenscanner" = ("Look, a your new Quest: Eistruhe", AAG.setIsCompromised AAG.OpenQuest "Eistruhe" defaultSuccessGraph)
  | n == "pw_Partyheld_Mail_Post-it" = ("Look, a your new Quest: Augenarztpraxis_Dr.Keuch", AAG.setIsCompromised AAG.OpenQuest "Augenarztpraxis_Dr.Keuch" defaultSuccessGraph)
  | n == "Protonmail" = ("Look, a your new Quest: Meine_lieblings_Frage", AAG.setIsCompromised AAG.OpenQuest "Meine_lieblings_Frage" defaultSuccessGraph)
  | n == "Finger" = ("Look, a your new Quest: Name_meines_ersten_Haustiers", AAG.setIsCompromised AAG.OpenQuest "Name_meines_ersten_Haustiers" defaultSuccessGraph)
  | otherwise = ("Congrats!", defaultSuccessGraph)
    where 
      defaultSuccessGraph = AAG.setIsCompromised AAG.User n graph
      compromisableNodes = AAG.getAllCompromisedNodeNames graph
      pw_Farmerama_dependencies = ["otp_Farmerama", "sicherheitscode_Farmerama", "Handvenen"]
      solved_pw_Farmerama = all (`elem` compromisableNodes) pw_Farmerama_dependencies

crack :: [String] -> String -> AAG.Graph -> (String, AAG.Graph)
crack args filename graph
  | null args = ("Use 'crack <node_name> [quest solution]'", graph)
  | not nodeExists = ("Typo?", graph)
  | not syntaxOk && not isQuest = ("You can only crack one node at a time. Use 'crack <node_name> [quest solution]'", graph)
  | filename == "level2" && AAG.isOpenQuest name graph = crackLevel2Quest (tail args) name filename graph
  | not nodeCanBeCompromised = ("You Failed. Start all over again.", getResettedGraph filename)
  | filename == "level2" = crackLevel2 name filename graph
  | otherwise = ("Level not implemented yet:" ++ filename, AAG.setIsCompromised AAG.User name graph)
  where
    syntaxOk = length args == 1
    name = head args
    nodeExists = AAG.hasNode name graph
    isQuest = AAG.isOpenQuest name graph
    nodeCanBeCompromised = AAG.canBeCompromised name graph

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
      ++ "   - reset the graph"
      ++ "  crack <node name>\n"
      ++ "   - to gain access to a node. But be careful. If you try to unlock a node,"
      ++ "you cannot unlock yet, you will have to start all over agian!",
    graph
  )

unkownCommand :: String -> AAG.Graph -> (String, AAG.Graph)
unkownCommand cmd graph =
  ( warn "Unknown command. Use 'help' for a list of commands",
    graph
  )

invoke :: String -> String -> AAG.Graph -> (String, AAG.Graph)
invoke cmd filename
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
  | "crack" == cmd || "crack " `isPrefixOf` cmd = do
      let args = extractArgs cmd 1
      crack args filename
  | otherwise =
      unkownCommand cmd

commandHandler :: String -> AAG.Graph -> IO ()
commandHandler filename g = do
  let aagFile = ".current.aag"
  let dotFile = "gui.dot"

  AAG.saveToFile aagFile g
  Representation.saveToFile dotFile g

  putStr (query "> ")
  hFlush stdout
  cmd <- getLine
  let (message, graph) = invoke cmd filename g
  putStrLn message
  commandHandler filename graph

queryGraphName :: IO ()
queryGraphName = do
  fileExists <- doesFileExist ".current.aag"
  if fileExists
    then currentGraph `seq` commandHandler "level2" currentGraph
    else level2Graph `seq` commandHandler "level2" level2Graph
  where
    currentGraph = AAG.loadFromFile ".current.aag"
    level2Graph = getResettedGraph "level2"