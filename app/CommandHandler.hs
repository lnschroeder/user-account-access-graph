module CommandHandler
  ( queryTutorial,
  )
where

import qualified AccountAccessGraph as AAG
import Data.List (isPrefixOf, isSuffixOf)
import Data.List.Split (splitOn)
import Data.Maybe (isNothing)
import qualified Graphviz as Representation
import Levels
import System.Directory (doesFileExist)
import System.IO (hFlush, stdout)
import Utils (info, query, warn)

extractArgs :: String -> Int -> [String]
extractArgs cmd l = drop l (words cmd)

getResettedGraph :: String -> AAG.Graph
getResettedGraph "tutorial" = Levels.tutorial1
getResettedGraph "level2" = Levels.level2

crackLevel2Quest :: [String] -> String -> String -> AAG.Graph -> (String, AAG.Graph)
crackLevel2Quest solution n filename graph
  | n == "Eistruhe" && null solution = ("--- Quest: Eistruhe ---\nYou need the Venenscan of Klaus. But a Venenscanner isn't cheap. However you'll probably know that it is possible to build your own. You already have the equipment. Now you'll just have to become friends with Klaus, go eat an icecream with him and ask him: who has prooved and certified the Parkhaus Elcknerplatz am Bahnhof Köpenick APCOA?. Then he'll put his hands on the Eistruhe, your self-built Venenscanner will take a picture of his veins and voila (it'll all make sense later)!\n\nWrite his answer on a piece of cardboard, take a picture of yourself eating an icecream and holding the cardboard with the solution into the camera and send the photo to me (Vihyr) - I will send you the password to continuie \n\nCrack it with 'crack <node_name> <quest solution>'", graph)
  | n == "Eistruhe" && unwords solution == "hmmmm lecker lecker" = ("Congrats! You solved the quest.", AAG.setIsCompromised AAG.User n graph)
  | n == "Augenarztpraxis_Dr.Keuch" && null solution = ("--- Quest: Augenarztpraxis_Dr.Keuch ---\nHow do you get an Irisscan? Well, we know Klaus' Augenarztpraxis. Find out when the Augenarztpraxis is supposed to be open according to the Littfasssäule next to the Augenarztpraxis. You will find a discrepancy from that information to the information you can find on their website. One hour is different, which one is it?\n\nWhy is this necessary inforamtion? Good question! You'll pretend to be him to ask the Augenarztpraxis for Klaus' Irisscan. To don't be sus, you'll write an angry mail, saying, you were there last week in that exact hour, to pick up the irisscans, but nobody opened you, because they were closed.\n\nUse the following format as your solution: Framstag 18:42-24:30\nCrack it with 'crack <node_name> <quest solution>'", graph)
  | n == "Augenarztpraxis_Dr.Keuch" && unwords solution == "Freitag 12:00-13:00" = ("Congrats! You solved the quest.", AAG.setIsCompromised AAG.User n graph)
  | n == "Meine_lieblings_Frage" && null solution = ("--- Quest Meine_lieblings_Frage ---\nWhat is Meine_lieblings_Frage? If you have no idea, look around in the Graph. You may find more information about Klaus, which will help you to find the answer\n\nCrack it with 'crack <node_name> <quest solution>'", graph)
  | n == "Meine_lieblings_Frage" && unwords solution == "Wie viel Uhr ist es?" = ("Congrats! You solved the quest.", AAG.setIsCompromised AAG.User n graph)
  | n == "Name_meines_ersten_Haustiers" && null solution = ("--- Quest Name_meines_ersten_Haustiers ---\nWhat is Meine_lieblings_Frage? If you have no idea, look around in the Graph. You may find more information about Klaus, which will help you to find the answer\n\nCrack it with 'crack <node_name> <quest solution>'", graph)
  | n == "Name_meines_ersten_Haustiers" && (unwords solution == "Fisch" || unwords solution == "Wimbo") = ("Congrats! You solved the quest.", AAG.setIsCompromised AAG.User n graph)
  | otherwise = ("Incorrect. Try again", graph)

crackLevel2 :: String -> String -> AAG.Graph -> (String, AAG.Graph)
crackLevel2 n filename graph
  | n == "Eistruhe" = ("You have already unlocked this Quest", graph)
  | n == "pw_Farmerama" && solved_pw_Farmerama = ("You made it! Here is the password: schuhean", AAG.setIsCompromised AAG.Solved n graph)
  | n == "pw_Farmerama" = ("Well done, unlock " ++ show pw_Farmerama_dependencies ++ " first to continue here!", AAG.setIsCompromised AAG.Pending n graph)
  | n == "Farmerama" = ("Exceptional work, we are proud of you! Here is the username: klaus.schuhe.an", AAG.setIsCompromised AAG.Solved n graph)
  | n == "Venenscanner" = ("Look, a your new Quest: Eistruhe", AAG.setIsCompromised AAG.OpenQuest "Eistruhe" defaultSuccessGraph)
  | n == "Glass" = ("Oh, no, you broke the Glas :o", defaultSuccessGraph)
  | n == "Schlüsselbund" = ("Damn, in his Hosentasche was his Schlüsselbund - what a successful pee on the toilet", defaultSuccessGraph)
  | n == "Wohnung" = ("His flat looks like real farmerama fan boy flat. It's in Köpenick btw. because he was in Berlin for a Klassenfahrt in his childhood and enjoyed his time there so much, that he moved there now.", defaultSuccessGraph)
  | n == "Finger" = ("Lucky you, Klaus Sommerfeldt's fingerprint is still visible on the Glas", defaultSuccessGraph)
  | n == "Safe" = ("You are a genius! Well done!", defaultSuccessGraph)
  | n == "pw_Partyheld_Mail_Post-it" = ("Look, a your new Quest: Augenarztpraxis_Dr.Keuch", AAG.setIsCompromised AAG.OpenQuest "Augenarztpraxis_Dr.Keuch" defaultSuccessGraph)
  | n == "Protonmail" = ("Look, a your new Quest: Meine_lieblings_Frage", AAG.setIsCompromised AAG.OpenQuest "Meine_lieblings_Frage" defaultSuccessGraph)
  | n == "Finger" = ("Look, a your new Quest: Name_meines_ersten_Haustiers", AAG.setIsCompromised AAG.OpenQuest "Name_meines_ersten_Haustiers" defaultSuccessGraph)
  | otherwise = ("Congrats!", defaultSuccessGraph)
  where
    defaultSuccessGraph = AAG.setIsCompromised AAG.User n graph
    compromisableNodes = AAG.getAllCompromisedNodeNames graph
    pw_Farmerama_dependencies = ["otp_Farmerama", "sicherheitscode_Farmerama", "Handvenen"]
    solved_pw_Farmerama = all (`elem` compromisableNodes) pw_Farmerama_dependencies

crackTutorialQuest :: [String] -> String -> String -> AAG.Graph -> (String, AAG.Graph)
crackTutorialQuest solution n filename graph
  | n == "pw_Netflix" && null solution = ("--- Quest: pw_Netflix ---\nThis is an unsolved Quest. Pssst: the password is 12345\nQuests can be cracked with 'crack pw_Netflix <solution>'", graph)
  | n == "pw_Netflix" && unwords solution == "12345" = ("Congrats! You solved the quest. Now you can access Netflix", AAG.setIsCompromised AAG.User n graph)
  | otherwise = ("Incorrect. Try again", graph)

crackTutorial :: String -> String -> AAG.Graph -> (String, AAG.Graph)
crackTutorial n filename graph
  | n == "Mail" = ("Congrats! You have cracked the Mail Account\nNow get access to Netflix.\nNetflix is protected with two factors. However, the otp is already cracked and the pw_Netflix can be guessed.\nBoth arrows to Netflix have the same color i.e. they both need to be cracked first to get access to Netflix.\n\nJust give it a try!", Levels.tutorial2)
  | n == "Netflix" = ("Congrats! You have cracked the Netflix Account\nNow you need to get access to Amazon. \nUsually you wouldn't be able to get access to it because you don't have the pw_Amazon.\nBut do you notice the arrow color?\nThere is another way to get to the Amazon account using the 'I forgot my password' option. Fot this the cracked Mail account is enough to gain access to Amazon without the pw_Amazon", Levels.tutorial3)
  | n == "Amazon" = ("Congrats! You have cracked the Amazon Account. Now you are on your own try to get access to Hetzner", Levels.tutorial4)
  | n == "Hetzner" = ("Congrats! You have finished the Tutorial. Now you should be prepared to start the real challenge. CTRL+C this and start the Application again and select (s)!", graph)
  | otherwise = ("Congrats!", defaultSuccessGraph)
  where
    defaultSuccessGraph = AAG.setIsCompromised AAG.User n graph

crack :: [String] -> String -> AAG.Graph -> (String, AAG.Graph)
crack args filename graph
  | null args = ("Use 'crack <node_name> [quest solution]'", graph)
  | not nodeExists = ("Typo?", graph)
  | not syntaxOk && not isQuest = ("You can only crack one node at a time. Use 'crack <node_name> [quest solution]'", graph)
  | filename == "level2" && AAG.isOpenQuest name graph = crackLevel2Quest (tail args) name filename graph
  | filename == "tutorial" && AAG.isOpenQuest name graph = crackTutorialQuest (tail args) name filename graph
  | filename == "tutorial" && not nodeCanBeCompromised = ("You didn't had the ability to crack " ++ name ++ " yet. You will have to start from the beginnig. So better be careful next time :|", getResettedGraph filename)
  | not nodeCanBeCompromised = ("You Failed. Start all over again.", getResettedGraph filename)
  | filename == "level2" = crackLevel2 name filename graph
  | filename == "tutorial" = crackTutorial name filename graph
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
  | "clear" == cmd = do
      clearGraph
  | "crack" == cmd || "crack " `isPrefixOf` cmd = do
      let args = extractArgs cmd 1
      crack args filename
  | otherwise =
      unkownCommand cmd

commandHandler :: String -> AAG.Graph -> IO ()
commandHandler filename g = do
  let aagFile = ".current.aag"
  let dotFile = "gui.dot"

  ( if filename /= "tutorial"
      then AAG.saveToFile aagFile g
      else return ()
    )
  Representation.saveToFile dotFile g (filename /= "tutorial")

  putStr (query "> ")
  hFlush stdout
  cmd <- getLine
  let (message, graph) = invoke cmd filename g
  putStrLn (info message)
  commandHandler filename graph

queryOverwriteOrLoad :: String -> IO ()
queryOverwriteOrLoad filename = do
  arg <- getLine
  case arg of
    "o" -> commandHandler filename []
    "l" -> g `seq` commandHandler filename g -- necessary to avoid `resource busy`
    _ -> queryOverwriteOrLoad filename
  where
    aagFilename = filename ++ ".aag"
    g = AAG.loadFromFile aagFilename

startGame :: IO ()
startGame = do
  putStrLn $ info "Oh hello, to prove your analytical abilities we have set a challenge for you. Go and prove yourself - solve the challenge!"
  putStrLn $ info "Klaus Sommerfeldt had played numerous browser games during his time in school in Lower Saxony. His Farmarama account holds incredible value for us, due to his exceptional harvesting capabilities. However, he is well aware of the importance of his account, why he has taken strong measures to protect it from being compromised. Your task is to find a way to gain access to that account."
  putStrLn $ info "Our team has meticulously modeled his Account Access Graph over the past years, which should assist you in your endeavor. "
  putStrLn $ info "Since you were peeing next to him lateley in the BHT, you were able to grab into his Hosentasche. That is your starting point. The graph should give you enough information on how to get access to his Farmerama account - again this one holds very important information about harvesting techniques."
  putStrLn $ info "But be careful! A wrong step will throw you back to the beginning (quests excluded)."
  putStrLn $ info "Your game state will be persisted. So don't worry to shut down the program. You can always come back."
  fileExists <- doesFileExist ".current.aag"
  if fileExists
    then currentGraph `seq` commandHandler "level2" currentGraph
    else level2Graph `seq` commandHandler "level2" level2Graph
  where
    currentGraph = AAG.loadFromFile ".current.aag"
    level2Graph = getResettedGraph "level2"

startTutorial :: IO ()
startTutorial = do
  putStrLn $ info "Open the generated gui.dot file with an .dot preview application"
  putStrLn $ info "I reccommend: https://marketplace.visualstudio.com/items?itemName=tintinweb.graphviz-interactive-preview\nIt's a bit buggy but reopening the Preview helps"
  putStrLn $ info "You should see a User Account Access Graph: https://dl.acm.org/doi/10.1145/3319535.3354193\n\n"
  putStrLn $ info "The only command you will need is 'crack'\n"
  putStrLn $ info "Once you can see the graph preview, you have to crack the Mail account."
  putStrLn $ info "The red border around pw_Mail means, that the password for the Mail account has already been cracked."
  putStrLn $ info "Go ahead and crack the Mail account"
  tutorialGraph `seq` commandHandler "tutorial" tutorialGraph
  where
    tutorialGraph = getResettedGraph "tutorial"

queryTutorial :: IO ()
queryTutorial = do
  putStr (info "Start tutorial (t) or start game (s):")
  putStr (query " ")
  hFlush stdout
  arg <- getLine
  case arg of
    "t" -> startTutorial
    "s" -> startGame
    _ -> queryTutorial
