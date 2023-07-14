module CommandHandler
  ( queryGraphName,
  )
where

import qualified AccountAccessGraph as AAG
import Data.List (isPrefixOf, isSuffixOf)
import Data.List.Split (splitOn)
import Graphviz (printGraph)
import System.Directory (doesFileExist)
import System.IO (hFlush, stdout)

extractCommandParameters :: String -> Int -> [String]
extractCommandParameters cmd l = drop l (words cmd)

invoke :: String -> AAG.Graph -> (String, AAG.Graph)
invoke cmd graph
  | "add node" == cmd || "add node " `isPrefixOf` cmd = do
      let args = extractCommandParameters cmd 2
      if length args == 1
        then
          let name = head args
           in ("\ESC[90mAdded node " ++ name, AAG.addNode name graph)
        else ("\ESC[33mAdd one node at a time and don't use spaces", graph)
  | "add access" == cmd || "add access " `isPrefixOf` cmd = do
      let args = extractCommandParameters cmd 2
      if length args > 1
        then
          let name = head args
              names = tail args
           in ( "\ESC[90mAdded access " ++ show names ++ " for node " ++ name,
                AAG.compromiseAllPossibleNodes $ AAG.addProtectedBy name names graph
              )
        else
          ( "\ESC[33mToo few arguments provided",
            graph
          )
  | "remove node" == cmd || "remove node " `isPrefixOf` cmd = do
      let args = extractCommandParameters cmd 2
      if length args == 1
        then
          let name = head args
           in if AAG.hasNode name graph
                then ("\ESC[90mRemoved node " ++ name, AAG.removeNode name graph)
                else ("\ESC[33mNode " ++ name ++ " not in Graph", graph)
        else ("\ESC[33mRemove one node at a time and don't use spaces", graph)
  | "compromise" == cmd || "compromise " `isPrefixOf` cmd = do
      let args = extractCommandParameters cmd 1
      if not (null args)
        then
          let names = args
           in ( "\ESC[90mCompromised node(s) " ++ show names,
                AAG.compromiseAllPossibleNodes $ AAG.compromiseNodes AAG.User names graph
              )
        else
          ( "\ESC[33mToo few arguments provided",
            graph
          )
  | "reset" == cmd = do
      ( "\ESC[90mGraph was resetted",
        AAG.resetAllNode graph
        )
  | "example" == cmd = do
      ( "\ESC[90mGenerated example graph",
        AAG.example
        )
  | "clear" == cmd = do
      ( "\ESC[90mCleared graph",
        []
        )
  | "help" == cmd = do
      ( "\ESC[90mavailable commands:\n"
          ++ "  add node <node name>\n"
          ++ "    - adds a new node to the graph\n"
          ++ "  add access <node name> <protector1> <protector2> ...\n"
          ++ "    - adds a list of nodes (protectors) to a given node as access\n"
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
  | otherwise =
      ( "\ESC[33mUnknown command. Use 'help' for a list of commands",
        graph
      )

commandHandler :: String -> AAG.Graph -> IO ()
commandHandler filename g = do
  let dotFile = filename ++ ".dot"
  let aagFile = filename ++ ".aag"

  AAG.saveToFile aagFile g

  if null g
    then
      writeFile
        dotFile
        ( "digraph {\n"
            ++ "\t\"DILMA?\" [shape = doubleoctagon;style = \"bold,filled\";fillcolor = orange;];\n"
            ++ "}"
        )
    else writeFile dotFile (printGraph g)
  putStr "\ESC[32m> "
  hFlush stdout
  cmd <- getLine
  let (message, graph) = invoke cmd g
  putStrLn message
  commandHandler filename graph

queryOverwriteOrLoad :: String -> IO ()
queryOverwriteOrLoad filename = do
  putStrLn ("\ESC[33mFile already exists: " ++ aagFilename)
  putStr "\ESC[90mLoad (l) or overwrite (o): \ESC[32m"
  hFlush stdout
  arg <- getLine
  case arg of
    "o" -> commandHandler filename []
    "l" -> g `seq` commandHandler filename g -- necessary to avoid `resource busy`
    _ -> queryOverwriteOrLoad filename
  where
    aagFilename = filename ++ ".aag"
    g = AAG.loadFromFile aagFilename

queryGraphName :: IO ()
queryGraphName = do
  putStr "\ESC[90mEnter the name of the graph: \ESC[32m"
  hFlush stdout
  filename <- getLine
  fileExists <- doesFileExist (filename ++ ".aag")
  if fileExists
    then queryOverwriteOrLoad filename
    else commandHandler filename []
