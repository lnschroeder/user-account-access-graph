module CommandHandler
  ( queryOutputFile,
  )
where

import qualified AccountAccessGraph as AAG
import Data.List (isPrefixOf, isSuffixOf)
import Data.List.Split (splitOn)
import Graphviz (printGraph)
import System.IO (hFlush, stdout)

extractCommandParameters :: String -> Int -> [String]
extractCommandParameters cmd l = drop l (splitOn " " cmd)

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
commandHandler path g = do
  writeFile path (printGraph g)
  putStr "\ESC[32m> "
  hFlush stdout
  name <- getLine
  let (message, graph) = invoke name g
  putStrLn message
  commandHandler path graph

queryOutputFile :: IO ()
queryOutputFile = do
  putStr "\ESC[90mEnter output file path: \ESC[32m"
  hFlush stdout
  input <- getLine
  if ".dot" `isSuffixOf` input
    then do
      putStrLn "\ESC[90mUse 'help' for a list of commands"
      commandHandler input []
    else do
      putStrLn "\ESC[33mFile path must end with .dot"
      queryOutputFile