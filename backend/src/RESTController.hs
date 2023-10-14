{-# LANGUAGE OverloadedStrings #-}

module RESTController
  ( controller,
  )
where

import qualified AccountAccessGraph as AAG
import Control.Monad.IO.Class (liftIO) -- Import liftIO
import Data.List.Split
import qualified Graphviz as Representation
import Network.HTTP.Types.Status (notFound404)
import System.Directory (doesFileExist)
import Web.Scotty

getGraph :: String -> AAG.Graph
getGraph aagFilename
  | aagFilename == "example.aag" = AAG.example
  | otherwise = AAG.loadFromFile aagFilename

controller :: IO ()
controller = do
  scotty 8080 $ do
    get "/graph/:name" $ do
      filename <- captureParam "name"
      let aagFilename = filename ++ ".aag"
      fileExists <- liftIO $ doesFileExist aagFilename
      if fileExists || filename == "example"
        then do
          json $ getGraph aagFilename
        else do
          status notFound404
          json ("File not found: " ++ aagFilename)
    post "/graph/:name" $ do
      filename <- captureParam "name"
      g <- jsonData :: ActionM AAG.Graph -- TODO: why ActionM?
      liftIO $ AAG.saveToFile (filename ++ ".aag") g
      liftIO $ Representation.saveToFile (filename ++ ".dot") g
      json ("nodes in graph" ++ show (length g))
    post "/node/:name" $ do
      nodename <- captureParam "name"
      g <- jsonData :: ActionM AAG.Graph
      let g' = AAG.addNode nodename g
      json g'
    post "/access/:name" $ do
      nodename <- captureParam "name"
      protectors <- queryParam "protectors"
      g <- jsonData :: ActionM AAG.Graph
      let ps = splitOn "," protectors
      let g' = AAG.addProtectedBy nodename ps g
      json g'
