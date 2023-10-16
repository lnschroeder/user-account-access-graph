{-# LANGUAGE OverloadedStrings #-}

module RESTController
  ( controller,
  )
where

import qualified AccountAccessGraph as AAG
import Control.Monad.IO.Class (liftIO) -- Import liftIO
import Data.Int (Int64)
import Data.List.Split (splitOn)
import Data.Maybe (fromJust, isNothing)
import qualified Data.Text.Lazy as LazyText
import Database.PostgreSQL.Simple.Types (fromOnly)
import Graphviz (printDotContent)
import qualified Graphviz as Representation
import Network.HTTP.Types (internalServerError500, methodNotAllowed405, ok200)
import Network.HTTP.Types.Status (notFound404)
import Postgres (connectToDatabase, readGraph, upsertExample, upsertGraph)
import System.Directory (doesFileExist)
import Web.Scotty

controller :: IO ()
controller = do
  conn <- Postgres.connectToDatabase
  Postgres.upsertExample conn
  putStrLn "Connected to database"
  scotty 8080 $ do
    get "/graph/:graphName" $ do
      graphName <- captureParam "graphName"
      graphs <- liftIO $ Postgres.readGraph conn graphName
      let graph = AAG.loadFromString (fromOnly $ head graphs)
      if isNothing graph
        then do
          status internalServerError500
          json ("Graph '" ++ graphName ++ "' invalid format" :: String)
        else do
          status ok200
          text $ LazyText.pack (Graphviz.printDotContent (fromJust graph :: AAG.Graph))
    put "/graph/:graphName" $ do
      graphName <- captureParam "graphName"
      if graphName == "example"
        then do
          status methodNotAllowed405
          json ("Graph 'example' is read-only" :: String)
        else do
          g <- jsonData :: ActionM AAG.Graph
          graphData <- liftIO $ Postgres.upsertGraph conn graphName g
          status ok200
          json (graphData :: Int64)
    put "/graph/:graphName/node/:nodeName" $ do
      graphName <- captureParam "graphName"
      nodeName <- captureParam "nodeName"
      if graphName == "example"
        then do
          status methodNotAllowed405
          json ("Graph 'example' is read-only" :: String)
        else do
          graphs <- liftIO $ Postgres.readGraph conn graphName
          let graph = AAG.loadFromString (fromOnly $ head graphs)
          if isNothing graph
            then do
              status internalServerError500
              json ("Graph '" ++ graphName ++ "' invalid format" :: String)
            else do
              let graph' = AAG.addNode nodeName (fromJust graph)
              graphData <- liftIO $ Postgres.upsertGraph conn graphName graph'
              status ok200
              json (graphData :: Int64) -- TODO return graph
    put "/graph/:graphName/access/:nodeName" $ do
      graphName <- captureParam "graphName"
      nodeName <- captureParam "nodeName"
      protectorList <- queryParam "protectors"
      let protectors = splitOn "," protectorList
      if graphName == "example"
        then do
          status methodNotAllowed405
          json ("Graph 'example' is read-only" :: String)
        else do
          graphs <- liftIO $ Postgres.readGraph conn graphName
          let graph = AAG.loadFromString (fromOnly $ head graphs)
          if isNothing graph
            then do
              status internalServerError500
              json ("Graph '" ++ graphName ++ "' invalid format" :: String)
            else do
              let graph' = AAG.addProtectedBy nodeName protectors (fromJust graph)
              graphData <- liftIO $ Postgres.upsertGraph conn graphName graph'
              status ok200
              json (graphData :: Int64) -- TODO return graph
