{-# LANGUAGE OverloadedStrings #-}

module Postgres
  ( connectToDatabase,
    readGraph,
    upsertExample,
    upsertGraph,
  )
where

import qualified AccountAccessGraph as AAG
import Data.Int (Int64)
import Database.PostgreSQL.Simple

localPG :: ConnectInfo
localPG =
  defaultConnectInfo
    { connectHost = "localhost",
      connectPort = 5432,
      connectDatabase = "db",
      connectUser = "postgres",
      connectPassword = "postgres"
    }

readGraph :: Connection -> String -> IO [Only String]
readGraph conn graphName = query conn "SELECT graph_string FROM graph WHERE name = ?" (Only graphName)

upsertGraph :: Connection -> String -> AAG.Graph -> IO Int64
upsertGraph conn graphName graph = execute conn sql_query (graphName, show graph)
  where                             
    sql_query = "INSERT INTO graph (name, graph_string) VALUES (?, ?) ON CONFLICT (name) DO UPDATE SET graph_string = EXCLUDED.graph_string" 

upsertExample :: Connection -> IO Int64
upsertExample conn = upsertGraph conn "example" AAG.example

connectToDatabase :: IO Connection
connectToDatabase = do
  connect localPG
