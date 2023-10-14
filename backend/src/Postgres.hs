{-# LANGUAGE OverloadedStrings #-}

module Postgres
  ( connectToDatabase,
  )
where

import qualified AccountAccessGraph as AAG 
import Data.Int ( Int64 )
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

readExample :: Connection -> String -> IO [Only String]
readExample conn graphName = query conn "SELECT graph_string FROM graph WHERE name = ?" $ Only graphName

upsertExample :: Connection -> IO Int64
upsertExample conn = execute conn sql_query $ Only (show AAG.example)
  where
    sql_query = "INSERT INTO graph (name, graph_string) VALUES ('example', ?) ON CONFLICT (name) DO UPDATE SET graph_string = EXCLUDED.graph_string"

connectToDatabase :: IO ()
connectToDatabase = do
  conn <- connect localPG
  putStrLn "Connected to database"
  upsertExample conn
  readExample conn "example" >>= print
