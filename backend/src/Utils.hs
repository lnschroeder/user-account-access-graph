module Utils
  ( query,
    info,
    warn,
  )
where

query :: String -> String
query = (++) "\ESC[32m"

info :: String -> String
info = (++) "\ESC[90m"

warn :: String -> String
warn = (++) "\ESC[33m"
