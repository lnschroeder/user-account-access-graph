module Utils
  ( query,
    info,
    warn,
    encrypt,
    decrypt,
  )
where

import Data.Char

query :: String -> String
query = (++) "\ESC[32m"

info :: String -> String
info = (++) "\ESC[90m"

warn :: String -> String
warn = (++) "\ESC[33m"

--- ENCRYPTION/DECRYPTION --- https://github.com/sve009/Simple-Haskell-Encryption/blob/master/enc.hs
prepareKey :: [Char] -> [Char] -> [Char]
prepareKey msg key = take (length msg) (cycle key)

convertString :: [Char] -> [Int]
convertString = map ((\x -> if x < 50 then x + 128 else x) . ord)

stringify :: [Int] -> [Char]
stringify = concatMap show

unStringify :: [Char] -> [Int]
unStringify [] = []
unStringify (x : y : z : xs) = read [x, y, z] : unStringify xs

encode :: [Char] -> [Char] -> [Char]
encode msg trueKey = stringify $ zipWith (+) (convertString msg) (convertString trueKey)

decode :: [Char] -> [Char] -> [Char]
decode msg trueKey = map (chr . \x -> if x > 127 then x - 128 else x) $ zipWith (-) (unStringify msg) (convertString trueKey)

encrypt :: [Char] -> [Char] -> [Char]
encrypt msg key = do
  let trueKey = prepareKey msg key
      cryptotext = encode msg trueKey
  cryptotext

decrypt :: [Char] -> [Char] -> [Char]
decrypt msg key = do
  let trueKey = prepareKey msg key
      normalText = decode msg trueKey
  normalText