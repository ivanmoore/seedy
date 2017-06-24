{-# LANGUAGE OverloadedStrings #-}
module EndToEndTest where

import Control.Monad
import Network.HTTP.Client
import Network.HTTP.Simple
import Network.HTTP.Types.Header
import Data.ByteString.Lazy.Char8 as C8

import RobUnit
import JSONParser

main :: IO ()
main = do
  Prelude.putStrLn "=== END TO END ==="
  Prelude.foldr (>>) (return ()) $ Prelude.map ioResult tests
  Prelude.putStrLn "ALL DONE"

tests :: [IO String]
tests = [

  ioTest "can get latest committer username"
    (liftM latestCommitAuthor (getResponseJSON "https://api.github.com/repos/ivanmoore/Eatcheap/commits"))
    "robknows",  
  ioTest "can get sha of latest commit"
    (liftM latestCommitSha (getResponseJSON "https://api.github.com/repos/ivanmoore/Eatcheap/commits"))
    "5e177263157957259217347b7191e6b4895262a1"

        ]

getResponseJSON :: String -> IO String
getResponseJSON url = do
  req  <- parseRequest url
  resp <- httpLBS (req {requestHeaders = [(hUserAgent, "Haskell")]})
  json <- pure $ C8.unpack $ responseBody resp
  return json
  



