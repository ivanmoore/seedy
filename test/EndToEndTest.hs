{-# LANGUAGE OverloadedStrings #-}
module EndToEndTest where

import Network.HTTP.Client
import Network.HTTP.Simple
import Network.HTTP.Types.Header
import Data.ByteString.Lazy.Char8 as C8

import RobUnit
import JSONHandler

main :: IO ()
main = do
  Prelude.putStrLn "=== END TO END ==="

  req  <- parseRequest "https://api.github.com/repos/ivanmoore/Eatcheap/commits"
  resp <- httpLBS (req {requestHeaders = [(hUserAgent, "Haskell")]})
  json <- pure $ C8.unpack $ responseBody resp

  ioResult $ ioTest "can get first committer username" "robknows" (latestCommitAuthor json)

  Prelude.putStrLn "ALL DONE"
