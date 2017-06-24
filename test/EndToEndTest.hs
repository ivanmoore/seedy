{-# LANGUAGE OverloadedStrings #-}
module EndToEndTest where

import Control.Monad

import RobUnit
import JSONParser
import NetworkAccessor

main :: IO ()
main = do
  putStrLn "=== END TO END ==="
  runIOTests tests
  putStrLn "ALL DONE"

eatcheapJSON :: IO String
eatcheapJSON = getResponseJSON "https://api.github.com/repos/ivanmoore/Eatcheap/commits"

tests :: [IO String]
tests = [

  ioTest "can get latest committer username"
    (liftM latestCommitAuthor eatcheapJSON)
    "robknows",  
  ioTest "can get sha of latest commit"
    (liftM latestCommitSha eatcheapJSON)
    "5e177263157957259217347b7191e6b4895262a1"

        ]
