module Spec where

import Lib

tests :: IO ()
tests = putStrLn (show canParseCommits)

canParseCommits :: Bool
canParseCommits = 59 == (length . parseCommits) "https://api.github.com/repos/ivanmoore/Eatcheap/commits"
