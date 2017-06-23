module EndToEndTest where

import RobUnit
import JSONHandler

main :: IO ()
main = do
  putStrLn "=== END TO END ==="
  ioResult $ ioTest "can get first committer username" "robknows" (latestCommitAuthor "resources/eatcheap.json")      
  putStrLn "ALL DONE"
