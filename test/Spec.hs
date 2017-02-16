import RetrieveJSON

main :: IO ()
main = putStrLn (" *********** Tests " ++ testResult ++ " ***********")
  where
    testResult = if canParseCommits then "Passed" else "Failed"

eatcheapUrl :: String
eatcheapUrl = "https://api.github.com/repos/robknows/Eatcheap/commits"

canParseCommits :: Bool
canParseCommits = False
