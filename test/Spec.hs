import Lib

main :: IO ()
main = putStrLn (" *********** Tests " ++ (if canParseCommits then "Passed" else "Failed") ++ " ***********")

canParseCommits :: Bool
canParseCommits = 59 == (length . parseCommits) "https://api.github.com/repos/ivanmoore/Eatcheap/commits"
