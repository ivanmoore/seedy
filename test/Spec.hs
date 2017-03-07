import RetrieveJSON
import Control.Monad

main :: IO ()
main = do
    putStrLn ""
    t1 <- testResult "Can count commits" canCountCommits
    t2 <- testResult "Can parse commits" canParseCommits
    putStrLn (t1 ++ t2)

testResult :: String -> IO Bool -> IO String
testResult testName test = liftM (showTestResult testName) test

showTestResult :: String -> Bool -> String
showTestResult testName pass = testName ++ ": " ++ result ++ "\n"
    where result = if pass then "Passed" else "Failed"

canCountCommits :: IO Bool
canCountCommits = liftM (== 59) (countRepoCommits "https://api.github.com/repos/ivanmoore/Eatcheap/commits")

canParseCommits :: IO Bool
canParseCommits = liftM (== "5e177263157957259217347b7191e6b4895262a1") (getFirstCommitSHA "resources/eatcheap.json")