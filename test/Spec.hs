import RetrieveJSON
import Control.Monad

main :: IO ()
main = do
    testResult <- liftM showTestResult canParseCommits
    putStrLn testResult

showTestResult :: Bool -> String
showTestResult pass = " *********** Tests " ++ result ++ " ***********"
    where result = if pass then "Passed" else "Failed"

canParseCommits :: IO Bool
canParseCommits = liftM (== 59) (countRepoCommits "https://api.github.com/repos/ivanmoore/Eatcheap/commits")
