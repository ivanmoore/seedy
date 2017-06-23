module RobUnit where

import Control.Monad
import Control.Conditional
import Data.String.Utils

makeTest :: (Eq a, Show a) => String -> a -> a -> String
makeTest description actual expected =
  if expected == actual
  then ""
  else "FAIL: " ++ description ++ "\n\texp: " ++ show expected ++ "\n\tact: " ++ show actual ++ "\n"

appendFailedCount :: [String] -> [String]
appendFailedCount tests = tests ++ ["PASSED: " ++ show passed ++ " FAILED: " ++ show fails ++ "\n"]
  where
    fails  = length (filter (startswith "FAIL") tests)
    passed = length tests - fails

ioTest :: (Show a, Eq a) => String -> a -> IO a -> IO String
ioTest description expected actual =
  ifM (liftM (== expected) actual) (return "") (ioFailureMessage description (show expected) actual)
  
ioFailureMessage :: (Show a) => String -> String -> IO a -> IO String
ioFailureMessage description expected actual
  = liftM (\x -> concat ["FAIL: ", description, "\n\texp: ", expected, "\n\tact: ", show x, "\n"]) actual

ioResult :: IO String -> IO ()
ioResult test = do
  result <- test
  putStrLn result
