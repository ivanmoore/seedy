module SeedyPelias where

import Data.Char
import Data.String.Utils
import Pelias

seedyExtract :: [JSONOperation] -> String -> Maybe Value
seedyExtract = optimisedExtract seedyOptimiser

seedyOptimiser :: Optimiser
seedyOptimiser (Index i : ops) json = (reform $ (substr start end) splitJSON, ops)
  where
    splitJSON        = (split "{") json
    substr :: Int -> Int -> [a] -> [a]
    substr s e       = (drop s) . (take e)
    reform :: [String] -> String
    reform           = concat . (map (\x -> '{' : x))
    indexItemRange :: Int -> Int
    indexItemRange x = (length . takeWhile (< (1 + 2 * x))) bracketMap
    bracketMap       = scanl (+) 0 $ (map (succ . countStrBalance)) $ (drop 2) $ splitJSON
    (start, end)     = (indexItemRange i, indexItemRange (i + 1))
seedyOptimiser ops json = (json, ops)
