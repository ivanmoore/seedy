module JSONHandler where

import Pelias
import SeedyPelias

latestCommitAuthor :: String -> IO String
latestCommitAuthor json = do
  (Just (SValue author)) <- pure $ seedyExtract [Index 0, Get "author", Get "login"] json
  return author
