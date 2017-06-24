module JSONParser where

import Pelias
import SeedyPelias

latestCommitAuthor :: String -> String
latestCommitAuthor json =
  case seedyExtract [Index 0, Get "author", Get "login"] json of
    Just (SValue author) -> author
    _                    -> error "JSONParser.latestCommitAuthor failed for input starting with \"" ++ (take 20 json) ++ "\"\n"

latestCommitSha :: String -> String
latestCommitSha json =
  case seedyExtract [Index 0, Get "sha"] json of
    Just (SValue sha) -> sha
    _                 -> error "JSONParser.latestCommitSha failed for input starting with \"" ++ (take 20 json) ++ "\"\n"

