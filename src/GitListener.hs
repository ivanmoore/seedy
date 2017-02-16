{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
module GitListener ( parseCommits, gitListener ) where

import Happstack.Lite
import Data.Text (Text)
import Data.Text.Lazy (unpack)

import Commit
import RetrieveJSON

gitListener :: ServerPart Response
gitListener = do
  json <- formdataField "json"
  ok $ toResponse $ "json: " ++ json ++ "\n"

formdataField :: String -> ServerPart String
formdataField fieldName = do
  msg <- lookText fieldName
  return (unpack msg)

parseCommits :: String -> [Commit]
parseCommits commitsUrl = [Commit { fake = x } | x <- [1..59]]
