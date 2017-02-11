{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
module Lib ( parseCommits, gitListener ) where

import Happstack.Lite
import Data.Maybe
import Data.Text (Text)
import Data.Text.Lazy (unpack)

import Data.Aeson
import GHC.Generics

data Commit = Commit {
      fake :: Int
    } deriving (Generic, Show)

instance FromJSON Commit where
    parseJSON (Object v) = do
        return Commit { fake = 2 }
    parseJSON _          = do
        return Commit { fake = 23 }

gitListener :: ServerPart Response
gitListener = do
  before <- formdataField "before"
  after  <- formdataField "after"
  ok $ toResponse ("after_data: " ++ after ++ "\nbefore_data: " ++ before ++ "\n")

formdataField :: String -> ServerPart String
formdataField fieldName = do
  msg <- lookText fieldName
  return (unpack msg)

parseCommits :: String -> [Commit]
parseCommits commitsUrl = [Commit { fake = x } | x <- [1..59]]