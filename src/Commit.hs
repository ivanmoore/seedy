{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
module Commit where

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
