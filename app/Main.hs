module Main where

import Happstack.Lite
import Data.Maybe
import Data.Text (Text)
import Data.Text.Lazy (unpack)

main :: IO ()
main = serve Nothing gitListener

gitListener :: ServerPart Response
gitListener = do
  before <- formdataField "before"
  after  <- formdataField "after"
  ok $ toResponse ("after_data: " ++ after ++ "\nbefore_data: " ++ before ++ "\n")

formdataField :: String -> ServerPart String
formdataField fieldName = do
  msg <- lookText fieldName
  return (unpack msg)
