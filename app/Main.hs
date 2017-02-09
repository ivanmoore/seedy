module Main where

import Happstack.Lite
import Data.Maybe

main :: IO ()
main = serve Nothing gitListener

gitListener :: ServerPart Response
gitListener = ok $ toResponse "hello world\n"
