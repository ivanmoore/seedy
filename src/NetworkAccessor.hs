{-# LANGUAGE OverloadedStrings #-}
module NetworkAccessor where

import Network.HTTP.Client
import Network.HTTP.Simple
import Network.HTTP.Types.Header
import Data.ByteString.Lazy.Char8 as C8

getResponseJSON :: String -> IO String
getResponseJSON url = do
  req  <- parseRequest url
  resp <- httpLBS (req {requestHeaders = [(hUserAgent, "Haskell")]})
  json <- pure $ C8.unpack $ responseBody resp
  return json
  




