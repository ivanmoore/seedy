{-# LANGUAGE OverloadedStrings #-}
module RetrieveJSON where

-- NB: For performance reasons we should make changes to use
--       the Network.Wreq.Session library after this works.
import Network.Wreq
import Control.Lens
import Data.ByteString.Lazy

urlJSON :: String -> IO ByteString 
urlJSON url = do
  r <- get url
  rbody <- pure $ r ^. responseBody
  return rbody
