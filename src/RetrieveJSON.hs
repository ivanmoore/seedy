{-# LANGUAGE OverloadedStrings #-}
module RetrieveJSON ( module RetrieveJSON ) where

-- NB: For performance reasons we should make changes to use
--       the Network.Wreq.Session library after this works.
import Network.Wreq
import Control.Lens
import Data.ByteString.Internal as Internal
import Data.ByteString.Lazy as Lazy
import Data.ByteString.Lazy.Char8 as LazyChar8
import Data.Maybe
import Data.Text as Text
import Control.Monad

linkHeader :: Response Lazy.ByteString -> Internal.ByteString
linkHeader = fromJust . (lookup "Link") . (^. responseHeaders)

urlResponseBody :: String -> IO Lazy.ByteString
urlResponseBody url = do
    r <- get url
    rbody <- pure $ r ^. responseBody
    return rbody

convertResponseBodyToText :: Lazy.ByteString -> Text
convertResponseBodyToText = (Text.pack) . (LazyChar8.unpack)

countCommits :: Text -> Int
countCommits = Text.count (Text.pack "parents")

countCommitsOnPage :: String -> IO Int
countCommitsOnPage = (liftM (countCommits . convertResponseBodyToText)) . urlResponseBody

countRepoCommits :: String -> IO Int
countRepoCommits url = liftM2 (+) (countCommitsOnPage url) (countCommitsOnPage page2url)
    where
        page2url = url ++ "?page=2"

getFirstCommitSHA :: String -> IO String
getFirstCommitSHA url = return "5e177263157957259217347b7191e6b4895262a1"