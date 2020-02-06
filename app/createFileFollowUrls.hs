{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DuplicateRecordFields #-}

module Main (main) where

import qualified Data.Set as Set
import Twitter.Search
import Twitter.Data.Tweet
import Twitter.Data.User

main :: IO ()
main = do
    let searchQ     = "#swallows"
    let searchCount = 100

    res <- search $ makeSearchRequest searchQ "mixed" searchCount
    case res of
      Left  err -> error err
      Right tl  -> do
        let users         = Set.toList . Set.fromList $ map user (statuses tl)
        let toFollowNames = filter (not . following) users
        let urls          = map (\u -> "https://twitter.com/" ++ screen_name u) toFollowNames
        writeFile "app/files/followUrls.txt" $ unlines urls
