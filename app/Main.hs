{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DuplicateRecordFields #-}

module Main where

import Data.List
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
        let users         = nub $ map user (statuses tl)
        let toFollowNames = filter (not . following) users
        let urls          = map (\u -> "https://twitter.com/" ++ screen_name u) toFollowNames
        writeFile "app/files/followUrls.txt" $ unlines urls
