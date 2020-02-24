{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DuplicateRecordFields #-}

module Main (main) where

import Control.Monad
import Control.Concurrent
import qualified Data.Set as Set
import Twitter.Search
import Twitter.Friendships (follow)
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
        let users       = Set.toList . Set.fromList $ map user (statuses tl)
        let followUsers = filter (not . following) users
        let followNames = map (screen_name) followUsers
        forM_ followNames $ \followName -> do
          threadDelay (5 * 60 * 1000 * 1000)
          res <- follow followName
          case res of
            Left  err -> putStrLn $ "BAD:" ++ err
            Right u   -> putStrLn $ "SUCCESS:" ++ (screen_name u)
