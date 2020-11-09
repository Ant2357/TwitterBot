{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DuplicateRecordFields #-}

module Main (main) where

import System.Environment (getArgs)
import Data.Char (isDigit)
import qualified Data.Text as T
import Control.Monad
import Control.Concurrent
import qualified Data.Set as Set
import Twitter.Search
import Twitter.Friendships (follow)
import Twitter.Data.Tweet
import Twitter.Data.User

followTimeline :: [Tweet] -> IO ()
followTimeline tweets = do
  let users       = Set.toList . Set.fromList $ map user tweets
  let followUsers = filter (not . following) users
  forM_ followUsers $ \followUser -> do
    let bio    = description followUser
    let newBio = T.replace "巨人" "" bio

    when ((T.length bio) == (T.length newBio)) $ do
      res <- follow $ screen_name followUser
      case res of
        Left  err -> putStrLn $ "[BAD] " ++ err
        Right u   -> putStrLn $ "[SUCCESS] " ++ (screen_name u)
      threadDelay (3 * 60 * 1000 * 1000)

main :: IO ()
main = do
  let isDigitOnly cs = foldl (\b c -> b && isDigit c) True cs

  args <- getArgs
  when (length args /= 2) $ error "[ERROR] length args /= 2"
  when ((not . isDigitOnly) (args !! 1)) $ error "[ERROR] args2: Not Int"

  let searchQ     = T.pack $ args !! 0
  let searchCount = read (args !! 1) :: Int

  res <- search $ makeSearchRequest searchQ "mixed" searchCount
  case res of
    Left  err -> error err
    Right tl  -> followTimeline (statuses tl)
