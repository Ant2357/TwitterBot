{-# LANGUAGE OverloadedStrings #-}

module Main where

import Twitter
import TLConfig

import qualified Data.Text.IO as T

main :: IO ()
main = do
  -- fav TweetID
  -- Ant渾身のツイートをいいね
  fav "1187353532933951494"
  tweet "a"

  timeline <- userTimeline TLConfig {
    twScreenName     = "ant2357",
    twCount          = 20,   -- 取得するツイート件数
    twExcludeReplies = True, -- リプライを除外するか
    twIncludeRts     = True  -- リツイートを含めるか
  }

  case timeline of
    Left err -> error err
    Right tl -> mapM_ (T.putStrLn . text) tl
