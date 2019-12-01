{-# LANGUAGE OverloadedStrings #-}

module Main where

import Twitter.Statuses
import Twitter.Favorites
import Twitter.Data.Tweet
import qualified Data.Text.IO as T

main = do
  fav 1175793383412862976
  tweet "しろたん"

  timeline <- userTimeline TLRequest {
    twScreenName     = "ant2357",
    twCount          = 20,   -- 検索件数
    twExcludeReplies = True, -- リプライを除外するか
    twIncludeRts     = True  -- リツイートを含めるか
  }

  case timeline of
    Left err -> error err
    Right tl -> mapM_ (T.putStrLn . text) tl
