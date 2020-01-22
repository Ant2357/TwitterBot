{-# LANGUAGE OverloadedStrings #-}

module Main where

import Twitter.Statuses
import Twitter.Data.Tweet
import qualified Data.Text.IO as T

main = do
  tweet "しろたん"

  -- 200件分のツイートを取得
  timeline <- userTimeline $ newTLRequest
    defTLRequest { twScreenName = "ant2357" } 200

  case timeline of
    Left err -> error err
    Right tl -> mapM_ (T.putStrLn . text) tl
