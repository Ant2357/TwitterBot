{-# LANGUAGE OverloadedStrings #-}

module Main where

import Twitter.Statuses
import Twitter.Data.Tweet
import qualified Data.Text.IO as T

main = do
  tweet "しろたん"

  -- makeTLRequest ユーザー名 検索件数 リプライを除外するか? リツイートを含めるか?
  -- makeTLRequest :: String -> Int -> Bool -> Bool -> TLRequest
  timeline <- userTimeline $ makeTLRequest "ant2357" 20 True True

  case timeline of
    Left err -> error err
    Right tl -> mapM_ (T.putStrLn . text) tl
