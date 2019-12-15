{-# LANGUAGE OverloadedStrings #-}

module Main where

import Twitter.Statuses
import Twitter.Favorites
import Twitter.Media
import Twitter.Data.Tweet
import qualified Data.Text.IO as T
import qualified Data.ByteString.Char8 as B8
import System.Directory

main = do
  -- 画像アップロード
  dir       <- getCurrentDirectory
  mediaFile <- B8.readFile $ dir ++ "/src/Img/example.jpg"
  res       <- mediaUpload mediaFile
  case res of
    Left err -> error err
    Right m  -> putStrLn ((show . media_id) m)

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
