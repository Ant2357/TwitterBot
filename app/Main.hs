{-# LANGUAGE OverloadedStrings #-}

module Main where

import Twitter
import qualified Data.Text.IO as T

main :: IO ()
main = do
  -- fav TweetID
  -- Ant渾身のツイートをいいね
  fav "1187353532933951494"
  tweet "a"

  timeline <- userTimeline "ant2357" 20
  case timeline of
    Left err -> error err
    Right tl -> mapM_ (T.putStrLn . text) tl
