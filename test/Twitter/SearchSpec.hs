{-# LANGUAGE OverloadedStrings #-}

module Twitter.SearchSpec (spec) where

import Test.Hspec
import Twitter.Search
import Control.Exception (evaluate)

spec :: Spec
spec = do
  describe "searchのテスト" $ do
    let searchQ     = "#swallows"
    let searchCount = 10

    it "人気のツイート検索" $ do
      res <- search $ makeSearchRequest searchQ "popular" searchCount
      case res of
        Left  _  -> "bad" `shouldBe` "case"
        Right tl -> ((length . statuses) tl) `shouldSatisfy` (<= searchCount)

    it "最新のツイート検索" $ do
      res <- search $ makeSearchRequest searchQ "recent" searchCount
      case res of
        Left  _  -> "bad" `shouldBe` "case"
        Right tl -> ((length . statuses) tl) `shouldSatisfy` (<= searchCount)

    it "全てのツイート検索" $ do
      res <- search $ makeSearchRequest searchQ "mixed" searchCount
      case res of
        Left  _  -> "bad" `shouldBe` "case"
        Right tl -> ((length . statuses) tl) `shouldSatisfy` (<= searchCount)

    it "不正なsearchQ(1未満)" $ do
      evaluate (makeSearchRequest searchQ "popular" 0) `shouldThrow` errorCall "searchCount range: 1 <= searchCount <= 100"
    it "不正なsearchQ(100超え)" $ do
      evaluate (makeSearchRequest searchQ "popular" 101) `shouldThrow` errorCall "searchCount range: 1 <= searchCount <= 100"
    it "不正なresult_type" $ do
      evaluate (makeSearchRequest searchQ "hoge" searchCount) `shouldThrow` errorCall "expected value resultType: \"popular\" or \"recent\" or \"mixed\""
