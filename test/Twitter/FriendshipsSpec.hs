{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DuplicateRecordFields #-}

module Twitter.FriendshipsSpec (spec) where

import Test.Hspec
import Twitter.Friendships
import Twitter.Data.User 
import Prelude hiding (lookup)

spec :: Spec
spec = do
  let screenName = "nhk_news"
  describe "followのテスト" $ do
    it "フォロー" $ do
      res <- follow screenName
      case res of
        Left  _  -> "bad"            `shouldBe` "case"
        Right tw -> (screen_name (tw :: User)) `shouldBe` screenName

    it "二重フォロー" $ do
      res <- follow screenName
      case res of
        Left  _  -> "bad"          `shouldBe` "case"
        Right tw -> (following tw) `shouldBe` True

  describe "unFollowのテスト" $ do
    it "アンフォロー" $ do
      res <- unFollow screenName
      case res of
        Left  _  -> "bad"            `shouldBe` "case"
        Right tw -> (screen_name (tw :: User)) `shouldBe` screenName

    it "二重アンフォロー" $ do
      res <- unFollow screenName
      case res of
        Left  _  -> "bad"          `shouldBe` "case"
        Right tw -> (following tw) `shouldBe` False

  describe "lookupのテスト" $ do
    let screenName = "strimuer213p" :: String
    it "自分と対象ユーザーの関係を調べる(数値)" $ do
      res <- lookup 4454609233
      case res of
        Left  _  -> "bad"                                     `shouldBe` "case"
        Right xs -> (screen_name ((head xs) :: Relationship)) `shouldBe` screenName

    it "自分と対象ユーザーの関係を調べる(String)" $ do
      res <- lookup screenName
      case res of
        Left  _  -> "bad"                                     `shouldBe` "case"
        Right xs -> (screen_name ((head xs) :: Relationship)) `shouldBe` screenName
