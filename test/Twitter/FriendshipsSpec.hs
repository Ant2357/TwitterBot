{-# LANGUAGE OverloadedStrings #-}

module Twitter.FriendshipsSpec (spec) where

import Test.Hspec
import Twitter.Friendships
import Twitter.Data.User

spec :: Spec
spec = do
  let screenName = "nhk_news"
  describe "followのテスト" $ do
    it "フォロー" $ do
      res <- follow screenName
      case res of
        Left  _  -> "bad"            `shouldBe` "case"
        Right tw -> (screen_name tw) `shouldBe` screenName

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
        Right tw -> (screen_name tw) `shouldBe` screenName

    it "二重アンフォロー" $ do
      res <- unFollow screenName
      case res of
        Left  _  -> "bad"          `shouldBe` "case"
        Right tw -> (following tw) `shouldBe` False
