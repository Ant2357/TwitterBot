{-# LANGUAGE DuplicateRecordFields #-}

module Twitter.FriendshipsSpec (spec) where

import Test.Hspec
import Twitter.Friendships
import Twitter.Data.User 
import Prelude hiding (lookup)

spec :: Spec
spec = do
  describe "フォロー関連のテスト" $ do
    let followName = "nhk_news"
    context "followのテスト" $ do
      it "フォロー" $ do
        res <- follow followName
        case res of
          Left  _  -> "bad"                      `shouldBe` "case"
          Right tw -> (screen_name (tw :: User)) `shouldBe` followName

      it "二重フォロー" $ do
        res <- follow followName
        case res of
          Left  _  -> "bad"          `shouldBe` "case"
          Right tw -> (following tw) `shouldBe` True

    context "unFollowのテスト" $ do
      it "アンフォロー" $ do
        res <- unFollow followName
        case res of
          Left  _  -> "bad"                      `shouldBe` "case"
          Right tw -> (screen_name (tw :: User)) `shouldBe` followName

      it "二重アンフォロー" $ do
        res <- unFollow followName
        case res of
          Left  _  -> "bad"          `shouldBe` "case"
          Right tw -> (following tw) `shouldBe` False

  describe "lookupのテスト" $ do
    let lastScreenName = "strimuer213p"
    it "自分と対象ユーザーの関係を調べる([ID])" $ do
      res <- lookup [775566164487393280, 4454609233]
      case res of
        Left  _  -> "bad"                                     `shouldBe` "case"
        Right xs -> (screen_name ((last xs) :: Relationship)) `shouldBe` lastScreenName

    it "自分と対象ユーザーの関係を調べる([String])" $ do
      res <- lookup ["masaniwasdp", "strimuer213p"]
      case res of
        Left  _  -> "bad"                                     `shouldBe` "case"
        Right xs -> (screen_name ((last xs) :: Relationship)) `shouldBe` lastScreenName

    it "引数が空リスト" $ do
      res <- lookup []
      case res of
        Left  _  -> "bad"     `shouldBe` "case"
        Right xs -> (null xs) `shouldBe` True
