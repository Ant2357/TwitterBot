{-# LANGUAGE OverloadedStrings #-}

module Twitter.FollowersSpec (spec) where

import Test.Hspec
import Control.Exception (evaluate)
import Twitter.Followers
import Twitter.Data.Ids
import Twitter.Data.Users

spec :: Spec
spec = do
  describe "フォロワー一覧の取得テスト" $ do
    let screenName    = "ant2357"
    let badScreenName = "ant2357_run"

    it "フォロワー一覧の取得(IDリスト)" $ do
      res <- followerIds screenName
      case res of
        Left  _    -> "bad"                 `shouldBe`      "case"
        Right fIds -> ((length . ids) fIds) `shouldSatisfy` (> 0)

    it "実在しないユーザーのフォロワー一覧の取得(IDリスト)" $ do
      res <- followerIds badScreenName
      case res of
        Left  _ -> "goodcase" `shouldBe` "goodcase"
        Right _ -> "bad"      `shouldBe` "case"

    it "フォロワー一覧の取得([ユーザーオブジェクト])" $ do
      res <- followerList $ makeRequestFollowerList screenName 200
      case res of
        Left  _      -> "bad"                     `shouldBe` "case"
        Right fUsers -> ((length . users) fUsers) `shouldBe` 200

    it "実在しないユーザーのフォロワー一覧の取得([ユーザーオブジェクト])" $ do
      res <- followerList $ makeRequestFollowerList badScreenName 200
      case res of
        Left  _ -> "goodcase" `shouldBe` "goodcase"
        Right _ -> "bad"      `shouldBe` "case"
