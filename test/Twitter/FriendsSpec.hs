{-# LANGUAGE OverloadedStrings #-}

module Twitter.FriendsSpec (spec) where

import Test.Hspec
import Twitter.Friends
import Twitter.Data.Ids
import Twitter.Data.UsersInfo

spec :: Spec
spec = do
  describe "Twitter.Friendsのテスト" $ do
    let screenName    = "ant2357"
    let badScreenName = "ant2357_run"

    it "フォロー一覧の取得(IDリスト)" $ do
      res <- followIds screenName
      case res of
        Left  _    -> "bad"                 `shouldBe`      "case"
        Right fIds -> ((length . ids) fIds) `shouldSatisfy` (> 0)

    it "実在しないユーザーのフォロー一覧の取得(IDリスト)" $ do
      res <- followIds badScreenName 
      case res of
        Left  _ -> "goodcase" `shouldBe` "goodcase"
        Right _ -> "bad"      `shouldBe` "case"

    it "フォロー一覧の取得([ユーザーオブジェクト])" $ do
      res <- followList $ makeRequestFollowList screenName 200
      case res of
        Left  _  -> "bad"                 `shouldBe` "case"
        Right xs -> ((length . users) xs) `shouldBe` 200 

    it "実在しないユーザーのフォロー一覧の取得([ユーザーオブジェクト])" $ do
      res <- followList $ makeRequestFollowList badScreenName 200
      case res of
        Left  _ -> "goodcase" `shouldBe` "goodcase"
        Right _ -> "bad"      `shouldBe` "case"
