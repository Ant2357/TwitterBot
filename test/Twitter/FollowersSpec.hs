{-# LANGUAGE OverloadedStrings #-}

module Twitter.FollowersSpec (spec) where

import Test.Hspec
import Twitter.Followers

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
