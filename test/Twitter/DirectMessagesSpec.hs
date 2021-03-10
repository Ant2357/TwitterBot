{-# LANGUAGE OverloadedStrings #-}

module Twitter.DirectMessagesSpec (spec) where

import Test.Hspec
import Twitter.DirectMessages
import Twitter.Data.Dm

spec :: Spec
spec = do
  describe "dmのテスト" $ do
    let recipientId = "4432778053"
    let msg         = "HaskellからテストDM"
    it "DM送信" $ do
      res <- dm recipientId msg
      case res of
        Left  _  -> "bad"      `shouldBe` "case"
        Right tw -> "goodcase" `shouldBe` "goodcase"
