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
        Left  _  -> "bad" `shouldBe` "case"
        Right tw -> ( dm_text
                    . dm_message_data
                    . dm_message_create
                    . dm_event
                    ) tw  `shouldBe` msg

    it "存在しない相手にDM送信" $ do
      res <- dm "-1" msg
      case res of
        Left  _ -> "goodcase" `shouldBe` "goodcase"
        Right _ -> "bad"      `shouldBe` "case"
