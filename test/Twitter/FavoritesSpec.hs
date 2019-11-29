
module Twitter.FavoritesSpec (spec) where

import Test.Hspec
import Twitter.Favorites
import Twitter.Data.Tweet
import Prelude hiding (id)

badCaseTest :: Either String Tweet -> Expectation
badCaseTest res = do
  case res of
    Left  err -> "goodcase" `shouldBe` "goodcase"
    Right tw  -> "bad" `shouldBe` "case"

spec :: Spec
spec = do
  describe "いいねのテスト" $ do
    let twId = "1175793383412862976"
    it "いいねする" $ do
      res <- fav twId
      case res of
        Left  err -> "bad" `shouldBe` "case"
        Right tw  -> (id tw) `shouldBe` (read twId)

    it "いいね済みのツイートにいいね" $ do
      res <- fav twId
      badCaseTest res

    it "いいね解除" $ do
      res <- unFav twId
      case res of
        Left  err -> "bad" `shouldBe` "case"
        Right tw  -> (id tw) `shouldBe` (read twId)

    it "未いいねに対していいね解除" $ do
      res <- unFav twId
      badCaseTest res

    context "異常系" $ do
      it "実在しないツイートID" $ do
        res <- unFav "0"
        badCaseTest res

      it "ツイートIDが不正" $ do
        res <- unFav "abcd"
        badCaseTest res
