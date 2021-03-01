
module Twitter.FavoritesSpec (spec) where

import Test.Hspec
import Twitter.Favorites
import Twitter.Data.Tweet
import Prelude hiding (id)

badCaseTest :: Either String Tweet -> Expectation
badCaseTest res = do
  case res of
    Left  _  -> "goodcase" `shouldBe` "goodcase"
    Right tw -> "bad"      `shouldBe` "case"

spec :: Spec
spec = do
  describe "いいねのテスト" $ do
    let twId = 1326924708126752768
    it "いいねする" $ do
      res <- fav twId
      case res of
        Left  _  -> "bad"   `shouldBe` "case"
        Right tw -> (id tw) `shouldBe` twId

    it "いいね済みのツイートにいいね" $ do
      res <- fav twId
      badCaseTest res

    it "いいね解除" $ do
      res <- unFav twId
      case res of
        Left  _  -> "bad"   `shouldBe` "case"
        Right tw -> (id tw) `shouldBe` twId

    it "未いいねに対していいね解除" $ do
      res <- unFav twId
      badCaseTest res

    it "実在しないTweetID" $ do
      res <- fav (-1)
      badCaseTest res
