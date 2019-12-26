{-# LANGUAGE OverloadedStrings #-}

module Twitter.StatusesSpec (spec) where

import Test.Hspec
import Control.Exception (evaluate)
import Twitter.Statuses
import Twitter.Media
import Twitter.Data.Tweet
import qualified Data.ByteString.Char8 as B8
import qualified Data.Text             as T
import qualified Data.Text.IO          as T
import System.Directory
import Prelude hiding (id)

errorCaseTest :: Either String [Tweet] -> Expectation
errorCaseTest timeline = do
  case timeline of
    Left  err -> err   `shouldBe` "Error in $: parsing [] failed, expected Array, but encountered Object"
    Right _   -> "bad" `shouldBe` "case"

timelineTweetCountEq :: Either String [Tweet] -> Int -> Expectation
timelineTweetCountEq timeline tweetCount = do
  case timeline of
    Left  _  -> "bad"       `shouldBe` "case"
    Right tl -> (length tl) `shouldBe` tweetCount

spec :: Spec
spec = do
  describe "makeTLRequestのテスト" $ do
    it "countが不正(1未満)" $ do
      evaluate (makeTLRequest "ant2357" 0 False True) `shouldThrow` errorCall "count range: 1 <= count <= 200"
    it "countが不正(200超え)" $ do
      evaluate (makeTLRequest "ant2357" 201 False True) `shouldThrow` errorCall "count range: 1 <= count <= 200"

  describe "userTimelineのテスト" $ do
    let screenName = "github"
    it "リプライRTの除外無し" $ do
      timeline <- userTimeline $ makeTLRequest screenName 100 False True
      timeline `timelineTweetCountEq` 100
    it "リプライRTを除外" $ do
      timeline <- userTimeline $ makeTLRequest screenName 100 True False
      case timeline of
        Left  _  -> "bad"       `shouldBe`      "case"
        Right tl -> (length tl) `shouldSatisfy` (<= 100)

    it "ユーザーにブロックされている" $ do
      timeline <- userTimeline $ makeTLRequest "OffGao" 20 False True
      errorCaseTest timeline
    it "ユーザーが凍結している" $ do
      timeline <- userTimeline $ makeTLRequest "paiza_run" 20 False True
      errorCaseTest timeline
    it "存在しないユーザー" $ do
      timeline <- userTimeline $ makeTLRequest "ant2357_run" 20 False True
      errorCaseTest timeline

  describe "tweetのテスト" $ do
    let twMsg = "にゃーん"
    it "ツイート" $ do
      res <- tweet twMsg
      case res of
        Left  _  -> "bad"     `shouldBe` "case"
        Right tw -> (text tw) `shouldBe` twMsg

    it "重複ツイート" $ do
      res <- tweet twMsg
      case res of
        Left  _ -> "goodcase" `shouldBe` "goodcase"
        Right _ -> "bad"      `shouldBe` "case"

  describe "mediaTweetのテスト" $ do
    it "画像ツイート" $ do
      let twMsg = "Haskellから画像ツイート"
      dir       <- getCurrentDirectory
      mediaFile <- B8.readFile $ dir ++ "/test/Img/example.jpg"
      mediaRes  <- mediaUpload mediaFile
      case mediaRes of
        Left  _ -> "mediaBad" `shouldBe` "case"
        Right m -> do
          res <- mediaTweet twMsg (media_id m)
          case res of
            Left  _  -> "bad"                               `shouldBe` "case"
            Right tw -> (T.take (T.length twMsg) (text tw)) `shouldBe` twMsg

  describe "unTweetのテスト" $ do
    it "ツイート削除" $ do
      timeline <- userTimeline $ makeTLRequest "ant2357" 2 False True
      case timeline of
        Left  _  -> "timelineBad" `shouldBe` "case"
        Right tl -> mapM_ (\tw -> do
          let delTwId = id tw
          res <- unTweet delTwId
          case res of
            Left  _  -> "bad"   `shouldBe` "case"
            Right tw -> (id tw) `shouldBe` delTwId
          ) tl

    it "存在しないツイートを削除" $ do
      res <- unTweet 0
      case res of
        Left  _ -> "goodcase" `shouldBe` "goodcase"
        Right _ -> "bad"      `shouldBe` "case"

  describe "RT関連のテスト" $ do
    let twId     = 990243814227918851     -- テストするツイートのID
    let twText   = "ちんちん"              -- ツイート本文
    let twRtText = "RT @ant2357: ちんちん" -- RTのツイート本文

    it "リツイート" $ do
      res <- retweet twId
      case res of
        Left  _  -> "bad"     `shouldBe` "case"
        Right tw -> (text tw) `shouldBe` twRtText
    it "RT済みのツイートをRT" $ do
      res <- retweet twId
      case res of
        Left  _ -> "goodcase" `shouldBe` "goodcase"
        Right _ -> "bad"      `shouldBe` "case"

    it "RT解除" $ do
      res <- unRetweet twId
      case res of
        Left  _  -> "bad"     `shouldBe` "case"
        Right tw -> (text tw) `shouldBe` twText
    it "未RTのツイートをRT解除" $ do
      {-
        RTしていないツイートをstatuses/unretweet/:idでRT解除しようとしても、
        エラーを吐かないTwitterほんまにゃーん
        何でツイート情報を返してくるの…
      -}
      res <- unRetweet twId
      case res of
        Left  _  -> "bad"   `shouldBe` "case"
        Right tw -> (id tw) `shouldBe` twId
