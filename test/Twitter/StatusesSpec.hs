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
  describe "newTLRequestのテスト" $ do
    it "countが不正(1未満)" $ do
      evaluate (newTLRequest defTLRequest 0) `shouldThrow` errorCall "count range: 1 <= count <= 200"
    it "countが不正(200超え)" $ do
      evaluate (newTLRequest defTLRequest 201) `shouldThrow` errorCall "count range: 1 <= count <= 200"

  describe "homeTimelineのテスト" $ do
    it "TL取得" $ do
      timeline <- homeTimeline $ newTLRequest defTLRequest 10
      timeline `timelineTweetCountEq` 10

  describe "userTimelineのテスト" $ do
    let screenName = "github"
    it "リプライRTの除外無し" $ do
      timeline <- userTimeline $ newTLRequest defTLRequest { twScreenName = screenName } 100
      timeline `timelineTweetCountEq` 100
    it "リプライRTを除外" $ do
      timeline <- userTimeline $ newTLRequest defTLRequest
        { twScreenName     = screenName
        , twExcludeReplies = True
        , twIncludeRts     = False
        } 100
      case timeline of
        Left  _  -> "bad"       `shouldBe`      "case"
        Right tl -> (length tl) `shouldSatisfy` (<= 100)

    it "ユーザーが鍵垢(非公開)" $ do
      timeline <- userTimeline $ newTLRequest defTLRequest { twScreenName = "OffGao" } 20
      errorCaseTest timeline
    it "ユーザーが凍結している" $ do
      timeline <- userTimeline $ newTLRequest defTLRequest { twScreenName = "paiza_run" } 20
      errorCaseTest timeline
    it "存在しないユーザー" $ do
      timeline <- userTimeline $ newTLRequest defTLRequest { twScreenName = "ant2357_run" } 20
      errorCaseTest timeline

  describe "tweetのテスト" $ do
    let twMsg = "Haskellから自動ツイート"
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

  describe "replyのテスト" $ do
    let twId  = 1373291233943064584
    let twMsg = "Haskellから自動リプライ"
    it "返信ツイート" $ do
      res <- reply twId twMsg
      case res of
        Left  _  -> "bad"     `shouldBe` "case"
        Right tw -> (text tw) `shouldBe` twMsg

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
      timeline <- homeTimeline $ newTLRequest defTLRequest 3
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
      -}
      res <- unRetweet twId
      case res of
        Left  _  -> "bad"   `shouldBe` "case"
        Right tw -> (id tw) `shouldBe` twId
