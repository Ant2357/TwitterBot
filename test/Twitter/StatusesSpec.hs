
module Twitter.StatusesSpec (spec) where

import Test.Hspec
import Twitter.Statuses
import Twitter.Data.Tweet
import qualified Data.Text    as T
import qualified Data.Text.IO as T
import Prelude hiding (id)

errorCaseTest :: Either String [Tweet] -> Expectation
errorCaseTest timeline = do
  case timeline of
    Left  err -> err `shouldBe` "Error in $: parsing [] failed, expected Array, but encountered Object"
    Right _   -> "bad" `shouldBe` "case"

timelineTweetCountEq :: Either String [Tweet] -> Int -> Expectation
timelineTweetCountEq timeline tweetCount = do
  case timeline of
    Left  _  -> "bad" `shouldBe` "case"
    Right tl -> (length tl) `shouldBe` tweetCount

spec :: Spec
spec = do
  describe "userTimelineのテスト" $ do
    let screenName = "github"
    context "正常系" $ do
      it "リプライRTの除外無し" $ do
        timeline <- userTimeline TLRequest { twScreenName = screenName, twCount = 100, twExcludeReplies = False, twIncludeRts = True }
        timeline `timelineTweetCountEq` 100
      it "リプライRTを除外" $ do
        timeline <- userTimeline TLRequest { twScreenName = screenName, twCount = 100, twExcludeReplies = True, twIncludeRts = False }
        case timeline of
          Left  err -> "bad" `shouldBe` "case"
          Right tl  -> (length tl) `shouldSatisfy` (<= 100)

      it "ユーザーにブロックされている" $ do
        timeline <- userTimeline TLRequest { twScreenName = "OffGao", twCount = 20, twExcludeReplies = False, twIncludeRts = True }
        errorCaseTest timeline
      it "ユーザーが凍結している" $ do
        timeline <- userTimeline TLRequest { twScreenName = "paiza_run", twCount = 20, twExcludeReplies = False, twIncludeRts = True }
        errorCaseTest timeline
      it "存在しないユーザー" $ do
        timeline <- userTimeline TLRequest { twScreenName = "ant24653", twCount = 20, twExcludeReplies = True,  twIncludeRts = False }
        errorCaseTest timeline

    context "count関連のテスト" $ do
      let defaultCount = 20
      let maxTwCount   = 200
      let shortIntMax  = 32767

      it "countの上限" $ do
        timeline <- userTimeline TLRequest { twScreenName = screenName, twCount = maxTwCount, twExcludeReplies = False, twIncludeRts = True }
        timeline `timelineTweetCountEq` maxTwCount

      it "countの上限越え" $ do
        timeline <- userTimeline TLRequest { twScreenName = screenName, twCount = maxTwCount + 1000, twExcludeReplies = False, twIncludeRts = True }
        timeline `timelineTweetCountEq` maxTwCount

      -- countの値が不正な場合は、countを省略した扱いになる (countのデフォルト値: 20)
      it "countの値が不正(負の数)" $ do
        timeline <- userTimeline TLRequest { twScreenName = screenName, twCount = -1, twExcludeReplies = False, twIncludeRts = True }
        timeline `timelineTweetCountEq` defaultCount

      it "countの値が不正(オーバーフロー)" $ do
        timeline <- userTimeline TLRequest { twScreenName = screenName, twCount = shortIntMax + 1, twExcludeReplies = False, twIncludeRts = True }
        timeline `timelineTweetCountEq` defaultCount

  describe "tweetのテスト" $ do
    let twMsg = T.pack "にゃーん"
    it "ツイート" $ do
      res <- tweet twMsg
      case res of
        Left  err -> "bad" `shouldBe` "case"
        Right tw  -> (text tw) `shouldBe` twMsg

    it "重複ツイート" $ do
      res <- tweet twMsg
      case res of
        Left  _ -> "goodcase" `shouldBe` "goodcase"
        Right _ -> "bad" `shouldBe` "case"

  describe "unTweetのテスト" $ do
    it "ツイート削除" $ do
      timeline <- userTimeline TLRequest { twScreenName = "ant2357", twCount = 1, twExcludeReplies = False, twIncludeRts = True }
      case timeline of
        Left  _  -> "timelineBad" `shouldBe` "case"
        Right tl -> do
          let delTwId = id (tl !! 0)
          res <- unTweet delTwId
          case res of
            Left  err -> "bad" `shouldBe` "case"
            Right tw  -> (id tw) `shouldBe` delTwId

    it "存在しないツイートを削除" $ do
      res <- unTweet 0
      case res of
        Left  err -> "goodcase" `shouldBe` "goodcase"
        Right tw  -> "bad" `shouldBe` "case"
