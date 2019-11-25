
module Twitter.StatusesSpec (spec) where

import Test.Hspec
import Twitter.Statuses

badCaseTest :: Either String [Tweet] -> Expectation
badCaseTest timeline = do
  case timeline of
    Left err -> err `shouldBe` "Error in $: parsing [] failed, expected Array, but encountered Object"
    Right tl -> "bad" `shouldBe` "case"

spec :: Spec
spec = do
  describe "userTimelineのテスト" $ do
    it "リプライRTの除外無し" $ do
      timeline <- userTimeline TLRequest { twScreenName = "ant2357", twCount = 100, twExcludeReplies = False, twIncludeRts = True }
      case timeline of
        Left err -> "bad" `shouldBe` "case"
        Right tl -> (length tl) `shouldBe` 100

    it "リプライRTを除外" $ do
      timeline <- userTimeline TLRequest { twScreenName = "ant2357", twCount = 100, twExcludeReplies = True, twIncludeRts = False }
      case timeline of
        Left err -> "bad" `shouldBe` "case"
        Right tl -> (length tl) `shouldSatisfy` (<= 100)

    let maxTwCount = 200
    it "countの上限" $ do
      timeline <- userTimeline TLRequest { twScreenName = "ant2357", twCount = maxTwCount, twExcludeReplies = False, twIncludeRts = True }
      case timeline of
        Left err -> "bad" `shouldBe` "case"
        Right tl -> (length tl) `shouldBe` maxTwCount

    it "countの上限越え" $ do
      timeline <- userTimeline TLRequest { twScreenName = "ant2357", twCount = maxTwCount + 1000, twExcludeReplies = False, twIncludeRts = True }
      case timeline of
        Left err -> "bad" `shouldBe` "case"
        Right tl -> (length tl) `shouldBe` maxTwCount

    it "countオーバーフロー" $ do
      -- countがオーバーフローした際は、countを省略した扱いになる (countのデフォルト値: 20)
      let defaultCount = 20
      let shortIntMax  = 32767
      timeline <- userTimeline TLRequest { twScreenName = "ant2357", twCount = shortIntMax + 1, twExcludeReplies = False, twIncludeRts = True }
      case timeline of
        Left err -> "bad" `shouldBe` "case"
        Right tl -> (length tl) `shouldBe` defaultCount

    it "ユーザーにブロックされている" $ do
      timeline <- userTimeline TLRequest { twScreenName = "OffGao", twCount = 20, twExcludeReplies = False, twIncludeRts = True }
      badCaseTest timeline

    it "ユーザーが凍結している" $ do
      timeline <- userTimeline TLRequest { twScreenName = "paiza_run", twCount = 20, twExcludeReplies = False, twIncludeRts = True }
      badCaseTest timeline

    it "存在しないユーザー" $ do
      timeline <- userTimeline TLRequest { twScreenName = "ant24653", twCount = 20, twExcludeReplies = True,  twIncludeRts = False }
      badCaseTest timeline
