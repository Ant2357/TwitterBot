
module Twitter.MediaSpec (spec) where

import Test.Hspec
import Twitter.Media
import System.Directory
import qualified Data.ByteString.Char8 as B8

spec :: Spec
spec = do
  describe "mediaUploadのテスト" $ do
    it "画像送信" $ do
      dir       <- getCurrentDirectory
      mediaFile <- B8.readFile $ dir ++ "/src/Img/example.jpg"
      res       <- mediaUpload mediaFile
      case res of
        Left err -> "bad" `shouldBe` "case"
        Right m  -> "goodcase" `shouldBe` "goodcase"
