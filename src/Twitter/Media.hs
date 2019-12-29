{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveGeneric #-}

module Twitter.Media (MediaInfo (..), mediaUpload) where

import qualified Data.ByteString.Char8  as B8
import qualified Data.ByteString.Base64 as B64
import Data.Aeson
import GHC.Generics
import Network.HTTP.Conduit
import Twitter.TwSettings

data MediaInfo = MediaInfo {
  media_id :: Integer,
  size     :: Integer
} deriving (Show, Generic)

instance FromJSON MediaInfo
instance ToJSON   MediaInfo

mediaUpload :: B8.ByteString -> IO (Either String MediaInfo)
mediaUpload mediaData = do
  req         <- parseRequest "https://upload.twitter.com/1.1/media/upload.json"
  let postReq  = urlEncodedBody [("media_data", B64.encode mediaData)] req
  res         <- requestTwitterApi postReq
  return $ eitherDecode $ responseBody res
