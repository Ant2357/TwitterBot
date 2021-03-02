{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveGeneric #-}

module Twitter.Media (Media (..), mediaUpload) where

import qualified Data.ByteString.Char8  as B8
import qualified Data.ByteString.Base64 as B64
import Data.Aeson
import GHC.Generics
import Network.HTTP.Conduit
import Twitter.TwSettings

data Media = Media {
  media_id :: Integer,
  size     :: Integer
} deriving (Show, Generic)

instance FromJSON Media
instance ToJSON   Media

mediaUpload :: B8.ByteString -> IO (Either String Media)
mediaUpload mediaData = do
  req         <- parseRequest "https://upload.twitter.com/1.1/media/upload.json"
  let postReq  = urlEncodedBody [("media_data", B64.encode mediaData)] req
  res         <- requestTwitterApi postReq
  return $ eitherDecode $ responseBody res
