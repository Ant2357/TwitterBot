{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveGeneric #-}

module Twitter.Statuses ( TLRequest
                        , makeTLRequest
                        , userTimeline
                        , tweet
                        , mediaTweet
                        , unTweet
                        , retweet
                        , unRetweet
                        ) where

import qualified Data.ByteString.Char8 as B8
import Data.Text
import Data.Text.Encoding
import Data.Aeson
import GHC.Generics
import Network.HTTP.Conduit
import Web.Authenticate.OAuth
import Twitter.TwSettings
import Twitter.Data.Tweet

data TLRequest = TLRequest {
  twScreenName     :: String,
  twCount          :: Int,
  twExcludeReplies :: Bool,
  twIncludeRts     :: Bool
} deriving (Show, Generic)

makeTLRequest :: String -> Int -> Bool -> Bool -> TLRequest
makeTLRequest twScreenName twCount twExcludeReplies twIncludeRts
  | twCount < 1 || 200 < twCount = error "count range: 1 <= count <= 200"
  | otherwise                    = TLRequest
    { twScreenName     = twScreenName
    , twCount          = twCount
    , twExcludeReplies = twExcludeReplies
    , twIncludeRts     = twIncludeRts
    }

userTimeline :: TLRequest -> IO (Either String [Tweet])
userTimeline tRequest = do
  req       <- parseRequest
              $ "https://api.twitter.com/1.1/statuses/user_timeline.json"
              ++ "?screen_name=" ++ twScreenName tRequest
              ++ "&count=" ++ show (twCount tRequest)
              ++ "&exclude_replies=" ++ show (twExcludeReplies tRequest)
              ++ "&include_rts=" ++ show (twIncludeRts tRequest)
  signedReq <- signOAuth twOAuth twCredential req
  res       <- httpLbs signedReq =<< (newManager tlsManagerSettings)
  return $ eitherDecode $ responseBody res

tweet :: Text -> IO (Either String Tweet)
tweet tw = do
  req         <- parseRequest "https://api.twitter.com/1.1/statuses/update.json"
  let postReq  = urlEncodedBody [("status", encodeUtf8 tw)] req
  signedReq   <- signOAuth twOAuth twCredential postReq
  res         <- httpLbs signedReq =<< (newManager tlsManagerSettings)
  return $ eitherDecode $ responseBody res

mediaTweet :: Text -> Integer -> IO (Either String Tweet)
mediaTweet tw mediaIds = do
  req         <- parseRequest "https://api.twitter.com/1.1/statuses/update.json"
  let postReq  = urlEncodedBody [("status", encodeUtf8 tw), ("media_ids", (B8.pack . show) mediaIds)] req
  signedReq   <- signOAuth twOAuth twCredential postReq
  res         <- httpLbs signedReq =<< (newManager tlsManagerSettings)
  return $ eitherDecode $ responseBody res

unTweet :: Integer -> IO (Either String Tweet)
unTweet twId = do
  req         <- parseRequest
                $ "https://api.twitter.com/1.1/statuses/destroy/"
                ++ (show twId)
                ++ ".json"
  let postReq  = urlEncodedBody [("trim_user", "false")] req
  signedReq   <- signOAuth twOAuth twCredential postReq
  res         <- httpLbs signedReq =<< (newManager tlsManagerSettings)
  return $ eitherDecode $ responseBody res

retweet :: Integer -> IO (Either String Tweet)
retweet twId = do
  req         <- parseRequest
                $ "https://api.twitter.com/1.1/statuses/retweet/"
                ++ (show twId)
                ++ ".json"
  let postReq  = urlEncodedBody [("trim_user", "false")] req
  signedReq   <- signOAuth twOAuth twCredential postReq
  res         <- httpLbs signedReq =<< (newManager tlsManagerSettings)
  return $ eitherDecode $ responseBody res

unRetweet :: Integer -> IO (Either String Tweet)
unRetweet twId = do
  req         <- parseRequest
                $ "https://api.twitter.com/1.1/statuses/unretweet/"
                ++ (show twId)
                ++ ".json"
  let postReq  = urlEncodedBody [("trim_user", "false")] req
  signedReq   <- signOAuth twOAuth twCredential postReq
  res         <- httpLbs signedReq =<< (newManager tlsManagerSettings)
  return $ eitherDecode $ responseBody res
