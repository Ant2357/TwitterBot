{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DuplicateRecordFields #-}

module Twitter.Statuses (TLRequest (..), tweet, unTweet, userTimeline) where

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

tweet :: Text -> IO (Either String Tweet)
tweet tw = do
  req         <- parseRequest "https://api.twitter.com/1.1/statuses/update.json"
  let postReq  = urlEncodedBody [("status", encodeUtf8 tw)] req
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
