{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveGeneric #-}

module Twitter (Tweet (..), tweet, fav, userTimeline) where

import Data.Text
import Data.Text.Encoding
import Data.Aeson
import GHC.Generics
import Network.HTTP.Conduit
import Web.Authenticate.OAuth
import TwSettings
import TLConfig

newtype Tweet = Tweet { text :: Text } deriving (Show, Generic)

instance FromJSON Tweet
instance ToJSON Tweet

tweet :: Text -> IO ()
tweet tw = do
  req         <- parseRequest "https://api.twitter.com/1.1/statuses/update.json"
  manager     <- newManager tlsManagerSettings
  let postReq  = urlEncodedBody [("status", encodeUtf8 tw)] req
  signedReq   <- signOAuth twOAuth twCredential postReq
  httpLbs signedReq manager
  return ()

fav :: Text -> IO ()
fav twId = do
  req         <- parseRequest "https://api.twitter.com/1.1/favorites/create.json"
  manager     <- newManager tlsManagerSettings
  let postReq  = urlEncodedBody [("id", encodeUtf8 twId)] req
  signedReq   <- signOAuth twOAuth twCredential postReq
  httpLbs signedReq manager
  return ()

userTimeline :: TLConfig -> IO (Either String [Tweet])
userTimeline tConfig = do
  req       <- parseRequest
              $ "https://api.twitter.com/1.1/statuses/user_timeline.json"
              ++ "?screen_name=" ++ twScreenName tConfig
              ++ "&count=" ++ show (twCount tConfig)
              ++ "&exclude_replies=" ++ show (twExcludeReplies tConfig)
              ++ "&include_rts=" ++ show (twIncludeRts tConfig)
  signedReq <- signOAuth twOAuth twCredential req
  manager   <- newManager tlsManagerSettings
  res       <- httpLbs signedReq manager
  return $ eitherDecode $ responseBody res
