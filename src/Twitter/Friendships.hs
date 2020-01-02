{-# LANGUAGE OverloadedStrings #-}

module Twitter.Friendships (follow, unFollow) where

import qualified Data.ByteString.Char8 as B8
import Data.Aeson
import Network.HTTP.Conduit
import Twitter.TwSettings
import Twitter.Data.User

follow :: String -> IO (Either String User)
follow screenName = do
  req         <- parseRequest "https://api.twitter.com/1.1/friendships/create.json"
  let postReq  = urlEncodedBody [("screen_name", B8.pack screenName)] req
  res         <- requestTwitterApi postReq
  return $ eitherDecode $ responseBody res

unFollow :: String -> IO (Either String User)
unFollow screenName = do
  req         <- parseRequest "https://api.twitter.com/1.1/friendships/destroy.json"
  let postReq  = urlEncodedBody [("screen_name", B8.pack screenName)] req
  res         <- requestTwitterApi postReq
  return $ eitherDecode $ responseBody res
