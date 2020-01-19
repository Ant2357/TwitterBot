{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DuplicateRecordFields #-}

module Twitter.Followers (followerIds, followerList) where

import Data.Aeson
import GHC.Generics
import Network.HTTP.Conduit
import Twitter.TwSettings
import Twitter.Data.User
import Twitter.Data.IdsInfo
import Twitter.Data.UserRequest
import Twitter.Data.UsersInfo

followerIds :: String -> IO (Either String IdsInfo)
followerIds screenName = do
  req <- parseRequest
      $ "https://api.twitter.com/1.1/followers/ids.json"
      ++ "?screen_name=" ++ screenName
  res <- requestTwitterApi req
  return $ eitherDecode $ responseBody res

followerList :: UserRequest -> IO (Either String UsersInfo)
followerList uRequest = do
  req <- parseRequest
      $ "https://api.twitter.com/1.1/followers/list.json"
      ++ "?screen_name=" ++ screenName uRequest
      ++ "&count=" ++ show (count uRequest)
  res <- requestTwitterApi req
  return $ eitherDecode $ responseBody res
