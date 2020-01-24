{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DuplicateRecordFields #-}

module Twitter.Followers (  RequestFollowerList
                          , makeRequestFollowerList
                          , followerList
                          , followerIds
                          ) where

import Data.Aeson
import GHC.Generics
import Network.HTTP.Conduit
import Twitter.TwSettings
import Twitter.Data.IdsInfo
import Twitter.Data.UsersInfo

followerIds :: String -> IO (Either String IdsInfo)
followerIds screenName = do
  req <- parseRequest
      $ "https://api.twitter.com/1.1/followers/ids.json"
      ++ "?screen_name=" ++ screenName
  res <- requestTwitterApi req
  return $ eitherDecode $ responseBody res

data RequestFollowerList = RequestFollowerList {
  screenName :: String,
  count      :: Int
} deriving (Show, Generic)

makeRequestFollowerList :: String -> Int -> RequestFollowerList
makeRequestFollowerList screenName count
  | count < 1 || 200 < count = error "count range: 1 <= count <= 200"
  | otherwise                = RequestFollowerList { screenName = screenName, count = count }

followerList :: RequestFollowerList -> IO (Either String UsersInfo)
followerList fRequest = do
  req <- parseRequest
      $ "https://api.twitter.com/1.1/followers/list.json"
      ++ "?screen_name=" ++ screenName fRequest
      ++ "&count=" ++ show (count fRequest)
  res <- requestTwitterApi req
  return $ eitherDecode $ responseBody res
