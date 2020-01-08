{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DuplicateRecordFields #-}

module Twitter.Followers (  FollowerIdsResult (..)
                          , FollowerListsResult (..)
                          , FollowerListRequest
                          , makeFollowerListRequest
                          , followerIds
                          , followerList
                          ) where

import Data.Aeson
import GHC.Generics
import Network.HTTP.Conduit
import Twitter.TwSettings
import Twitter.Data.User

data FollowerIdsResult = FollowerIdsResult {
  ids                 :: [Integer],
  next_cursor         :: Integer,
  next_cursor_str     :: String,
  previous_cursor     :: Integer,
  previous_cursor_str :: String
} deriving (Show, Generic)
instance FromJSON FollowerIdsResult
instance ToJSON   FollowerIdsResult

data FollowerListsResult = FollowerListsResult {
  users               :: [User],
  next_cursor         :: Integer,
  next_cursor_str     :: String,
  previous_cursor     :: Integer,
  previous_cursor_str :: String
} deriving (Show, Generic)
instance FromJSON FollowerListsResult
instance ToJSON   FollowerListsResult

data FollowerListRequest = FollowerListRequest {
  screenName :: String,
  count      :: Int
} deriving (Show, Generic)

makeFollowerListRequest :: String -> Int -> FollowerListRequest
makeFollowerListRequest screenName count
  | count < 1 || 200 < count = error "count range: 1 <= count <= 200"
  | otherwise                = FollowerListRequest { screenName = screenName, count = count }

followerIds :: String -> IO (Either String FollowerIdsResult)
followerIds screenName = do
  req       <- parseRequest
              $ "https://api.twitter.com/1.1/followers/ids.json"
              ++ "?screen_name=" ++ screenName
  res       <- requestTwitterApi req
  return $ eitherDecode $ responseBody res

followerList :: FollowerListRequest -> IO (Either String FollowerListsResult)
followerList fRequest = do
  req       <- parseRequest
              $ "https://api.twitter.com/1.1/followers/list.json"
              ++ "?screen_name=" ++ screenName fRequest
              ++ "&count=" ++ show (count fRequest)
  res       <- requestTwitterApi req
  return $ eitherDecode $ responseBody res
