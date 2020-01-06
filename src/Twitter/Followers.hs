{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveGeneric #-}

module Twitter.Followers (FollowerIdsResult (..), followerIds) where

import Data.Aeson
import GHC.Generics
import Network.HTTP.Conduit
import Twitter.TwSettings

data FollowerIdsResult = FollowerIdsResult {
  ids                 :: [Integer],
  next_cursor         :: Integer,
  next_cursor_str     :: String,
  previous_cursor     :: Integer,
  previous_cursor_str :: String
} deriving (Show, Generic)
instance FromJSON FollowerIdsResult
instance ToJSON   FollowerIdsResult

followerIds :: String -> IO (Either String FollowerIdsResult)
followerIds screenName = do
  req       <- parseRequest
              $ "https://api.twitter.com/1.1/followers/ids.json"
              ++ "?screen_name=" ++ screenName
  res       <- requestTwitterApi req
  return $ eitherDecode $ responseBody res
