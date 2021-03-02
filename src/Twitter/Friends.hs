{-# LANGUAGE DeriveGeneric #-}

module Twitter.Friends (  RequestFollowList
                        , makeRequestFollowList
                        , followIds
                        , followList
                        ) where

import Data.Aeson
import GHC.Generics
import Network.HTTP.Conduit
import Twitter.TwSettings
import Twitter.Data.Ids
import Twitter.Data.Users

followIds :: String -> IO (Either String Ids)
followIds screenName = do
  req <- parseRequest
      $ "https://api.twitter.com/1.1/friends/ids.json"
      ++ "?screen_name=" ++ screenName
  res <- requestTwitterApi req
  return $ eitherDecode $ responseBody res

data RequestFollowList = RequestFollowList {
  screenName :: String,
  count      :: Int
} deriving (Show, Generic)

makeRequestFollowList :: String -> Int -> RequestFollowList
makeRequestFollowList screenName count
  | count < 1 || 200 < count = error "count range: 1 <= count <= 200"
  | otherwise                = RequestFollowList { screenName = screenName, count = count }

followList :: RequestFollowList -> IO (Either String Users)
followList fRequest = do
  req <- parseRequest
      $ "https://api.twitter.com/1.1/friends/list.json"
      ++ "?screen_name=" ++ screenName fRequest
      ++ "&count=" ++ show (count fRequest)
  res <- requestTwitterApi req
  return $ eitherDecode $ responseBody res
