
module Twitter.Friends (followIds, followList) where

import Data.Aeson
import Network.HTTP.Conduit
import Twitter.TwSettings
import Twitter.Data.IdsInfo
import Twitter.Data.UserRequest
import Twitter.Data.UsersInfo

followIds :: String -> IO (Either String IdsInfo)
followIds screenName = do
  req <- parseRequest
          $ "https://api.twitter.com/1.1/friends/ids.json"
          ++ "?screen_name=" ++ screenName
  res <- requestTwitterApi req
  return $ eitherDecode $ responseBody res

followList :: UserRequest -> IO (Either String UsersInfo)
followList uRequest = do
  req <- parseRequest
          $ "https://api.twitter.com/1.1/friends/list.json"
          ++ "?screen_name=" ++ screenName uRequest
          ++ "&count=" ++ show (count uRequest)
  res <- requestTwitterApi req
  return $ eitherDecode $ responseBody res
