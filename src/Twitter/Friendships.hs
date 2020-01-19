{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE IncoherentInstances #-}

module Twitter.Friendships (Relationship (..)
                            , follow
                            , unFollow
                            , Twitter.Friendships.lookup
                            ) where

import qualified Data.ByteString.Char8 as B8
import qualified Data.Text             as T
import Data.List                       as DataList
import Data.Aeson
import GHC.Generics
import Network.HTTP.Conduit
import Twitter.TwSettings
import Twitter.Data.User
import Prelude hiding (lookup)

data Relationship = Relationship {
  name        :: T.Text,
  screen_name :: String,
  id          :: Integer,
  connections :: [String]
} deriving (Show, Generic)
instance FromJSON Relationship
instance ToJSON   Relationship

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

class Lookup a where
  lookup :: a -> IO (Either String [Relationship])

instance (Num a, Show a) => Lookup [a] where
  lookup userIds = do
    let userIdsStr = (init . tail . show) userIds
    req <- parseRequest
        $ "https://api.twitter.com/1.1/friendships/lookup.json"
        ++ "?user_id=" ++ userIdsStr 
    res <- requestTwitterApi req
    return $ eitherDecode $ responseBody res

instance Lookup [String] where
  lookup screenNames = do
    let screenNamesStr = DataList.intercalate "," screenNames
    req <- parseRequest
        $ "https://api.twitter.com/1.1/friendships/lookup.json"
        ++ "?screen_name=" ++ screenNamesStr
    res <- requestTwitterApi req
    return $ eitherDecode $ responseBody res
