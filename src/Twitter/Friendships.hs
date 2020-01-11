{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE IncoherentInstances #-}

module Twitter.Friendships (Relationship (..)
                            , follow
                            , unFollow
                            , lookup
                            ) where

import qualified Data.ByteString.Char8 as B8
import Data.Text
import Data.Aeson
import GHC.Generics
import Network.HTTP.Conduit
import Twitter.TwSettings
import Twitter.Data.User
import Prelude hiding (lookup)

data Relationship = Relationship {
  name        :: Text,
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

instance (Num a, Show a, Eq a) => Lookup a where
  lookup userId = do
    req <- parseRequest
            $ "https://api.twitter.com/1.1/friendships/lookup.json"
            ++ "?user_id=" ++ show userId 
    res <- requestTwitterApi req
    return $ eitherDecode $ responseBody res

instance Lookup String where
  lookup screenName = do
    req <- parseRequest
            $ "https://api.twitter.com/1.1/friendships/lookup.json"
            ++ "?screen_name=" ++ screenName
    res <- requestTwitterApi req
    return $ eitherDecode $ responseBody res
