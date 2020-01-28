{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DuplicateRecordFields #-}

module Twitter.Data.Tweet (Tweet (..)) where

import Data.Text
import Data.Aeson
import GHC.Generics
import Twitter.Data.User

data Tweet = Tweet {
  created_at      :: String,
  favorited       :: Bool,
  favorite_count  :: Int,
  id              :: Integer,
  id_str          :: String,
  is_quote_status :: Bool,
  retweeted       :: Bool,
  retweet_count   :: Int,
  text            :: Text,
  user            :: User
} deriving (Show, Generic)

instance FromJSON Tweet
instance ToJSON   Tweet
