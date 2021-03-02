{-# LANGUAGE DeriveGeneric #-}

module Twitter.Data.Users (Users (..)) where

import Data.Aeson
import Twitter.Data.User
import GHC.Generics

data Users = Users {
  users               :: [User],
  next_cursor         :: Integer,
  next_cursor_str     :: String,
  previous_cursor     :: Integer,
  previous_cursor_str :: String
} deriving (Show, Generic)

instance FromJSON Users
instance ToJSON   Users
