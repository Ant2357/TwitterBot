{-# LANGUAGE DeriveGeneric #-}

module Twitter.Data.UsersInfo (UsersInfo (..)) where

import Data.Aeson
import Twitter.Data.User
import GHC.Generics

data UsersInfo = UsersInfo {
  users               :: [User],
  next_cursor         :: Integer,
  next_cursor_str     :: String,
  previous_cursor     :: Integer,
  previous_cursor_str :: String
} deriving (Show, Generic)

instance FromJSON UsersInfo
instance ToJSON   UsersInfo
