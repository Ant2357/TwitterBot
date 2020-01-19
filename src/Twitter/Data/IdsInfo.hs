{-# LANGUAGE DeriveGeneric #-}

module Twitter.Data.IdsInfo (IdsInfo (..)) where

import Data.Aeson
import GHC.Generics

data IdsInfo = IdsInfo {
  ids                 :: [Integer],
  next_cursor         :: Integer,
  next_cursor_str     :: String,
  previous_cursor     :: Integer,
  previous_cursor_str :: String
} deriving (Show, Generic)

instance FromJSON IdsInfo
instance ToJSON   IdsInfo
