{-# LANGUAGE DeriveGeneric #-}

module Twitter.Data.Ids (Ids (..)) where

import Data.Aeson
import GHC.Generics

data Ids = Ids {
  ids                 :: [Integer],
  next_cursor         :: Integer,
  next_cursor_str     :: String,
  previous_cursor     :: Integer,
  previous_cursor_str :: String
} deriving (Show, Generic)

instance FromJSON Ids
instance ToJSON   Ids
