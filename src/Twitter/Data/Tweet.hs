{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DuplicateRecordFields #-}

module Twitter.Data.Tweet (Tweet (..)) where

import Data.Text
import Data.Aeson
import GHC.Generics

data Tweet = Tweet {
  id     :: Integer,
  text   :: Text
} deriving (Show, Generic)

instance FromJSON Tweet
instance ToJSON   Tweet
