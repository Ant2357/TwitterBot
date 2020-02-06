{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DuplicateRecordFields #-}

module Twitter.Data.User (User (..)) where

import Data.Text
import Data.Aeson
import GHC.Generics

data User = User {
  id              :: Integer,
  name            :: Text,
  screen_name     :: String,
  friends_count   :: Integer,
  followers_count :: Integer,
  following       :: Bool,
  statuses_count  :: Integer
} deriving (Show, Eq, Ord, Generic)

instance FromJSON User
instance ToJSON   User
