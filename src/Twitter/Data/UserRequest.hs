{-# LANGUAGE DeriveGeneric #-}

module Twitter.Data.UserRequest (UserRequest (screenName, count), makeUserRequest) where

import Data.Aeson
import GHC.Generics

data UserRequest = UserRequest {
  screenName :: String,
  count      :: Int
} deriving (Show, Generic)

makeUserRequest :: String -> Int -> UserRequest
makeUserRequest screenName count
  | count < 1 || 200 < count = error "count range: 1 <= count <= 200"
  | otherwise                = UserRequest { screenName = screenName, count = count }

instance FromJSON UserRequest
instance ToJSON   UserRequest
