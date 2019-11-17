{-# LANGUAGE DeriveGeneric #-}

module TLRequest (TLRequest (..)) where

import GHC.Generics

data TLRequest = TLRequest {
  twScreenName     :: String,
  twCount          :: Int,
  twExcludeReplies :: Bool,
  twIncludeRts     :: Bool
} deriving (Show, Generic)
