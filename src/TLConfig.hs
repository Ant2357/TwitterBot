{-# LANGUAGE DeriveGeneric #-}

module TLConfig (TLConfig (..)) where

import GHC.Generics

data TLConfig = TLConfig {
  twScreenName     :: String,
  twCount          :: Int,
  twExcludeReplies :: Bool,
  twIncludeRts     :: Bool
} deriving (Show, Generic)
