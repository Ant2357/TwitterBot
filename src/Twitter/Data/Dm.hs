{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE OverloadedLists #-}

module Twitter.Data.Dm ( PostDM (..)
                        , PostTarget (..)
                        , PostMessageData (..)
                        , PostMessageCreate (..)
                        , PostEvent (..)
                        ) where

import Data.Text
import Data.Aeson
import Data.Aeson.TH

-- PostTarget
data PostTarget = PostTarget {
  dm_recipient_id :: Text
} deriving (Show)
$(deriveJSON defaultOptions { fieldLabelModifier = Prelude.drop 3 } ''PostTarget)

-- PostMessageData
data PostMessageData = PostMessageData {
  dm_text :: Text
} deriving (Show)
$(deriveJSON defaultOptions { fieldLabelModifier = Prelude.drop 3 } ''PostMessageData)

-- PostMessageCreate
data PostMessageCreate = PostMessageCreate {
  dm_message_data :: PostMessageData,
  dm_target :: PostTarget
} deriving (Show)
$(deriveJSON defaultOptions { fieldLabelModifier = Prelude.drop 3 } ''PostMessageCreate)

-- PostEvent
data PostEvent = PostEvent {
  dm_type :: Text,
  dm_message_create :: PostMessageCreate
} deriving (Show)
$(deriveJSON defaultOptions { fieldLabelModifier = Prelude.drop 3 } ''PostEvent)

-- PostDM
data PostDM = PostDM {
  dm_event :: PostEvent
} deriving (Show)
$(deriveJSON defaultOptions { fieldLabelModifier = Prelude.drop 3 } ''PostDM)
