{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveGeneric #-}

module Twitter.DirectMessages (dm) where

import Data.Text
import Data.Aeson
import Network.HTTP.Conduit
import Network.HTTP.Simple

import Twitter.TwSettings
import Twitter.Data.Dm

dm :: Text -> Text -> IO (Either String PostDM)
dm recipientId msg = do
  let json = PostDM {
    dm_event = PostEvent {
      dm_type = "message_create",
      dm_message_create = PostMessageCreate {
        dm_message_data = PostMessageData {
          dm_text = msg
        },
        dm_target = PostTarget {
          dm_recipient_id = recipientId
        }
      }
    }
  }

  req         <- (\n -> n {method = "POST"}) <$> parseRequest "https://api.twitter.com/1.1/direct_messages/events/new.json"
  let postReq  = setRequestBodyJSON json req
  res         <- requestTwitterApi postReq
  return $ eitherDecode $ responseBody res
