{-# LANGUAGE OverloadedStrings #-}

module Twitter (tweet, fav) where

import Data.Text
import Data.Text.Encoding
import Network.HTTP.Conduit
import Web.Authenticate.OAuth
import TwSettings

tweet :: Text -> IO ()
tweet tw = do
  req     <- parseRequest "https://api.twitter.com/1.1/statuses/update.json"
  manager <- newManager tlsManagerSettings
  let postReq = urlEncodedBody [("status", encodeUtf8 tw)] req
  signedReq <- signOAuth twOAuth twCredential postReq
  httpLbs signedReq manager
  return ()

fav :: Text -> IO ()
fav twId = do
  req     <- parseRequest "https://api.twitter.com/1.1/favorites/create.json"
  manager <- newManager tlsManagerSettings
  let postReq = urlEncodedBody [("id", encodeUtf8 twId)] req
  signedReq <- signOAuth twOAuth twCredential postReq
  httpLbs signedReq manager
  return ()
