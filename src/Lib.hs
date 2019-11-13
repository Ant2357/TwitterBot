{-# LANGUAGE OverloadedStrings #-}

module Lib (tweet, fav) where

import qualified Data.ByteString.Char8 as S8
import Data.Text
import Data.Text.Encoding
import Network.HTTP.Conduit
import Web.Authenticate.OAuth
import TwSettings

twOAuth :: OAuth
twOAuth = newOAuth {
  oauthServerName     = "api.twitter.com",
  oauthConsumerKey    = (S8.pack consumerKey),
  oauthConsumerSecret = (S8.pack consumerSecret)
}

twCredential :: Credential
twCredential = newCredential
  (S8.pack accessToken)
  (S8.pack accessTokenSecret)

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