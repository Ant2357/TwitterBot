{-# LANGUAGE OverloadedStrings #-}

module Twitter.TwSettings (requestTwitterApi, loginName) where

import qualified Data.ByteString.Lazy.Internal as S
import Web.Authenticate.OAuth
import Network.HTTP.Conduit

loginName         = "ant2357"
consumerKey       = "XXXXXXX"
consumerSecret    = "XXXXXXX"
accessToken       = "XXXXXXX"
accessTokenSecret = "XXXXXXX"

twOAuth :: OAuth
twOAuth = newOAuth {
  oauthServerName     = "api.twitter.com",
  oauthConsumerKey    = consumerKey,
  oauthConsumerSecret = consumerSecret
}

twCredential :: Credential
twCredential = newCredential accessToken accessTokenSecret

requestTwitterApi :: Request -> IO (Response S.ByteString)
requestTwitterApi req = do
  signedReq <- signOAuth twOAuth twCredential req
  return   =<< httpLbs signedReq =<< (newManager tlsManagerSettings)
