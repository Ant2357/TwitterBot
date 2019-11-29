
module Twitter.TwSettings where

import qualified Data.ByteString.Char8 as S8
import Web.Authenticate.OAuth

consumerKey       = "XXXXXXX"
consumerSecret    = "XXXXXXX"
accessToken       = "XXXXXXX"
accessTokenSecret = "XXXXXXX"

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
