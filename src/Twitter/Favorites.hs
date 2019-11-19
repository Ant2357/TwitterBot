{-# LANGUAGE OverloadedStrings #-}

module Twitter.Favorites (fav) where

import Data.Text
import Data.Text.Encoding
import Network.HTTP.Conduit
import Web.Authenticate.OAuth
import TwSettings

fav :: Text -> IO ()
fav twId = do
  req         <- parseRequest "https://api.twitter.com/1.1/favorites/create.json"
  manager     <- newManager tlsManagerSettings
  let postReq  = urlEncodedBody [("id", encodeUtf8 twId)] req
  signedReq   <- signOAuth twOAuth twCredential postReq
  httpLbs signedReq manager
  return ()
