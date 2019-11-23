{-# LANGUAGE OverloadedStrings #-}

module Twitter.Favorites (fav) where

import qualified Data.Text as T
import Data.Text.Encoding
import Network.HTTP.Conduit
import Web.Authenticate.OAuth
import TwSettings

fav :: String -> IO ()
fav twId = do
  req         <- parseRequest "https://api.twitter.com/1.1/favorites/create.json"
  manager     <- newManager tlsManagerSettings
  let postReq  = urlEncodedBody [("id", encodeUtf8 (T.pack twId))] req
  signedReq   <- signOAuth twOAuth twCredential postReq
  httpLbs signedReq manager
  return ()
