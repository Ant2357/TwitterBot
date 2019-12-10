{-# LANGUAGE OverloadedStrings #-}

module Twitter.Favorites (fav, unFav) where

import qualified Data.ByteString.Char8 as B8
import Data.Text.Encoding
import Data.Aeson
import Network.HTTP.Conduit
import Web.Authenticate.OAuth
import Twitter.TwSettings
import Twitter.Data.Tweet

fav :: Integer -> IO (Either String Tweet)
fav twId = do
  req         <- parseRequest "https://api.twitter.com/1.1/favorites/create.json"
  manager     <- newManager tlsManagerSettings
  let postReq  = urlEncodedBody [("id", (B8.pack . show) twId)] req
  signedReq   <- signOAuth twOAuth twCredential postReq
  res         <- httpLbs signedReq manager
  return $ eitherDecode $ responseBody res

unFav :: Integer -> IO (Either String Tweet)
unFav twId = do
  req         <- parseRequest "https://api.twitter.com/1.1/favorites/destroy.json"
  manager     <- newManager tlsManagerSettings
  let postReq  = urlEncodedBody [("id", (B8.pack . show) twId)] req
  signedReq   <- signOAuth twOAuth twCredential postReq
  res         <- httpLbs signedReq manager
  return $ eitherDecode $ responseBody res
