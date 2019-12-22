{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveGeneric #-}

module Twitter.Search (SearchRequest, makeSearchRequest, SearchResult (..), search) where

import Data.Text
import Data.Text.Encoding
import Data.Aeson
import GHC.Generics
import Network.URI.Encode
import Network.HTTP.Conduit
import Web.Authenticate.OAuth
import Twitter.TwSettings
import Twitter.Data.Tweet

data SearchResult = SearchResult {
  statuses :: [Tweet]
} deriving (Show, Generic)
instance FromJSON SearchResult
instance ToJSON SearchResult

data SearchRequest = SearchRequest {
  searchQ     :: Text,
  resultType  :: String,
  searchCount :: Int
} deriving (Show, Generic)

makeSearchRequest :: Text -> String -> Int -> SearchRequest
makeSearchRequest searchQ resultType searchCount 
  | searchCount < 1 || 100 < searchCount = error "searchCount range: 1 <= searchCount <= 100"
  | resultType    /= "popular"
    && resultType /= "recent"
    && resultType /= "mixed"             = error "expected value resultType: \"popular\" or \"recent\" or \"mixed\""
  | otherwise                            = SearchRequest
    { searchQ     = searchQ
    , searchCount = searchCount
    , resultType  = resultType
    }

search :: SearchRequest -> IO (Either String SearchResult)
search sRequest = do
  req         <- parseRequest
                $ "https://api.twitter.com/1.1/search/tweets.json"
                ++ "?q=" ++ ((show . encodeText . searchQ) sRequest)
                ++ "&lang=ja"
                ++ "&locale=ja"
                ++ "&result_type=" ++ (resultType sRequest)
                ++ "&count=" ++ ((show . searchCount) sRequest)
  signedReq   <- signOAuth twOAuth twCredential req
  res         <- httpLbs signedReq =<< (newManager tlsManagerSettings)
  return $ eitherDecode $ responseBody res
