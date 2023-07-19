module NewsAPI where

import App          (App(..), AppConfig(..), Cachekey)
import APIData      (Response(..))

import Data.Aeson   (decode)
import Network.HTTP.Client (newManager, parseRequest, httpLbs, responseBody, requestHeaders, Manager)
import Network.HTTP.Client.TLS (tlsManagerSettings)
import qualified Data.ByteString.Lazy  as LZB
import Data.Text    (unpack, pack)
import Web.HttpApiData  (FromHttpApiData(..), ToHttpApiData(..))
import Network.URI.Encode   (encode)
import Data.Cache   (insert, lookup, fetchWithCache) 
import qualified Data.Cache   as C


type PageNumber = Int
type PageSize = Int


fetchTopNewsHeadlines :: Maybe PageNumber -> Maybe PageSize -> App Response
fetchTopNewsHeadlines pn ps = do
    key <- asks unNewsAPIKey
    fetch . topNewsUrl $ key
    where 
        topNewsUrl k
            = "https://newsapi.org/v2/top-headlines?country=us"
                <> "&pageSize=" <> maybe "10" show ps
                <> "&page=" <> maybe "1" show pn 
                <> "&apiKey=" <> unpack k


clientManager :: App Manager
clientManager = liftIO . newManager $ tlsManagerSettings


type URL = String

fetch :: URL -> App Response
fetch u = do
    cache <- asks unCache
    fetchWithCache cache (pack u) (\_ -> do
        manager <- clientManager
        request <- liftIO . parseRequest $ u
        let userAgentHeader = ("User-Agent", "NewsAPI/1.0")
            request1 = request { requestHeaders = userAgentHeader : requestHeaders request }
        response <- liftIO $ httpLbs request1 manager
        let jsonBody = responseBody response :: LZB.ByteString
        return . fromMaybe (error ("error with api" <> show jsonBody)) . decode $ jsonBody)


type Query = String


data QueryBy
  = QueryByTitle
  | QueryByDescription
  | QueryByContent
  deriving stock (Generic, Show)


instance FromHttpApiData QueryBy where
    parseQueryParam param =
        case param of
            "description" -> Right QueryByDescription
            "content" -> Right QueryByContent
            _         -> Right QueryByTitle


instance ToHttpApiData QueryBy where
  toQueryParam query =
    case query of
      QueryByTitle -> "title"
      QueryByDescription -> "description"
      QueryByContent -> "content"


fetchSearchedArticles :: QueryBy -> Query -> Maybe PageNumber -> Maybe PageSize  -> App Response
fetchSearchedArticles qb q pn ps = do
    k <- asks unNewsAPIKey
    fetch . searchUrl $ k
    where 
        searchUrl k
            = "https://newsapi.org/v2/everything?"
                <> toQueryParamPair ("searchIn", unpack . toQueryParam $ qb)
                <> toQueryParamPair ("q", q)
                <> "&pageSize=" <> maybe "10" show ps
                <> "&page=" <> maybe "1" show pn 
                <> "&apiKey=" <> unpack k
        toQueryParamPair :: (Text, String) -> String
        toQueryParamPair (n, value) = "&" <> unpack n <> "=" <> encode value




