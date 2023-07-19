module NewsAPI where

import App          (App(..), AppConfig(..))

import Data.Aeson   (decode, ToJSON, FromJSON)
import Network.HTTP.Client (newManager, parseRequest, httpLbs, responseBody, requestHeaders, Manager)
import Network.HTTP.Client.TLS (tlsManagerSettings)
import qualified Data.ByteString.Lazy  as LZB
import Data.Text    (unpack)
import Web.HttpApiData  (FromHttpApiData(..), ToHttpApiData(..))
import Network.URI.Encode   (encode)


data Response = Response  
    { status :: Text
    , totalResults :: Int
    , articles :: [Article]
    } deriving stock (Eq, Show, Generic)


instance ToJSON Response
instance FromJSON Response


data Source = Source
    { id :: Maybe Text
    , name :: Maybe  Text
    } deriving stock (Eq, Show, Generic)

instance ToJSON Source
instance FromJSON Source


data Article = Article
    { source :: Maybe Source
    , author :: Maybe Text
    , title :: Maybe Text
    , description :: Maybe Text
    , url :: Maybe Text
    , urlToImage :: Maybe Text
    , publishedAt :: Maybe Text
    , content :: Maybe Text
    } deriving stock (Eq, Show, Generic)


instance ToJSON Article
instance FromJSON Article


type PageNumber = Int
type PageSize = Int


fetchTopNewsArticles :: Maybe PageNumber -> Maybe PageSize -> App Response
fetchTopNewsArticles pn ps = do
    key <- asks newsAPIKey
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

fetch :: forall a .FromJSON a => URL -> App a
fetch u = do
    manager <- clientManager
    request <- liftIO . parseRequest $ u
    let userAgentHeader = ("User-Agent", "NewsAPI/1.0")
        request1 = request { requestHeaders = userAgentHeader : requestHeaders request }
    response <- liftIO $ httpLbs request1 manager
    let jsonBody = responseBody response :: LZB.ByteString
    return . fromMaybe (error ("error with api" <> show jsonBody)) . decode $ jsonBody


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
    k <- asks newsAPIKey
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




