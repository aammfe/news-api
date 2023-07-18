module NewsAPI where

import App          (App(..), AppConfig(..), NewsAPIKey)

import Data.Aeson   (decode, ToJSON, FromJSON)
import Network.HTTP.Client (newManager, parseRequest, httpLbs, responseBody, requestHeaders, Manager)
import Network.HTTP.Client.TLS (tlsManagerSettings)
import qualified Data.ByteString.Lazy  as LZB
import Data.Text    (unpack)


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

clientManager :: App Manager
clientManager = liftIO . newManager $ tlsManagerSettings

fetchTopNewsArticles :: Maybe PageNumber -> Maybe PageSize -> App Response
fetchTopNewsArticles pn ps = do
    liftIO $ putStrLn "fetchTopNewsArticles"
    key <- asks newsAPIKey
    manager <- clientManager
    request <- liftIO . parseRequest . getTopNewsUrl key pn $ ps
    
    liftIO $ putStrLn "fetchTopNewsArticles1"

    let userAgentHeader = ("User-Agent", "NewsAPI/1.0")
        request1 = request { requestHeaders = userAgentHeader : requestHeaders request }
    
    response <- liftIO $ httpLbs request1 manager
    liftIO $ putStrLn "fetchTopNewsArticles2"

    let jsonBody = responseBody response :: LZB.ByteString
    return . fromMaybe (error ("error with api" <> show jsonBody)) . decode $ jsonBody



type URL = String
getTopNewsUrl :: NewsAPIKey -> Maybe PageNumber -> Maybe PageSize -> URL
getTopNewsUrl k pn ps
    = "https://newsapi.org/v2/top-headlines?country=us&pageSize=" <> maybe "10" show ps
        <> "&page=" <> maybe "1" show pn 
        <> "&apiKey=" <> unpack k



--  fetching N news articles
--   finding a news article with a specific title or author, and searching by keywords



