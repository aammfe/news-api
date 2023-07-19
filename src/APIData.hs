module APIData where

import Data.Aeson   (ToJSON, FromJSON)

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
