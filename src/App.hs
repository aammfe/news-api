module App where

import APIData    (Response)
import Data.Cache (Cache)


type NewsAPIKey = Text
type Cachekey = Text

data AppConfig = AppConfig
  { unNewsAPIKey :: Text
  , unCache :: Cache Cachekey Response
  }

newtype App a = App { runApp :: ReaderT AppConfig IO a }
  deriving newtype (Functor, Applicative, Monad, MonadIO, MonadReader AppConfig)

