module App where

type NewsAPIKey = Text

data AppConfig = AppConfig
  { newsAPIKey :: Text
  }

newtype App a = App { runApp :: ReaderT AppConfig IO a }
  deriving newtype (Functor, Applicative, Monad, MonadIO, MonadReader AppConfig)
