module AppOptions where

import Options.Applicative


newsApiKeyOption :: Parser String
newsApiKeyOption =
  strOption
    ( long "newsApiKey"
        <> metavar "NEWS_API_KEY"
        <> help "API key for accessing news API"
    )


readNewsApiKey :: IO String
readNewsApiKey = do
  envApiKey <- lookupEnv "NEWS_API_KEY"
  cliApiKey <- execParser newsApiKeyParser
  return $ fromMaybe "" envApiKey <|> cliApiKey
  where
    newsApiKeyParser = info (newsApiKeyOption <**> helper) mempty
