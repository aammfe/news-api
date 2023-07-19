module Main where


import App
import API
import AppOptions
import Data.Text (pack)
import Network.Wai.Handler.Warp (run)


main :: IO ()
main = do
  key <- readNewsApiKey
  putStrLn key
  putStrLn "app is running on port:8080"
  let config = AppConfig { newsAPIKey = pack key}
  run 8080 (app config)