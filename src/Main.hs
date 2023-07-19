module Main where


import App
import API
import AppOptions

import Data.Text (pack)
import Network.Wai.Handler.Warp (run)
import Data.Cache (newCache,Cache, purgeExpired)
import Control.Concurrent (forkIO, threadDelay)
import System.Clock (TimeSpec)



tenMinutes :: TimeSpec
tenMinutes = 10 * 60 * 100000000000


main :: IO ()
main = do
  c <- newCache (Just tenMinutes)
  void . forkIO . cleanupCache $ c --to remove cache

  key <- readNewsApiKey
  let config = AppConfig (pack key) c 
  putStrLn "app is running on port:8080"
  run 8080 (app config)




cleanupCache :: Hashable k => Cache k v -> IO ()
cleanupCache c = do
  threadDelay (60 * 1000000) -- Delay for cleanup (every 60 seconds)
  purgeExpired c
  cleanupCache c


