module API where

import App
import NewsAPI
import Servant
import APIData


type API = "top-headlines" :> 
                QueryParam "pageNumber" PageNumber :>
                QueryParam "pageSize" PageSize :>
                  Get '[JSON] Response
          :<|>
             "query" :> 
                QueryParam' '[Required] "queryBy" QueryBy :>
                QueryParam' '[Required] "query" Query :>
                QueryParam "pageNumber" PageNumber :>
                QueryParam "pageSize" PageSize :>
                  Get '[JSON] Response



appServer :: ServerT API App 
appServer = fetchTopNewsHeadlines :<|> fetchSearchedArticles

appToServer :: AppConfig -> App a -> Handler a
appToServer config ap = Handler $ do
  let readerAction = runReaderT (runApp ap) config
  liftIO readerAction


server :: AppConfig -> Server API
server conf = hoistServer api (appToServer conf) appServer 


api :: Proxy API
api = Proxy


app :: AppConfig -> Application
app confg = serve api (server confg)
