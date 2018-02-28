module Reveille.Internal.Application where

import qualified Control.Concurrent.STM as Stm
import qualified Data.Either as Either
import qualified Data.Text as Text
import qualified Data.Time as Time
import qualified Network.Wai as Wai
import qualified Reveille.Internal.Database as Database
import qualified Reveille.Internal.Handler as Handler
import qualified Reveille.Internal.Unicode as Unicode

application :: Stm.TVar Database.Database -> Wai.Application
application database request respond = do
  db <- Stm.readTVarIO database
  now <- Time.getCurrentTime
  let response = routeRequest request db now
  respond response

routeRequest
  :: Wai.Request -> Database.Database -> Time.UTCTime -> Wai.Response
routeRequest request database now =
  case (requestMethod request, requestPath request) of
    ("GET", []) -> Handler.getIndexHandler database now
    ("GET", ["authors.opml"]) -> Handler.getAuthorsHandler database
    ("GET", ["favicon.ico"]) -> Handler.getFaviconHandler
    ("GET", ["feed.atom"]) -> Handler.getFeedHandler database now
    ("GET", ["health-check.xml"]) -> Handler.getHealthCheckHandler
    ("GET", ["robots.txt"]) -> Handler.getRobotsHandler
    _ -> Handler.defaultHandler

requestMethod :: Wai.Request -> String
requestMethod request =
  Either.fromRight "?" (Unicode.fromUtf8 (Wai.requestMethod request))

requestPath :: Wai.Request -> [String]
requestPath request = map Text.unpack (Wai.pathInfo request)
