module Reveille.Internal.Aggregator where

import qualified Control.Concurrent as Concurrent
import qualified Control.Concurrent.STM as Stm
import qualified Control.Exception as Exception
import qualified Control.Monad as Monad
import qualified Data.ByteString.Lazy as LazyBytes
import qualified Data.Set as Set
import qualified Network.HTTP.Client as Client
import qualified Network.HTTP.Types as Http
import qualified Reveille.Internal.Author as Reveille
import qualified Reveille.Internal.Database as Reveille
import qualified Reveille.Internal.Entry as Reveille
import qualified Reveille.Internal.Item as Reveille
import qualified Reveille.Internal.Unicode as Reveille
import qualified Reveille.Internal.Url as Reveille
import qualified Reveille.Internal.Version as Reveille
import qualified System.IO as IO
import qualified Text.Feed.Import as Feed
import qualified Text.Feed.Query as Feed
import qualified Text.Feed.Types as Feed

startAggregator :: Client.Manager -> Stm.TVar Reveille.Database -> IO a
startAggregator manager database = Monad.forever
  (do
    runAggregator manager database
    Concurrent.threadDelay (15 * 60 * 1000000)
  )

runAggregator :: Client.Manager -> Stm.TVar Reveille.Database -> IO ()
runAggregator manager database = do
  db <- Stm.readTVarIO database
  mapM_
    (\author -> Exception.catch
      (do
        entries <- fetchAuthorEntries manager author
        Stm.atomically
          (Stm.modifyTVar database (Reveille.addDatabaseEntries entries))
      )
      (\exception -> do
        IO.hPutStrLn
          IO.stderr
          (Exception.displayException (exception :: Exception.SomeException))
      )
    )
    (Reveille.getDatabaseAuthors db)

fetchAuthorEntries
  :: Client.Manager -> Reveille.Author -> IO (Set.Set Reveille.Entry)
fetchAuthorEntries manager author = do
  url <- fromRight (getAuthorFeed author)
  request <- fromRight (parseUrl url)
  response <- Client.httpLbs request manager
  feed <- fromRight (parseFeed (Client.responseBody response))
  items <- fromRight (parseItems (Feed.feedItems feed))
  pure (Set.fromList (map (Reveille.Entry author) items))

fromRight :: (Show l, Monad m) => Either l r -> m r
fromRight e = case e of
  Left l -> fail (show l)
  Right r -> pure r

data AggregatorError
  = AggregatorErrorNoFeed
  | AggregatorErrorInvalidUrl
  | AggregatorErrorInvalidFeed
  | AggregatorErrorInvalidItem
  deriving Show

getAuthorFeed :: Reveille.Author -> Either AggregatorError Reveille.Url
getAuthorFeed author = case Reveille.authorFeed author of
  Nothing -> Left AggregatorErrorNoFeed
  Just url -> Right url

parseUrl :: Reveille.Url -> Either AggregatorError Client.Request
parseUrl url = case Client.parseRequest (Reveille.fromUrl url) of
  Nothing -> Left AggregatorErrorInvalidUrl
  Just request -> Right request
    { Client.requestHeaders = [(Http.hUserAgent, Reveille.toUtf8 userAgent)]
    }

userAgent :: String
userAgent = "reveille-" ++ Reveille.versionString

parseFeed :: LazyBytes.ByteString -> Either AggregatorError Feed.Feed
parseFeed body = case Feed.parseFeedSource body of
  Nothing -> Left AggregatorErrorInvalidFeed
  Just feed -> Right feed

parseItems :: [Feed.Item] -> Either AggregatorError [Reveille.Item]
parseItems feedItems = mapM parseItem feedItems

parseItem :: Feed.Item -> Either AggregatorError Reveille.Item
parseItem feedItem = case Reveille.toItem feedItem of
  Left _ -> Left AggregatorErrorInvalidItem
  Right item -> Right item
