module Reveille.Internal.Aggregator where

import qualified Control.Concurrent as Concurrent
import qualified Control.Concurrent.STM as Stm
import qualified Control.Exception as Exception
import qualified Control.Monad as Monad
import qualified Control.Monad.Trans.Class as Trans
import qualified Control.Monad.Trans.Except as Except
import qualified Data.ByteString.Lazy as LazyBytes
import qualified Data.Ratio as Ratio
import qualified Data.Set as Set
import qualified Data.Void as Void
import qualified Network.HTTP.Client as Client
import qualified Network.HTTP.Types as Http
import qualified Reveille.Internal.Author as Author
import qualified Reveille.Internal.Database as Database
import qualified Reveille.Internal.Entry as Entry
import qualified Reveille.Internal.Item as Item
import qualified Reveille.Internal.Name as Name
import qualified Reveille.Internal.Unicode as Unicode
import qualified Reveille.Internal.Url as Url
import qualified Reveille.Internal.Version as Version
import qualified System.Clock as Clock
import qualified Text.Feed.Import as Feed
import qualified Text.Feed.Query as Feed
import qualified Text.Feed.Types as Feed
import qualified Text.Printf as Printf

startAggregator :: Client.Manager -> Stm.TVar Database.Database -> IO Void.Void
startAggregator manager database =
  Monad.forever (runAggregator manager database)

runAggregator :: Client.Manager -> Stm.TVar Database.Database -> IO ()
runAggregator manager database = do
  updateAuthors manager database
  Concurrent.threadDelay (15 * 60 * 1000000)

updateAuthors :: Client.Manager -> Stm.TVar Database.Database -> IO ()
updateAuthors manager database = do
  putStrLn "Updating feeds ..."
  db <- Stm.readTVarIO database
  let authors = Database.getDatabaseAuthors db
  ((), elapsed) <- timed (mapM_ (updateAuthor manager database) authors)
  Printf.printf
    "Updated %d feed%s in %.1f seconds.\n"
    (Set.size authors)
    (pluralize authors)
    (timeSpecToDouble elapsed)

timed :: IO a -> IO (a, Clock.TimeSpec)
timed action = do
  start <- Clock.getTime Clock.Monotonic
  result <- action
  stop <- Clock.getTime Clock.Monotonic
  let elapsed = Clock.diffTimeSpec stop start
  pure (result, elapsed)

pluralize :: Set.Set a -> String
pluralize xs = if Set.size xs == 1 then "" else "s"

timeSpecToDouble :: Clock.TimeSpec -> Double
timeSpecToDouble timeSpec =
  rationalToDouble (fromNanos (Clock.toNanoSecs timeSpec))

fromNanos :: Integer -> Rational
fromNanos nanos = nanos Ratio.% 1000000000

rationalToDouble :: Rational -> Double
rationalToDouble rational = fromRational rational

updateAuthor
  :: Client.Manager -> Stm.TVar Database.Database -> Author.Author -> IO ()
updateAuthor manager database author = do
  result <- fetchAuthorEntries (safeHttpLbs manager) author
  case result of
    Left aggregatorError -> Printf.printf
      "%s: %s\n"
      (Name.fromName (Author.authorName author))
      (show aggregatorError)
    Right entries -> Stm.atomically
      (Stm.modifyTVar database (Database.addDatabaseEntries entries))

type Response = Client.Response LazyBytes.ByteString

safeHttpLbs
  :: Client.Manager
  -> Client.Request
  -> IO (Either Client.HttpException Response)
safeHttpLbs manager request = Exception.catch
  (do
    response <- Client.httpLbs request manager
    pure (Right response)
  )
  (\exception -> pure (Left exception))

fetchAuthorEntries
  :: Monad m
  => (Client.Request -> m (Either Client.HttpException Response))
  -> Author.Author
  -> m (Either AggregatorError (Set.Set Entry.Entry))
fetchAuthorEntries performRequest author =
  Except.runExceptT (fetchAuthorEntriesT performRequest author)

fetchAuthorEntriesT
  :: Monad m
  => (Client.Request -> m (Either Client.HttpException Response))
  -> Author.Author
  -> Except.ExceptT AggregatorError m (Set.Set Entry.Entry)
fetchAuthorEntriesT performRequest author = do
  url <- toExceptT (getAuthorFeed author)
  request <- toExceptT (parseUrl url)
  result <- Trans.lift (performRequest request)
  response <- either
    (\exception -> Except.throwE (AggregatorErrorHttpException exception))
    pure
    result
  feed <- toExceptT (parseFeed (Client.responseBody response))
  items <- toExceptT (parseItems (Feed.feedItems feed))
  pure (Set.fromList (Entry.toEntries author items))

toExceptT :: Monad m => Either e a -> Except.ExceptT e m a
toExceptT = either Except.throwE pure

fromRight :: (Show l, Monad m) => Either l r -> m r
fromRight e = case e of
  Left l -> fail (show l)
  Right r -> pure r

data AggregatorError
  = AggregatorErrorNoFeed
  | AggregatorErrorInvalidUrl Url.Url
  | AggregatorErrorHttpException Client.HttpException
  | AggregatorErrorInvalidFeed LazyBytes.ByteString
  | AggregatorErrorInvalidItem Item.ItemError
  deriving Show

getAuthorFeed :: Author.Author -> Either AggregatorError Url.Url
getAuthorFeed author = case Author.authorFeed author of
  Nothing -> Left AggregatorErrorNoFeed
  Just url -> Right url

parseUrl :: Url.Url -> Either AggregatorError Client.Request
parseUrl url = case Client.parseRequest (Url.fromUrl url) of
  Nothing -> Left (AggregatorErrorInvalidUrl url)
  Just request -> Right request
    { Client.requestHeaders = [(Http.hUserAgent, Unicode.toUtf8 userAgent)]
    }

userAgent :: String
userAgent = "reveille-" ++ Version.versionString

parseFeed :: LazyBytes.ByteString -> Either AggregatorError Feed.Feed
parseFeed body = case Feed.parseFeedSource body of
  Nothing -> Left (AggregatorErrorInvalidFeed body)
  Just feed -> Right feed

parseItems :: [Feed.Item] -> Either AggregatorError [Item.Item]
parseItems feedItems = mapM parseItem feedItems

parseItem :: Feed.Item -> Either AggregatorError Item.Item
parseItem feedItem = case Item.toItem feedItem of
  Left itemError -> Left (AggregatorErrorInvalidItem itemError)
  Right item -> Right item
