module Reveille.Internal.Aggregator where

import qualified Control.Concurrent as Concurrent
import qualified Control.Concurrent.STM as Stm
import qualified Control.Exception as Exception
import qualified Control.Monad as Monad
import qualified Control.Monad.Trans.Class as Trans
import qualified Control.Monad.Trans.Except as Except
import qualified Data.ByteString.Lazy as LazyBytes
import qualified Data.Map as Map
import qualified Data.Ratio as Ratio
import qualified Data.Set as Set
import qualified Data.Void as Void
import qualified Network.HTTP.Client as Client
import qualified Network.HTTP.Types as Http
import qualified Reveille.Internal.Author as Author
import qualified Reveille.Internal.Database as Database
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
  () <- plugSpaceLeak database
  Concurrent.threadDelay (15 * 60 * 1000000)

plugSpaceLeak :: Stm.TVar Database.Database -> IO ()
plugSpaceLeak database = do
  db <- Stm.readTVarIO database
  pure (seqDatabase db ())

seqDatabase :: Database.Database -> unit -> unit
seqDatabase database unit = seqEntries (Database.unwrapDatabase database) unit

seqEntries :: Map.Map Author.Author (Set.Set Item.Item) -> unit -> unit
seqEntries entries unit = Map.foldrWithKey
  (\author items x -> seqAuthor author (seqItems items x))
  unit
  entries

seqAuthor :: Author.Author -> unit -> unit
seqAuthor author unit = seq
  (Author.authorName author)
  (seq (Author.authorUrl author) (seqMaybe (Author.authorFeed author) unit))

seqMaybe :: Maybe a -> unit -> unit
seqMaybe m unit = case m of
  Nothing -> unit
  Just x -> seq x unit

seqItems :: Set.Set Item.Item -> unit -> unit
seqItems items unit = Set.foldr seqItem unit items

seqItem :: Item.Item -> unit -> unit
seqItem item unit = seq
  (Item.itemName item)
  (seq (Item.itemUrl item) (seq (Item.itemTime item) unit))

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
    Right itemResults -> mapM_
      (\itemResult -> case itemResult of
        Left itemError -> Printf.printf
          "%s: %s\n"
          (Name.fromName (Author.authorName author))
          (show itemError)
        Right item -> Stm.atomically
          (Stm.modifyTVar
            database
            (Database.addDatabaseItems author (Set.singleton item))
          )
      )
      itemResults

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
  -> m (Either AggregatorError [Either Item.ItemError Item.Item])
fetchAuthorEntries performRequest author =
  Except.runExceptT (fetchAuthorEntriesT performRequest author)

fetchAuthorEntriesT
  :: Monad m
  => (Client.Request -> m (Either Client.HttpException Response))
  -> Author.Author
  -> Except.ExceptT
       AggregatorError
       m
       [Either Item.ItemError Item.Item]
fetchAuthorEntriesT performRequest author = do
  url <- toExceptT (getAuthorFeed author)
  request <- toExceptT (parseUrl url)
  result <- Trans.lift (performRequest request)
  response <- either
    (\exception -> Except.throwE (AggregatorErrorHttpException exception))
    pure
    result
  feed <- toExceptT (parseFeed (Client.responseBody response))
  pure (parseItems url (Feed.feedItems feed))

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

parseItems :: Url.Url -> [Feed.Item] -> [Either Item.ItemError Item.Item]
parseItems feedUrl feedItems = map (parseItem feedUrl) feedItems

parseItem :: Url.Url -> Feed.Item -> Either Item.ItemError Item.Item
parseItem feedUrl feedItem = case Item.toItem feedUrl feedItem of
  Left itemError -> Left itemError
  Right item -> Right item
