module Reveille.Aggregator
  ( startAggregator
  ) where

import Reveille.Author (Author(authorName, authorUrl, authorFeed))
import Reveille.Authors (authors)
import Reveille.Database (Database, updateDatabase)
import Reveille.Item (Item(itemName, itemTime), toItem)
import Reveille.Name (fromName)
import Reveille.Unicode (toUtf8)
import Reveille.Url (fromUrl)

import qualified Control.Concurrent as Concurrent
import qualified Control.Concurrent.STM as Stm
import qualified Control.Exception as Exception
import qualified Control.Monad as Monad
import qualified Data.Foldable as Foldable
import qualified Data.Set as Set
import qualified Data.Time as Time
import qualified Data.Version as Version
import qualified Network.HTTP.Client as Client
import qualified Network.HTTP.Types as Http
import qualified Paths_reveille as This
import qualified Text.Feed.Import as Feed
import qualified Text.Feed.Query as Feed
import qualified Text.Printf as Printf

startAggregator :: Client.Manager -> Stm.TVar Database -> IO ()
startAggregator manager database = Monad.forever (do
  Foldable.for_ authors (\ author -> do
    Printf.printf "- %s <%s>\n"
      (fromName (authorName author))
      (fromUrl (authorUrl author))

    items <- Exception.catches
      (getAuthorItems manager author)
      [ Exception.Handler (\ exception -> do
        print (exception :: Exception.IOException)
        pure Set.empty)
      , Exception.Handler (\ exception -> do
        print (exception :: Client.HttpException)
        pure Set.empty)
      ]
    Stm.atomically (Stm.modifyTVar database (updateDatabase author items))

    Foldable.for_ items (\ item -> do
      Printf.printf "  - %s: %s\n"
        (rfc3339 (itemTime item))
        (fromName (itemName item))))

  sleep 3600)

sleep :: Int -> IO ()
sleep seconds = Concurrent.threadDelay (seconds * 1000000)

rfc3339 :: Time.UTCTime -> String
rfc3339 time = Time.formatTime Time.defaultTimeLocale "%Y-%m-%dT%H:%M:%S%QZ" time

getAuthorItems :: Client.Manager -> Author -> IO (Set.Set Item)
getAuthorItems manager author = do
  url <- case authorFeed author of
    Nothing -> fail "no feed"
    Just url -> pure url

  initialRequest <- Client.parseUrlThrow (fromUrl url)
  let
    version = Version.showVersion This.version
    userAgent = "reveille/" ++ version
    request = initialRequest
      { Client.requestHeaders =
        [ (Http.hUserAgent, toUtf8 userAgent)
        ]
      }
  response <- Client.httpLbs request manager

  feed <- case Feed.parseFeedSource (Client.responseBody response) of
    Nothing -> fail "invalid feed"
    Just feed -> pure feed

  items <- case mapM toItem (Feed.feedItems feed) of
    Left _ -> fail "invalid item"
    Right items -> pure items

  pure (Set.fromList items)
