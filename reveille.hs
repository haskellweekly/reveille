{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}

module Main
  ( main
  ) where

import qualified Conduit
import qualified Control.Concurrent as Concurrent
import qualified Control.Concurrent.Async as Async
import qualified Control.Exception as Exception
import qualified Control.Monad as Monad
import qualified Data.ByteString as Bytes
import qualified Data.ByteString.Builder as Builder
import qualified Data.ByteString.Lazy as LazyBytes
import qualified Data.Either as Either
import qualified Data.List as List
import qualified Data.Maybe as Maybe
import qualified Data.NonNull as NonNull
import qualified Data.Ord as Ord
import qualified Data.String as String
import qualified Data.Text as Text
import qualified Data.Text.Encoding as Text
import qualified Data.Time as Time
import qualified Data.Version as Version
import qualified Data.XML.Types as Xml
import qualified Database.SQLite.Simple as Sqlite
import qualified Lucid
import qualified Network.HTTP.Simple as Client
import qualified Network.HTTP.Types as Http
import qualified Network.Wai as Wai
import qualified Network.Wai.Handler.Warp as Warp
import qualified Paths_reveille as This
import qualified System.Environment as Environment
import qualified Text.Atom.Conduit.Parse as Atom
import qualified Text.Atom.Conduit.Render as Atom
import qualified Text.Atom.Types as Atom
import qualified Text.RSS.Conduit.Parse.Simple as Rss
import qualified Text.RSS.Types as Rss
import qualified Text.Read as Read
import qualified Text.XML.Stream.Parse as Xml
import qualified Text.XML.Stream.Render as Xml
import qualified URI.ByteString as Uri

main :: IO ()
main = do
  settings <- getSettings
  database <- getDatabase
  Sqlite.withConnection database (defaultMain settings)

getDatabase :: IO String
getDatabase = do
  maybeDatabase <- Environment.lookupEnv "DATABASE"
  pure (Maybe.fromMaybe ":memory:" maybeDatabase)

getSettings :: IO Warp.Settings
getSettings = do
  host <- getHost
  port <- getPort
  pure
    (Warp.setHost
       host
       (Warp.setPort
          port
          (Warp.setBeforeMainLoop
             (beforeMainLoop host port)
             (Warp.setLogger
                logger
                (Warp.setOnExceptionResponse
                   onExceptionResponse
                   (Warp.setServerName Bytes.empty Warp.defaultSettings))))))

getHost :: IO Warp.HostPreference
getHost = do
  maybeRawHost <- Environment.lookupEnv "HOST"
  pure (String.fromString (Maybe.fromMaybe "127.0.0.1" maybeRawHost))

getPort :: IO Warp.Port
getPort = do
  maybeRawPort <- Environment.lookupEnv "PORT"
  pure (Maybe.fromMaybe 8080 (Read.readMaybe (Maybe.fromMaybe "" maybeRawPort)))

beforeMainLoop :: Warp.HostPreference -> Warp.Port -> IO ()
beforeMainLoop host port =
  putStrLn (concat ["Listening on ", show host, " port ", show port, " ..."])

logger :: Wai.Request -> Http.Status -> Maybe Integer -> IO ()
logger request status _ =
  putStrLn
    (concat
       [ Either.fromRight "?" (fromUtf8 (Wai.requestMethod request))
       , " "
       , Either.fromRight "?" (fromUtf8 (Wai.rawPathInfo request))
       , Either.fromRight "?" (fromUtf8 (Wai.rawQueryString request))
       , " "
       , show (Http.statusCode status)
       ])

fromUtf8 :: Bytes.ByteString -> Either String String
fromUtf8 bytes =
  case Text.decodeUtf8' bytes of
    Left unicodeException -> Left (Exception.displayException unicodeException)
    Right text -> Right (Text.unpack text)

onExceptionResponse :: Exception.SomeException -> Wai.Response
onExceptionResponse _ =
  Wai.responseLBS Http.internalServerError500 [] LazyBytes.empty

defaultMain :: Warp.Settings -> Sqlite.Connection -> IO ()
defaultMain settings connection = do
  createItemsTable connection
  Async.race_ (runSync connection) (runServer settings connection)

runSync :: Sqlite.Connection -> IO ()
runSync connection =
  Monad.forever $ do
    syncFeeds connection feeds
    Concurrent.threadDelay (60 * 60 * 1000000)

createItemsTable :: Sqlite.Connection -> IO ()
createItemsTable connection =
  Sqlite.execute_
    connection
    " create table if not exists items \
    \ ( id integer primary key \
    \ , author text not null \
    \ , name text not null \
    \ , time datetime not null \
    \ , link text not null unique ) "

syncFeeds :: Sqlite.Connection -> [Feed] -> IO ()
syncFeeds connection feeds_ = do
  putStrLn (concat ["Syncing ", show (length feeds_), " feeds ..."])
  mapM_ (syncFeed connection) feeds_
  putStrLn (concat ["Synced ", show (length feeds_), " feeds."])

syncFeed :: Sqlite.Connection -> Feed -> IO ()
syncFeed connection feed = do
  putStrLn (concat ["Syncing <", feedUrl feed, "> ..."])
  result <- fetchFeed feed
  case result of
    Left problem -> putStrLn problem
    Right results -> do
      let (problems, items) = Either.partitionEithers results
      mapM_ putStrLn problems
      insertItems connection items
  putStrLn (concat ["Synced <", feedUrl feed, ">."])

insertItems :: Sqlite.Connection -> [Item] -> IO ()
insertItems connection items = do
  putStrLn (concat ["Inserting ", show (length items), " items ..."])
  mapM_ (insertItem connection) items
  putStrLn (concat ["Inserted ", show (length items), " items."])

insertItem :: Sqlite.Connection -> Item -> IO ()
insertItem connection item = do
  putStrLn (concat ["Inserting <", Text.unpack (itemLink item), "> ..."])
  Sqlite.execute
    connection
    " insert or ignore into items ( author , name , time , link ) \
    \ values ( ? , ? , ? , ? ) "
    (itemAuthor item, itemName item, itemTime item, itemLink item)

fetchFeed :: Feed -> IO (Either String [Either String Item])
fetchFeed feed = do
  request <- Client.parseRequest (feedUrl feed)
  Conduit.runConduitRes
    (Conduit.catchC
       (Conduit.fuse
          (Client.httpSource (withHeaders request) Client.getResponseBody)
          (Conduit.fuse (Xml.parseBytes Xml.def) (parseFeed (feedFormat feed))))
       (\exception ->
          pure
            (Left
               (Exception.displayException
                  (exception :: Exception.SomeException)))))

withHeaders :: Client.Request -> Client.Request
withHeaders request =
  Client.addRequestHeader
    Http.hAccept
    "*/*"
    (Client.addRequestHeader
       Http.hUserAgent
       (toUtf8 ("reveille-" ++ Version.showVersion This.version))
       request)

toUtf8 :: String -> Bytes.ByteString
toUtf8 string = Text.encodeUtf8 (Text.pack string)

parseFeed ::
     Format
  -> Conduit.ConduitM Xml.Event Conduit.Void (Conduit.ResourceT IO) (Either String [Either String Item])
parseFeed format =
  case format of
    FormatAtom -> parseAtom
    FormatRss -> parseRss

parseAtom ::
     Conduit.ConduitM Xml.Event Conduit.Void (Conduit.ResourceT IO) (Either String [Either String Item])
parseAtom = do
  maybeAtomFeed <- Atom.atomFeed
  case maybeAtomFeed of
    Nothing -> pure (Left "invalid atom feed")
    Just atomFeed -> pure (Right (extractAtomItems atomFeed))

extractAtomItems :: Atom.AtomFeed -> [Either String Item]
extractAtomItems atomFeed =
  map (extractAtomItem atomFeed) (Atom.feedEntries atomFeed)

extractAtomItem :: Atom.AtomFeed -> Atom.AtomEntry -> Either String Item
extractAtomItem atomFeed atomEntry =
  Right
    Item
      { itemAuthor = fromAtomText (Atom.feedTitle atomFeed)
      , itemName = fromAtomText (Atom.entryTitle atomEntry)
      , itemTime = Atom.entryUpdated atomEntry
      , itemLink = Atom.entryId atomEntry
      }

fromAtomText :: Atom.AtomText -> Text.Text
fromAtomText atomText =
  case atomText of
    Atom.AtomPlainText textType text ->
      case textType of
        Atom.TypeText -> text
        Atom.TypeHTML -> text -- TODO: unescape this
    Atom.AtomXHTMLText text -> text -- TODO: unescape this too

parseRss ::
     Conduit.ConduitM Xml.Event Conduit.Void (Conduit.ResourceT IO) (Either String [Either String Item])
parseRss = do
  maybeRssDocument <- Rss.rssDocument
  case maybeRssDocument of
    Nothing -> pure (Left "invalid rss feed")
    Just rssDocument -> pure (Right (extractRssItems rssDocument))

extractRssItems :: Rss.RssDocument' -> [Either String Item]
extractRssItems rssDocument =
  map (extractRssItem rssDocument) (Rss.channelItems rssDocument)

extractRssItem :: Rss.RssDocument' -> Rss.RssItem' -> Either String Item
extractRssItem rssDocument rssItem = do
  itemTime <-
    case Rss.itemPubDate rssItem of
      Nothing -> Left "no pub date"
      Just time -> Right time
  itemLink <-
    case Rss.itemLink rssItem of
      Nothing -> Left "no link"
      Just rssUri -> Rss.withRssURI renderUri rssUri
  Right
    Item
      { itemAuthor = Rss.channelTitle rssDocument
      , itemName = Rss.itemTitle rssItem
      , itemTime
      , itemLink
      }

renderUri :: Uri.URIRef a -> Either String Text.Text
renderUri uri =
  fmap
    Text.pack
    (fromUtf8
       (LazyBytes.toStrict (Builder.toLazyByteString (Uri.serializeURIRef uri))))

runServer :: Warp.Settings -> Sqlite.Connection -> IO ()
runServer settings connection =
  Warp.runSettings settings (applicationWith connection)

applicationWith :: Sqlite.Connection -> Wai.Application
applicationWith connection request respond =
  case (Wai.requestMethod request, Wai.pathInfo request) of
    ("GET", []) -> rootHandler connection respond
    ("GET", ["feed.atom"]) -> feedHandler connection respond
    _ -> notFoundHandler respond

rootHandler :: Sqlite.Connection -> Handler
rootHandler connection respond = do
  items <- getRecentItems connection
  let html = Lucid.renderBS (itemsToHtml items)
  respond
    (Wai.responseLBS
       Http.ok200
       [(Http.hContentType, "text/html; charset=utf-8")]
       html)

type Handler
   = (Wai.Response -> IO Wai.ResponseReceived) -> IO Wai.ResponseReceived

getRecentItems :: Sqlite.Connection -> IO [Item]
getRecentItems connection =
  Sqlite.query_
    connection
    " select author, name, time, link \
    \ from items \
    \ where time >= datetime('now', '-8 days') \
    \ order by time desc "

itemsToHtml :: [Item] -> Lucid.Html ()
itemsToHtml items =
  Lucid.doctypehtml_
    (mconcat
       [ Lucid.head_
           (mconcat
              [ Lucid.meta_ [Lucid.charset_ "utf-8"]
              , Lucid.title_ "Reveille"
              , Lucid.link_ [Lucid.rel_ "alternate", Lucid.href_ "/feed.atom"]
              ])
       , Lucid.body_
           (mconcat
              [ Lucid.h1_ "Reveille"
              , Lucid.ul_ (foldMap itemToHtml items)
              , Lucid.p_
                  (Lucid.a_
                     [Lucid.href_ "https://github.com/haskellweekly/reveille"]
                     "github.com/haskellweekly/reveille")
              ])
       ])

itemToHtml :: Item -> Lucid.Html ()
itemToHtml item =
  Lucid.li_
    (mconcat
       [ Lucid.a_ [Lucid.href_ (itemLink item)] (Lucid.toHtml (itemName item))
       , Lucid.span_ " from "
       , Lucid.span_ (Lucid.toHtml (itemAuthor item))
       , Lucid.span_ " on "
       , Lucid.time_
           [ Lucid.datetime_
               (renderTime "%Y-%m-%dT%H:%M:%S%Q%z" (itemTime item))
           ]
           (Lucid.toHtml (renderTime "%A, %B %e, %Y" (itemTime item)))
       ])

renderTime :: Text.Text -> Time.UTCTime -> Text.Text
renderTime format time =
  Text.pack (Time.formatTime Time.defaultTimeLocale (Text.unpack format) time)

feedHandler :: Sqlite.Connection -> Handler
feedHandler connection respond = do
  now <- Time.getCurrentTime
  items <- getRecentItems connection
  atom <- either fail pure (itemsToAtom now items)
  xml <- renderAtom atom
  respond
    (Wai.responseLBS
       Http.ok200
       [(Http.hContentType, "application/atom+xml")]
       xml)

itemsToAtom :: Time.UTCTime -> [Item] -> Either String Atom.AtomFeed
itemsToAtom time items = do
  entries <- mapM itemToAtom items
  Right
    Atom.AtomFeed
      { Atom.feedAuthors = []
      , Atom.feedCategories = []
      , Atom.feedContributors = []
      , Atom.feedEntries = entries
      , Atom.feedGenerator = Nothing
      , Atom.feedIcon = Nothing
      , Atom.feedId = Text.pack "https://reveille.haskellweekly.news/feed.atom"
      , Atom.feedLinks = []
      , Atom.feedLogo = Nothing
      , Atom.feedRights = Nothing
      , Atom.feedSubtitle = Nothing
      , Atom.feedTitle = Atom.AtomPlainText Atom.TypeText "Reveille"
      , Atom.feedUpdated =
          Maybe.fromMaybe
            time
            (Maybe.listToMaybe (List.sortOn Ord.Down (map itemTime items)))
      }

itemToAtom :: Item -> Either String Atom.AtomEntry
itemToAtom item = do
  author <- toAtomPerson (itemAuthor item)
  Right
    Atom.AtomEntry
      { Atom.entryAuthors = [author]
      , Atom.entryCategories = []
      , Atom.entryContent = Nothing
      , Atom.entryContributors = []
      , Atom.entryId = itemLink item
      , Atom.entryLinks = []
      , Atom.entryPublished = Nothing
      , Atom.entryRights = Nothing
      , Atom.entrySource = Nothing
      , Atom.entrySummary = Nothing
      , Atom.entryTitle = Atom.AtomPlainText Atom.TypeText (itemName item)
      , Atom.entryUpdated = itemTime item
      }

toAtomPerson :: Text.Text -> Either String Atom.AtomPerson
toAtomPerson name = do
  personName <-
    maybe (Left "toAtomPerson: empty name") Right (NonNull.fromNullable name)
  Right
    Atom.AtomPerson
      { Atom.personName = personName
      , Atom.personEmail = Text.empty
      , Atom.personUri = Nothing
      }

renderAtom :: Atom.AtomFeed -> IO LazyBytes.ByteString
renderAtom atom =
  Conduit.connect
    (Conduit.fuse
       (Atom.renderAtomFeed atom)
       (Conduit.fuse (Xml.renderBuilder Xml.def) Conduit.builderToByteString))
    Conduit.sinkLazy

notFoundHandler :: Handler
notFoundHandler respond =
  respond (Wai.responseLBS Http.notFound404 [] LazyBytes.empty)

data Item = Item
  { itemAuthor :: Text.Text -- TODO: make non null
  , itemName :: Text.Text -- TODO: make non null
  , itemTime :: Time.UTCTime
  , itemLink :: Text.Text -- TODO: make uri
  } deriving (Eq, Show)

instance Sqlite.FromRow Item where
  fromRow = do
    itemAuthor <- Sqlite.field
    itemName <- Sqlite.field
    itemTime <- Sqlite.field
    itemLink <- Sqlite.field
    pure Item {itemAuthor, itemName, itemTime, itemLink}

data Feed = Feed
  { feedFormat :: Format
  , feedUrl :: String
  } deriving (Eq, Show)

data Format
  = FormatAtom
  | FormatRss
  deriving (Eq, Show)

feeds :: [Feed]
feeds =
  [ Feed { feedFormat = FormatAtom, feedUrl = "https://haskellweekly.news/haskell-weekly.atom" }

  , Feed { feedFormat = FormatAtom, feedUrl = "http://allocinit.io/feed.xml" }
  , Feed { feedFormat = FormatAtom, feedUrl = "http://baatz.io/atom.xml" }
  , Feed { feedFormat = FormatAtom, feedUrl = "http://bitemyapp.com/atom.xml" }
  , Feed { feedFormat = FormatAtom, feedUrl = "http://blog.ielliott.io/feed.xml" }
  , Feed { feedFormat = FormatAtom, feedUrl = "http://blog.ploeh.dk/atom.xml" }
  , Feed { feedFormat = FormatAtom, feedUrl = "http://clrnd.com.ar/atom.xml" }
  , Feed { feedFormat = FormatAtom, feedUrl = "http://feeds.feedburner.com/ChurningAndChurning" }
  , Feed { feedFormat = FormatAtom, feedUrl = "http://feeds.feedburner.com/GoogleOpenSourceBlog" }
  , Feed { feedFormat = FormatAtom, feedUrl = "http://feeds2.feedburner.com/chadaustin" }
  , Feed { feedFormat = FormatAtom, feedUrl = "http://fixpt.de/atom.xml" }
  , Feed { feedFormat = FormatAtom, feedUrl = "http://haroldcarr.com/atom.xml" }
  , Feed { feedFormat = FormatAtom, feedUrl = "http://jeremymikkola.com/atom.xml" }
  , Feed { feedFormat = FormatAtom, feedUrl = "http://mutanatum.com/atom.xml" }
  , Feed { feedFormat = FormatAtom, feedUrl = "http://nmattia.com/atom.xml?type=blog" }
  , Feed { feedFormat = FormatAtom, feedUrl = "http://oleg.fi/gists/atom.xml" }
  , Feed { feedFormat = FormatAtom, feedUrl = "http://osa1.net/rss.xml" }
  , Feed { feedFormat = FormatAtom, feedUrl = "http://r6.ca/blog/feed.atom" }
  , Feed { feedFormat = FormatAtom, feedUrl = "http://reasonablypolymorphic.com/atom.xml" }
  , Feed { feedFormat = FormatAtom, feedUrl = "http://snapframework.com/blog/feed.xml" }
  , Feed { feedFormat = FormatAtom, feedUrl = "http://syocy.hatenablog.com/feed" }
  , Feed { feedFormat = FormatAtom, feedUrl = "http://tab.snarc.org/rss.xml" }
  , Feed { feedFormat = FormatAtom, feedUrl = "http://taylor.fausak.me/sitemap.atom" }
  , Feed { feedFormat = FormatAtom, feedUrl = "http://tech.frontrowed.com/feed.xml" }
  , Feed { feedFormat = FormatAtom, feedUrl = "http://vaibhavsagar.com/atom.xml" }
  , Feed { feedFormat = FormatAtom, feedUrl = "http://www.adomokos.com/feeds/posts/default" }
  , Feed { feedFormat = FormatAtom, feedUrl = "http://www.andrevdm.com/atom.xml" }
  , Feed { feedFormat = FormatAtom, feedUrl = "http://www.echonolan.net/atom.xml" }
  , Feed { feedFormat = FormatAtom, feedUrl = "http://www.haskellforall.com/feeds/posts/default" }
  , Feed { feedFormat = FormatAtom, feedUrl = "http://www.michaelburge.us/feed.xml" }
  , Feed { feedFormat = FormatAtom, feedUrl = "http://www.stephendiehl.com/feed.atom" }
  , Feed { feedFormat = FormatAtom, feedUrl = "http://www.sylvain-henry.info/home/atom.xml" }
  , Feed { feedFormat = FormatAtom, feedUrl = "https://aphyr.com/posts.atom" }
  , Feed { feedFormat = FormatAtom, feedUrl = "https://begriffs.com/atom.xml" }
  , Feed { feedFormat = FormatAtom, feedUrl = "https://blog.akii.de/feed.atom" }
  , Feed { feedFormat = FormatAtom, feedUrl = "https://blog.rcook.org/atom.xml" }
  , Feed { feedFormat = FormatAtom, feedUrl = "https://blog.scottnonnenberg.com/atom.xml" }
  , Feed { feedFormat = FormatAtom, feedUrl = "https://brianmckenna.org/blog/feed" }
  , Feed { feedFormat = FormatAtom, feedUrl = "https://charukiewi.cz/atom.xml" }
  , Feed { feedFormat = FormatAtom, feedUrl = "https://chrispenner.ca/atom.xml" }
  , Feed { feedFormat = FormatAtom, feedUrl = "https://codygman.github.io/atom.xml" }
  , Feed { feedFormat = FormatAtom, feedUrl = "https://cse.iitk.ac.in/users/ppk/posts/feeds/atom.xml" }
  , Feed { feedFormat = FormatAtom, feedUrl = "https://deliquus.com/atom.xml" }
  , Feed { feedFormat = FormatAtom, feedUrl = "https://dfordivam.github.io/atom.xml" }
  , Feed { feedFormat = FormatAtom, feedUrl = "https://dodisturb.me/atom.xml" }
  , Feed { feedFormat = FormatAtom, feedUrl = "https://dpwright.com/atom.xml" }
  , Feed { feedFormat = FormatAtom, feedUrl = "https://eli.thegreenplace.net/feeds/all.atom.xml" }
  , Feed { feedFormat = FormatAtom, feedUrl = "https://feeds.feedburner.com/AndyArvanitis" }
  , Feed { feedFormat = FormatAtom, feedUrl = "https://fgaz.me/atom.xml" }
  , Feed { feedFormat = FormatAtom, feedUrl = "https://functor.tokyo/blog/feed" }
  , Feed { feedFormat = FormatAtom, feedUrl = "https://futtetennismo.me/feed.xml" }
  , Feed { feedFormat = FormatAtom, feedUrl = "https://gelisam.blogspot.com/feeds/posts/default" }
  , Feed { feedFormat = FormatAtom, feedUrl = "https://haskell-at-work.com/atom.xml" }
  , Feed { feedFormat = FormatAtom, feedUrl = "https://haskellexists.blogspot.com/feeds/posts/default" }
  , Feed { feedFormat = FormatAtom, feedUrl = "https://haskelltools.blogspot.com/feeds/posts/default" }
  , Feed { feedFormat = FormatAtom, feedUrl = "https://hmemcpy.com/atom.xml" }
  , Feed { feedFormat = FormatAtom, feedUrl = "https://hookrace.net/blog/feed/" }
  , Feed { feedFormat = FormatAtom, feedUrl = "https://icidasset.com/feed.xml" }
  , Feed { feedFormat = FormatAtom, feedUrl = "https://ixmatus.net/atom.xml" }
  , Feed { feedFormat = FormatAtom, feedUrl = "https://jawaninja.com/atom.xml" }
  , Feed { feedFormat = FormatAtom, feedUrl = "https://jerrington.me/atom.xml" }
  , Feed { feedFormat = FormatAtom, feedUrl = "https://joyfulmantis.github.io/atom.xml" }
  , Feed { feedFormat = FormatAtom, feedUrl = "https://jpvillaisaza.github.io/feed" }
  , Feed { feedFormat = FormatAtom, feedUrl = "https://jship.github.io/atom.xml" }
  , Feed { feedFormat = FormatAtom, feedUrl = "https://juhp.blogspot.com/feeds/posts/default" }
  , Feed { feedFormat = FormatAtom, feedUrl = "https://kseo.github.io/atom.xml" }
  , Feed { feedFormat = FormatAtom, feedUrl = "https://lexi-lambda.github.io/feeds/all.atom.xml" }
  , Feed { feedFormat = FormatAtom, feedUrl = "https://literateprogrammer.blogspot.com/feeds/posts/default" }
  , Feed { feedFormat = FormatAtom, feedUrl = "https://making.pusher.com/feed.xml" }
  , Feed { feedFormat = FormatAtom, feedUrl = "https://markkarpov.com/feed.atom" }
  , Feed { feedFormat = FormatAtom, feedUrl = "https://mazzo.li/atom.xml" }
  , Feed { feedFormat = FormatAtom, feedUrl = "https://mendo.zone/feed.xml" }
  , Feed { feedFormat = FormatAtom, feedUrl = "https://mpickering.github.io/atom.xml" }
  , Feed { feedFormat = FormatAtom, feedUrl = "https://namc.in/feed.xml" }
  , Feed { feedFormat = FormatAtom, feedUrl = "https://neilmitchell.blogspot.com/feeds/posts/default" }
  , Feed { feedFormat = FormatAtom, feedUrl = "https://nikita-volkov.github.io/feed.xml" }
  , Feed { feedFormat = FormatAtom, feedUrl = "https://noughtmare.gitlab.io/atom.xml" }
  , Feed { feedFormat = FormatAtom, feedUrl = "https://paulspontifications.blogspot.com/feeds/posts/default" }
  , Feed { feedFormat = FormatAtom, feedUrl = "https://prl.ccs.neu.edu/blog/feeds/all.atom.xml" }
  , Feed { feedFormat = FormatAtom, feedUrl = "https://profsjt.blogspot.com/feeds/posts/default" }
  , Feed { feedFormat = FormatAtom, feedUrl = "https://purelyfunctional.org/atom.xml" }
  , Feed { feedFormat = FormatAtom, feedUrl = "https://qfpl.io/atom.xml" }
  , Feed { feedFormat = FormatAtom, feedUrl = "https://rkrishnan.org/atom.xml" }
  , Feed { feedFormat = FormatAtom, feedUrl = "https://rootmos.github.io/feed.xml" }
  , Feed { feedFormat = FormatAtom, feedUrl = "https://samtay.github.io/atom.xml" }
  , Feed { feedFormat = FormatAtom, feedUrl = "https://silky.github.io/atom.xml" }
  , Feed { feedFormat = FormatAtom, feedUrl = "https://simonmar.github.io/atom.xml" }
  , Feed { feedFormat = FormatAtom, feedUrl = "https://tech.channable.com/atom.xml" }
  , Feed { feedFormat = FormatAtom, feedUrl = "https://teh.id.au/posts/atom.xml" }
  , Feed { feedFormat = FormatAtom, feedUrl = "https://trofi.github.io/feed/atom.xml" }
  , Feed { feedFormat = FormatAtom, feedUrl = "https://ucsd-progsys.github.io/liquidhaskell-blog/atom.xml" }
  , Feed { feedFormat = FormatAtom, feedUrl = "https://wadler.blogspot.com/feeds/posts/default" }
  , Feed { feedFormat = FormatAtom, feedUrl = "https://www.kovach.me/atom.xml" }
  , Feed { feedFormat = FormatAtom, feedUrl = "https://www.schoolofhaskell.com/recent-content/feed" }
  , Feed { feedFormat = FormatAtom, feedUrl = "https://www.shimweasel.com/atom.xml" }
  , Feed { feedFormat = FormatAtom, feedUrl = "https://www.snoyman.com/feed" }
  , Feed { feedFormat = FormatAtom, feedUrl = "https://www.stackage.org/blog/feed" }
  , Feed { feedFormat = FormatAtom, feedUrl = "https://www.stackbuilders.com/tutorials/atom.xml" }
  , Feed { feedFormat = FormatAtom, feedUrl = "https://www.stephanboyer.com/atom" }
  , Feed { feedFormat = FormatAtom, feedUrl = "https://www.yesodweb.com/feed" }

  , Feed { feedFormat = FormatRss, feedUrl = "http://alpmestan.com/rss.xml" }
  , Feed { feedFormat = FormatRss, feedUrl = "http://blog.haskellformac.com/feed" }
  , Feed { feedFormat = FormatRss, feedUrl = "http://blog.sumtypeofway.com/rss/" }
  , Feed { feedFormat = FormatRss, feedUrl = "http://blogs.intevation.de/wilde/index.xml" }
  , Feed { feedFormat = FormatRss, feedUrl = "http://comonad.com/reader/feed/" }
  , Feed { feedFormat = FormatRss, feedUrl = "http://deliberate-software.com/index.xml" }
  , Feed { feedFormat = FormatRss, feedUrl = "http://feeds.feedburner.com/incodeblog" }
  , Feed { feedFormat = FormatRss, feedUrl = "http://get-finch.com/feed.xml" }
  , Feed { feedFormat = FormatRss, feedUrl = "http://lambda-the-ultimate.org/rss.xml" }
  , Feed { feedFormat = FormatRss, feedUrl = "http://math.andrej.com/feed/" }
  , Feed { feedFormat = FormatRss, feedUrl = "http://paul.bone.id.au/blog.xml" }
  , Feed { feedFormat = FormatRss, feedUrl = "http://regex.uk/blog/rss.xml" }
  , Feed { feedFormat = FormatRss, feedUrl = "http://summer.haskell.org/news.xml" }
  , Feed { feedFormat = FormatRss, feedUrl = "http://techblog.holidaycheck.com/feed.xml" }
  , Feed { feedFormat = FormatRss, feedUrl = "http://tomasp.net/rss.xml" }
  , Feed { feedFormat = FormatRss, feedUrl = "http://www.arcadianvisions.com/blog/rss.xml" }
  , Feed { feedFormat = FormatRss, feedUrl = "http://www.prigrammer.com/?feed=rss2" }
  , Feed { feedFormat = FormatRss, feedUrl = "http://www.usrsb.in/rss.xml" }
  , Feed { feedFormat = FormatRss, feedUrl = "https://alpacaaa.net/blog/index.xml" }
  , Feed { feedFormat = FormatRss, feedUrl = "https://alternativebit.fr/posts/index.xml" }
  , Feed { feedFormat = FormatRss, feedUrl = "https://andyshiue.github.io/feed.xml" }
  , Feed { feedFormat = FormatRss, feedUrl = "https://argumatronic.com/rss.xml" }
  , Feed { feedFormat = FormatRss, feedUrl = "https://arunraghavan.net/feed/" }
  , Feed { feedFormat = FormatRss, feedUrl = "https://bartoszmilewski.com/feed/" }
  , Feed { feedFormat = FormatRss, feedUrl = "https://bgamari.github.io/rss.xml" }
  , Feed { feedFormat = FormatRss, feedUrl = "https://blog.cotten.io/feed" }
  , Feed { feedFormat = FormatRss, feedUrl = "https://blog.eta-lang.org/feed" }
  , Feed { feedFormat = FormatRss, feedUrl = "https://blog.grakn.ai/feed" }
  , Feed { feedFormat = FormatRss, feedUrl = "https://blog.hackage.haskell.org/rss.xml" }
  , Feed { feedFormat = FormatRss, feedUrl = "https://blog.lahteenmaki.net/rss.xml" }
  , Feed { feedFormat = FormatRss, feedUrl = "https://blog.poisson.chat/rss.xml" }
  , Feed { feedFormat = FormatRss, feedUrl = "https://blogs.msdn.microsoft.com/commandline/feed/" }
  , Feed { feedFormat = FormatRss, feedUrl = "https://blogs.ncl.ac.uk/andreymokhov/feed/" }
  , Feed { feedFormat = FormatRss, feedUrl = "https://byorgey.wordpress.com/feed/" }
  , Feed { feedFormat = FormatRss, feedUrl = "https://carlosmchica.github.io/feed.xml" }
  , Feed { feedFormat = FormatRss, feedUrl = "https://cdsmith.wordpress.com/feed/" }
  , Feed { feedFormat = FormatRss, feedUrl = "https://chris-martin.org/rss.xml" }
  , Feed { feedFormat = FormatRss, feedUrl = "https://chrisdone.com/rss.xml" }
  , Feed { feedFormat = FormatRss, feedUrl = "https://code.takt.com/feed" }
  , Feed { feedFormat = FormatRss, feedUrl = "https://dbp.io/rss.xml" }
  , Feed { feedFormat = FormatRss, feedUrl = "https://denibertovic.com/rss.xml" }
  , Feed { feedFormat = FormatRss, feedUrl = "https://deque.blog/feed/" }
  , Feed { feedFormat = FormatRss, feedUrl = "https://dev.to/feed/rtfeldman" }
  , Feed { feedFormat = FormatRss, feedUrl = "https://elvishjerricco.github.io/feed.xml" }
  , Feed { feedFormat = FormatRss, feedUrl = "https://eng.uber.com/feed/" }
  , Feed { feedFormat = FormatRss, feedUrl = "https://engineers.irisconnect.net/rss.xml" }
  , Feed { feedFormat = FormatRss, feedUrl = "https://feeds.feedburner.com/KrisJenkinsBlog" }
  , Feed { feedFormat = FormatRss, feedUrl = "https://feeds.soundcloud.com/users/soundcloud:users:201515747/sounds.rss" }
  , Feed { feedFormat = FormatRss, feedUrl = "https://ghc.haskell.org/trac/ghc/blog?format=rss" }
  , Feed { feedFormat = FormatRss, feedUrl = "https://halogenandtoast.com/rss/" }
  , Feed { feedFormat = FormatRss, feedUrl = "https://hypothesis.works/articles/feed/" }
  , Feed { feedFormat = FormatRss, feedUrl = "https://ipfs.io/ipfs/QmfN5DojVnEsf1Une3DFwfUiFpfWnQf31f61qgybiXVeQE/index.xml" }
  , Feed { feedFormat = FormatRss, feedUrl = "https://itscode.red/index.xml" }
  , Feed { feedFormat = FormatRss, feedUrl = "https://janmasrovira.gitlab.io/ascetic-slug/index.xml" }
  , Feed { feedFormat = FormatRss, feedUrl = "https://jaredweakly.com/feed/" }
  , Feed { feedFormat = FormatRss, feedUrl = "https://jaseemabid.github.io/feed.xml" }
  , Feed { feedFormat = FormatRss, feedUrl = "https://jaspervdj.be/rss.xml" }
  , Feed { feedFormat = FormatRss, feedUrl = "https://jezenthomas.com/feed.xml" }
  , Feed { feedFormat = FormatRss, feedUrl = "https://johncarlosbaez.wordpress.com/feed/" }
  , Feed { feedFormat = FormatRss, feedUrl = "https://jozefg.bitbucket.io/rss.xml" }
  , Feed { feedFormat = FormatRss, feedUrl = "https://jpittis.ca/index.xml" }
  , Feed { feedFormat = FormatRss, feedUrl = "https://lettier.github.io/rss.xml" }
  , Feed { feedFormat = FormatRss, feedUrl = "https://medium.com/feed/@concertdaw" }
  , Feed { feedFormat = FormatRss, feedUrl = "https://medium.com/feed/@djoyner" }
  , Feed { feedFormat = FormatRss, feedUrl = "https://medium.com/feed/@fintan.halpenny" }
  , Feed { feedFormat = FormatRss, feedUrl = "https://medium.com/feed/@folsen" }
  , Feed { feedFormat = FormatRss, feedUrl = "https://medium.com/feed/@horrorcheck" }
  , Feed { feedFormat = FormatRss, feedUrl = "https://medium.com/feed/@jonathangfischoff" }
  , Feed { feedFormat = FormatRss, feedUrl = "https://medium.com/feed/@paf31" }
  , Feed { feedFormat = FormatRss, feedUrl = "https://medium.com/feed/@saurabhnanda" }
  , Feed { feedFormat = FormatRss, feedUrl = "https://medium.com/feed/@sjsyrek" }
  , Feed { feedFormat = FormatRss, feedUrl = "https://medium.com/feed/@zw3rk" }
  , Feed { feedFormat = FormatRss, feedUrl = "https://medium.com/feed/@zyxoas" }
  , Feed { feedFormat = FormatRss, feedUrl = "https://medium.com/feed/wit-ai" }
  , Feed { feedFormat = FormatRss, feedUrl = "https://metarabbit.wordpress.com/feed/" }
  , Feed { feedFormat = FormatRss, feedUrl = "https://mgattozzi.com/feed" }
  , Feed { feedFormat = FormatRss, feedUrl = "https://michaelxavier.net/rss.xml" }
  , Feed { feedFormat = FormatRss, feedUrl = "https://mmhaskell.com/blog?format=rss" }
  , Feed { feedFormat = FormatRss, feedUrl = "https://mzabani.wordpress.com/feed" }
  , Feed { feedFormat = FormatRss, feedUrl = "https://nunoalexandre.com/feed.xml" }
  , Feed { feedFormat = FormatRss, feedUrl = "https://ocharles.org.uk/blog/posts.rss" }
  , Feed { feedFormat = FormatRss, feedUrl = "https://paytonturnage.com/feed.xml" }
  , Feed { feedFormat = FormatRss, feedUrl = "https://pchiusano.github.io/feed.xml" }
  , Feed { feedFormat = FormatRss, feedUrl = "https://programming.tobiasdammers.nl/blog/rss" }
  , Feed { feedFormat = FormatRss, feedUrl = "https://ro-che.info/articles/rss.xml" }
  , Feed { feedFormat = FormatRss, feedUrl = "https://ryanglscott.github.io/feed.xml" }
  , Feed { feedFormat = FormatRss, feedUrl = "https://tech.small-improvements.com/feed/" }
  , Feed { feedFormat = FormatRss, feedUrl = "https://theinitialcommit.com/feed.xml" }
  , Feed { feedFormat = FormatRss, feedUrl = "https://torchhound.github.io/rss.xml" }
  , Feed { feedFormat = FormatRss, feedUrl = "https://trandi.wordpress.com/feed/" }
  , Feed { feedFormat = FormatRss, feedUrl = "https://two-wrongs.com/feed.xml" }
  , Feed { feedFormat = FormatRss, feedUrl = "https://typesandkinds.wordpress.com/feed/" }
  , Feed { feedFormat = FormatRss, feedUrl = "https://unknownparallel.wordpress.com/feed/" }
  , Feed { feedFormat = FormatRss, feedUrl = "https://vadosware.io/index.xml" }
  , Feed { feedFormat = FormatRss, feedUrl = "https://whatthefunctional.wordpress.com/feed/" }
  , Feed { feedFormat = FormatRss, feedUrl = "https://wickstrom.tech/feed.xml" }
  , Feed { feedFormat = FormatRss, feedUrl = "https://www.athiemann.net/feed.xml" }
  , Feed { feedFormat = FormatRss, feedUrl = "https://www.dabolivar.com/index.xml" }
  , Feed { feedFormat = FormatRss, feedUrl = "https://www.fosskers.ca/rss-en" }
  , Feed { feedFormat = FormatRss, feedUrl = "https://www.functionalgeekery.com/feed/" }
  , Feed { feedFormat = FormatRss, feedUrl = "https://www.jeroenkeiren.nl/feed/" }
  , Feed { feedFormat = FormatRss, feedUrl = "https://www.joachim-breitner.de/blog_feed.rss" }
  , Feed { feedFormat = FormatRss, feedUrl = "https://www.microsoft.com/en-us/research/blog/category/podcast/feed/" }
  , Feed { feedFormat = FormatRss, feedUrl = "https://www.parsonsmatt.org/feed.xml" }
  , Feed { feedFormat = FormatRss, feedUrl = "https://www.spock.li/feed.xml" }
  , Feed { feedFormat = FormatRss, feedUrl = "https://www.tweag.io/rss.xml" }
  , Feed { feedFormat = FormatRss, feedUrl = "https://www.twilio.com/blog/feed" }
  , Feed { feedFormat = FormatRss, feedUrl = "https://yager.io/feed/" }

  , Feed { feedFormat = FormatAtom, feedUrl = "http://chriswarbo.net/blog.atom" } -- TODO: invalid date
  , Feed { feedFormat = FormatAtom, feedUrl = "http://degoes.net/feed.xml" } -- TODO: invalid end element
  , Feed { feedFormat = FormatAtom, feedUrl = "http://kcsongor.github.io/feed.xml" } -- TODO: invalid date
  , Feed { feedFormat = FormatAtom, feedUrl = "https://abhiroop.github.io/feed.xml" } -- TODO: null element
  , Feed { feedFormat = FormatAtom, feedUrl = "https://blog.infinitenegativeutility.com/rss" } -- TODO: invalid date
  , Feed { feedFormat = FormatAtom, feedUrl = "https://blue-dinosaur.github.io/feed.xml" } -- TODO: null element
  , Feed { feedFormat = FormatAtom, feedUrl = "https://chairnerd.seatgeek.com/atom.xml" } -- TODO: null element
  , Feed { feedFormat = FormatAtom, feedUrl = "https://codurance.com/atom.xml" } -- TODO: missing updated element
  , Feed { feedFormat = FormatAtom, feedUrl = "https://debugsteven.github.io/feed.xml" } -- TODO: null element
  , Feed { feedFormat = FormatAtom, feedUrl = "https://feeds.feedburner.com/ezyang" } -- TODO: missing updated element
  , Feed { feedFormat = FormatAtom, feedUrl = "https://jml.io/feeds/all.atom.xml" } -- TODO: insecure
  , Feed { feedFormat = FormatAtom, feedUrl = "https://m0ar.github.io/safe-streaming/feed.xml" } -- TODO: null element
  , Feed { feedFormat = FormatAtom, feedUrl = "https://quasimal.com/feed.xml" } -- TODO: insecure
  , Feed { feedFormat = FormatAtom, feedUrl = "https://www.fpcomplete.com/blog/atom.xml" } -- TODO: missing name element
  , Feed { feedFormat = FormatAtom, feedUrl = "https://www.morphism.tech/feed/" } -- TODO: insecure
  , Feed { feedFormat = FormatAtom, feedUrl = "https://www.well-typed.com/blog/atom.xml" } -- TODO: invalid end element

  , Feed { feedFormat = FormatRss, feedUrl = "http://gilmi.me/blog/rss" } -- TODO: malformed path
  , Feed { feedFormat = FormatRss, feedUrl = "http://jxv.io/blog/rss.xml" } -- TODO: invalid time
  , Feed { feedFormat = FormatRss, feedUrl = "http://keera.co.uk/blog/feed/" } -- TODO: invalid feed
  , Feed { feedFormat = FormatRss, feedUrl = "http://storm-country.com/rss" } -- TODO: no pub date
  , Feed { feedFormat = FormatRss, feedUrl = "https://joyofhaskell.com/rss.xml" } -- TODO: invalid end element
  , Feed { feedFormat = FormatRss, feedUrl = "https://jstoelm.libsyn.com/rss" } -- TODO: invalid end element
  , Feed { feedFormat = FormatRss, feedUrl = "https://open.bekk.no/feed/Technology" } -- TODO: invalid time
  , Feed { feedFormat = FormatRss, feedUrl = "https://tech-blog.capital-match.com/feed.rss" } -- TODO: invalid time
  , Feed { feedFormat = FormatRss, feedUrl = "https://www.haskellcast.com/feed.xml" } -- TODO : invalid end element

  ]
