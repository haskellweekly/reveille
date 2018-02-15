module Main
  ( main
  ) where

import Data.Function ((&))

import qualified Control.Concurrent as Concurrent
import qualified Control.Concurrent.Async as Async
import qualified Control.Concurrent.STM as Stm
import qualified Control.Exception as Exception
import qualified Control.Monad as Monad
import qualified Data.ByteString.Lazy as LazyBytes
import qualified Data.Foldable as Foldable
import qualified Data.List as List
import qualified Data.Map as Map
import qualified Data.Maybe as Maybe
import qualified Data.Ord as Ord
import qualified Data.Set as Set
import qualified Data.Text as Text
import qualified Data.Text.Encoding as Text
import qualified Data.Time as Time
import qualified Data.Version as Version
import qualified Network.HTTP.Client as Client
import qualified Network.HTTP.Client.TLS as Client
import qualified Network.HTTP.Types as Http
import qualified Network.Wai as Wai
import qualified Network.Wai.Handler.Warp as Warp
import qualified Paths_reveille as This
import qualified Text.Feed.Import as Feed
import qualified Text.Feed.Query as Feed
import qualified Text.Feed.Types as Feed
import qualified Text.Printf as Printf
import qualified Text.XML as Xml

main :: IO ()
main = do
  manager <- Client.newTlsManager
  database <- Stm.newTVarIO initialDatabase

  Async.concurrently_
    (startUpdater manager database)
    (startServer database)

startUpdater :: Client.Manager -> Stm.TVar Database -> IO ()
startUpdater manager database = Monad.forever (do
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

startServer :: Stm.TVar Database -> IO ()
startServer database = Warp.run 3000 (\ request respond -> do
  let method = Text.unpack (Text.decodeUtf8 (Wai.requestMethod request))
  let path = map Text.unpack (Wai.pathInfo request)
  case (method, path) of
    ("GET", ["feed.atom"]) -> do
      db <- Stm.readTVarIO database
      now <- Time.getCurrentTime
      let items = getRecentDatabaseItems db now
      let entries = map itemToEntry items
      let feed = xmlElement
            "feed"
            [("xmlns", "http://www.w3.org/2005/Atom")]
            ( xmlNode "title" [] [xmlContent "Haskell Weekly"]
            : xmlNode "id" [] [xmlContent "https://haskellweekly.news/"]
            : xmlNode "updated" [] [xmlContent (rfc3339 now)]
            : xmlNode "link" [("rel", "self"), ("href", "http://localhost:3000/feed.atom")] []
            : entries
            )
      let document = xmlDocument feed
      respond (Wai.responseLBS
        Http.ok200
        [(Http.hContentType, Text.encodeUtf8 (Text.pack "application/atom+xml"))]
        (Xml.renderLBS Xml.def document))
    ("GET", ["health-check"]) -> respond
      (Wai.responseLBS Http.ok200 [] LazyBytes.empty)
    ("GET", ["robots.txt"]) -> respond (Wai.responseLBS
      Http.ok200
      []
      (LazyBytes.fromStrict (Text.encodeUtf8 (Text.pack (unlines
        [ "User-Agent: *"
        , "Disallow:"
        ])))))
    ("GET", ["favicon.ico"]) -> respond (Wai.responseLBS
      Http.ok200
      []
      (LazyBytes.pack
        [ 0x00, 0x00, 0x01, 0x00, 0x01, 0x00, 0x10, 0x10
        , 0x02, 0x00, 0x01, 0x00, 0x01, 0x00, 0xb0, 0x00
        , 0x00, 0x00, 0x16, 0x00, 0x00, 0x00, 0x28, 0x00
        , 0x00, 0x00, 0x10, 0x00, 0x00, 0x00, 0x20, 0x00
        , 0x00, 0x00, 0x01, 0x00, 0x01, 0x00, 0x00, 0x00
        , 0x00, 0x00, 0x40, 0x00, 0x00, 0x00, 0x00, 0x00
        , 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x02, 0x00
        , 0x00, 0x00, 0x02, 0x00, 0x00, 0x00, 0x79, 0x48
        , 0x70, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00
        , 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00
        , 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00
        , 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00
        , 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00
        , 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00
        , 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00
        , 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00
        , 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00
        , 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00
        , 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00
        , 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00
        , 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00
        , 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00
        , 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00
        , 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00
        , 0x00, 0x00, 0x00, 0x00, 0x00, 0x00
        ]))
    ("GET", []) -> do
      db <- Stm.readTVarIO database
      now <- Time.getCurrentTime
      let items = getRecentDatabaseItems db now
      respond (Wai.responseLBS
        Http.ok200
        [(Http.hContentType, Text.encodeUtf8 (Text.pack "text/html; charset=utf-8"))]
        (LazyBytes.fromStrict (Text.encodeUtf8 (Text.pack (unlines
          [ "<!doctype html>"
          , "<html>"
          , "  <head>"
          , "    <meta charset=\"utf-8\">"
          , "    <title>Haskell Weekly</title>"
          , "    <link rel=\"alternate\" type=\"application/atom+xml\" href=\"feed.atom\">"
          , "  </head>"
          , "  <body>"
          , "    <h1>Haskell Weekly</h1>"
          , "    <p>"
          , "      Subscribe to the <a href=\"feed.atom\">feed</a>."
          , "    </p>"
          , "    <ol>"
          , List.intercalate "\n" (map
            (\ (author, item) -> concat
              [ "      <li>\n"
              , "        <a href=\""
              , fromUrl (itemUrl item)
              , "\">"
              , fromName (itemName item)
              , "</a> by "
              , fromName (authorName author)
              , " on "
              , Time.formatTime Time.defaultTimeLocale "%B %-e" (itemTime item)
              , "\n"
              , "      </li>"
              ])
            items)
          , "    </ol>"
          , "  </body>"
          , "</html>"
          ])))))
    _ -> respond (Wai.responseLBS Http.notFound404 [] mempty))

xmlName :: String -> Xml.Name
xmlName string = Xml.Name (Text.pack string) Nothing Nothing

xmlElement :: String -> [(String, String)] -> [Xml.Node] -> Xml.Element
xmlElement name attributes children = Xml.Element
  (xmlName name)
  (Map.fromList (map (\ (k, v) -> (xmlName k, Text.pack v)) attributes))
  children

xmlNode :: String -> [(String, String)] -> [Xml.Node] -> Xml.Node
xmlNode name attributes children = Xml.NodeElement (xmlElement name attributes children)

xmlContent :: String -> Xml.Node
xmlContent string = Xml.NodeContent (Text.pack string)

xmlDocument :: Xml.Element -> Xml.Document
xmlDocument element = Xml.Document (Xml.Prologue [] Nothing []) element []

rfc3339 :: Time.UTCTime -> String
rfc3339 time = Time.formatTime Time.defaultTimeLocale "%Y-%m-%dT%H:%M:%S%QZ" time

authors :: Set.Set Author
authors = Set.fromList
  [ toAuthor
    "Alex Beal"
    "http://www.usrsb.in"
    (Just "http://www.usrsb.in/rss.xml")
  , toAuthor
    "Alexis King"
    "https://lexi-lambda.github.io"
    (Just "https://lexi-lambda.github.io/feeds/all.atom.xml")
  , toAuthor
    "Andre Van Der Merwe"
    "http://www.andrevdm.com/"
    (Just "http://www.andrevdm.com/atom.xml")
  , toAuthor
    "Attila Domokos"
    "http://www.adomokos.com"
    (Just "http://www.adomokos.com/feeds/posts/default")
  , toAuthor
    "Ben Gamari"
    "https://tomsmalley.github.io"
    Nothing
  , toAuthor
    "Concert"
    "https://medium.com/@concertdaw"
    (Just "https://medium.com/feed/@concertdaw")
  , toAuthor
    "Daniel Bolivar"
    "https://www.dabolivar.com"
    (Just "https://www.dabolivar.com/index.xml")
  , toAuthor
    "FP Complete"
    "https://www.fpcomplete.com"
    (Just "https://www.fpcomplete.com/blog/atom.xml")
  , toAuthor
    "Gabriel Gonzalez"
    "http://www.haskellforall.com"
    (Just "http://www.haskellforall.com/feeds/posts/default")
  , toAuthor
    "Google Summer of Code"
    "https://summerofcode.withgoogle.com"
    (Just "http://feeds.feedburner.com/GoogleOpenSourceBlog")
  , toAuthor
    "Haskell at Work"
    "https://haskell-at-work.com"
    (Just "https://www.youtube.com/feeds/videos.xml?channel_id=UCUgxpaK7ySR-z6AXA5-uDuw")
  , toAuthor
    "Haskell Weekly"
    "https://haskellweekly.news"
    (Just "https://haskellweekly.news/haskell-weekly.atom")
  , toAuthor
    "Humble Bundle"
    "https://www.humblebundle.com"
    (Just "http://blog.humblebundle.com/rss")
  , toAuthor
    "Ibnu D. Aji"
    "https://ibnuda.gitlab.io"
    Nothing -- https://gitlab.com/ibnuda/ibnuda.gitlab.io/issues/2
  , toAuthor
    "Joachim Breitner"
    "https://www.joachim-breitner.de"
    (Just "https://www.joachim-breitner.de/blog_feed.rss")
  , toAuthor
    "Mark Karpov"
    "https://markkarpov.com"
    (Just "https://markkarpov.com/feed.atom")
  , toAuthor
    "Matt Noonan"
    "http://storm-country.com"
    (Just "http://storm-country.com/rss")
  , toAuthor
    "Michael Snoyman"
    "https://www.snoyman.com"
    (Just "https://www.snoyman.com/feed")
  , toAuthor
    "Mistral Contrastin"
    "https://dodisturb.me"
    Nothing -- https://github.com/madgen/madgen.github.io/issues/1
  , toAuthor
    "Monday Morning Haskell"
    "https://mmhaskell.com"
    (Just "https://mmhaskell.com/blog?format=rss")
  , toAuthor
    "Nuno Alexandre"
    "https://nunoalexandre.com"
    (Just "https://nunoalexandre.com/feed.xml")
  , toAuthor
    "Oskar Wickstr\xf6m"
    "https://wickstrom.tech"
    (Just "https://wickstrom.tech/feed.xml")
  , toAuthor
    "Ryan Scott"
    "https://ryanglscott.github.io"
    (Just "https://ryanglscott.github.io/feed.xml")
  , toAuthor
    "Sandy Maguire"
    "http://reasonablypolymorphic.com"
    (Just "http://reasonablypolymorphic.com/atom.xml")
  , toAuthor
    "Siddharth Bhat"
    "https://pixel-druid.com"
    Nothing -- https://github.com/bollu/pixeldruid/issues/1
  , toAuthor
    "Stackage"
    "https://www.stackage.org"
    (Just "https://www.stackage.org/blog/feed")
  , toAuthor
    "Taylor Fausak"
    "http://taylor.fausak.me"
    (Just "http://taylor.fausak.me/sitemap.atom")
  , toAuthor
    "Tom Smalley"
    "https://tomsmalley.github.io"
    Nothing
  , toAuthor
    "Tweag I/O"
    "https://www.tweag.io"
    (Just "https://www.tweag.io/rss.xml")
  , toAuthor
    "Vaibhav Sagar"
    "http://vaibhavsagar.com"
    (Just "http://vaibhavsagar.com/atom.xml")
  ]

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
        [ (Http.hUserAgent, Text.encodeUtf8 (Text.pack userAgent))
        ]
      }
  response <- Client.httpLbs request manager

  feed <- case Feed.parseFeedSource (Client.responseBody response) of
    Nothing -> fail "invalid feed"
    Just feed -> pure feed

  items <- case mapM toItem (Feed.feedItems feed) of
    Nothing -> fail "invalid item"
    Just items -> pure items

  pure (Set.fromList items)

data Author = Author
  { authorName :: Name
  , authorUrl :: Url
  , authorFeed :: Maybe Url
  } deriving (Eq, Ord, Show)

toAuthor :: String -> String -> Maybe String -> Author
toAuthor name url feed = Author
  { authorName = toName name
  , authorUrl = toUrl url
  , authorFeed = fmap toUrl feed
  }

data Item = Item
  { itemName :: Name
  , itemUrl :: Url
  , itemTime :: Time.UTCTime
  } deriving (Eq, Ord, Show)

toItem :: Feed.Item -> Maybe Item
toItem feedItem = do
  name <- Feed.getItemTitle feedItem
  url <- Feed.getItemLink feedItem
  maybeTime <- Feed.getItemPublishDate feedItem
  time <- maybeTime
  pure Item
    { itemName = Name name
    , itemUrl = Url url
    , itemTime = time
    }

itemToEntry :: (Author, Item) -> Xml.Node
itemToEntry (author, item) =
  let url = fromUrl (itemUrl item)
  in xmlNode "entry" []
    [ xmlNode "title" [] [Xml.NodeContent (unwrapName (itemName item))]
    , xmlNode "id" [] [xmlContent url]
    , xmlNode "updated" [] [xmlContent (rfc3339 (itemTime item))]
    , xmlNode "link" [("href", url)] []
    , xmlNode "author" []
      [ xmlNode "name" [] [Xml.NodeContent (unwrapName (authorName author))]
      , xmlNode "uri" [] [Xml.NodeContent (unwrapUrl (authorUrl author))]
      ]
    ]

newtype Name = Name
  { unwrapName :: Text.Text
  } deriving (Eq, Ord, Show)

toName :: String -> Name
toName string = Name (Text.pack string)

fromName :: Name -> String
fromName name = Text.unpack (unwrapName name)

newtype Url = Url
  { unwrapUrl :: Text.Text
  } deriving (Eq, Ord, Show)

toUrl :: String -> Url
toUrl string = Url (Text.pack string)

fromUrl :: Url -> String
fromUrl url = Text.unpack (unwrapUrl url)

newtype Database = Database
  { unwrapDatabase :: Map.Map Author (Set.Set Item)
  } deriving (Eq, Ord, Show)

initialDatabase :: Database
initialDatabase = Database Map.empty

updateDatabase :: Author -> Set.Set Item -> Database -> Database
updateDatabase author items database = Database (Map.insertWith Set.union author items (unwrapDatabase database))

getRecentDatabaseItems :: Database -> Time.UTCTime -> [(Author, Item)]
getRecentDatabaseItems database now =
  let twoWeeksAgo = Time.addUTCTime (-14 * Time.nominalDay) now
  in database
    & unwrapDatabase
    & Map.toList
    & concatMap (\ (author, items) -> items
      & Set.toList
      & Maybe.mapMaybe (\ item ->
        if itemTime item > now then
          Nothing
        else if itemTime item < twoWeeksAgo then
          Nothing
        else
          Just (author, item)))
    & List.sortBy (Ord.comparing (\ (_, item) -> Ord.Down (itemTime item)))
