module Main
  ( main
  ) where

import qualified Control.Concurrent as Concurrent
import qualified Control.Concurrent.Async as Async
import qualified Control.Concurrent.STM as Stm
import qualified Control.Exception as Exception
import qualified Control.Monad as Monad
import qualified Data.ByteString.Lazy as LazyBytes
import qualified Data.Foldable as Foldable
import qualified Data.List as List
import qualified Data.Map as Map
import qualified Data.Ord as Ord
import qualified Data.Set as Set
import qualified Data.Text as Text
import qualified Data.Text.Encoding as Text
import qualified Data.Time as Time
import qualified Network.HTTP.Client as Client
import qualified Network.HTTP.Client.TLS as Client
import qualified Network.HTTP.Types as Http
import qualified Network.Wai as Wai
import qualified Network.Wai.Handler.Warp as Warp
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

    items <- Exception.catch
      (getAuthorItems manager author)
      (\ exception -> do
        print (exception :: Exception.IOException)
        pure Set.empty)
    Stm.atomically (Stm.modifyTVar database (updateDatabase author items))

    Foldable.for_ items (\ item -> do
      Printf.printf "  - %s: %s\n"
        (rfc3339 (itemTime item))
        (fromName (itemName item))))

  sleep 60)

sleep :: Int -> IO ()
sleep seconds = Concurrent.threadDelay (seconds * 1000000)

startServer :: Stm.TVar Database -> IO ()
startServer database = Warp.run 3000 (\ request respond -> do
  let method = Text.unpack (Text.decodeUtf8 (Wai.requestMethod request))
  let path = map Text.unpack (Wai.pathInfo request)
  case (method, path) of
    ("GET", ["feed.atom"]) -> do
      db <- Stm.readTVarIO database
      let items = getAllDatabaseItems db
      let entries = map itemToEntry items
      now <- Time.getCurrentTime
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
    ("GET", []) -> respond (Wai.responseLBS
      Http.ok200
      [(Http.hContentType, Text.encodeUtf8 (Text.pack "text/html; charset=utf-8"))]
      (LazyBytes.fromStrict (Text.encodeUtf8 (Text.pack (unlines
        [ "<!doctype html>"
        , "<html>"
        , "  <head>"
        , "    <meta charset=\"utf-8\">"
        , "    <title>Haskell Weekly</title>"
        , "  </head>"
        , "  <body>"
        , "    <h1>Haskell Weekly</h1>"
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
    "Taylor Fausak"
    "http://taylor.fausak.me"
    (Just "http://taylor.fausak.me/sitemap.atom")
  , toAuthor
    "FP Complete"
    "https://www.fpcomplete.com"
    (Just "https://www.fpcomplete.com/blog/atom.xml")
  , toAuthor
    "Ben Gamari"
    "https://tomsmalley.github.io"
    Nothing
  , toAuthor
    "Gabriel Gonzalez"
    "http://www.haskellforall.com"
    (Just "http://www.haskellforall.com/feeds/posts/default")
  , toAuthor
    "Tweag I/O"
    "https://www.tweag.io"
    (Just "https://www.tweag.io/rss.xml")
  , toAuthor
    "Vaibhav Sagar"
    "http://vaibhavsagar.com"
    (Just "http://vaibhavsagar.com/atom.xml")
  , toAuthor
    "Joachim Breitner"
    "https://www.joachim-breitner.de"
    (Just "https://www.joachim-breitner.de/blog_feed.rss")
  , toAuthor
    "Nuno Alexandre"
    "https://nunoalexandre.com"
    (Just "https://nunoalexandre.com/feed.xml")
  , toAuthor
    "Andre Van Der Merwe"
    "http://www.andrevdm.com/"
    (Just "http://www.andrevdm.com/atom.xml")
  , toAuthor
    "Tom Smalley"
    "https://tomsmalley.github.io"
    Nothing
  , toAuthor
    "Ibnu D. Aji"
    "https://ibnuda.gitlab.io"
    Nothing -- https://gitlab.com/ibnuda/ibnuda.gitlab.io/issues/2
  ]

getAuthorItems :: Client.Manager -> Author -> IO (Set.Set Item)
getAuthorItems manager author = do
  url <- case authorFeed author of
    Nothing -> fail "no feed"
    Just url -> pure url

  request <- Client.parseUrlThrow (fromUrl url)
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

getAllDatabaseItems :: Database -> [(Author, Item)]
getAllDatabaseItems database = List.sortBy
  (Ord.comparing (\ (_, item) -> Ord.Down (itemTime item)))
  (concatMap
    (\ (author, items) -> map (\ item -> (author, item)) (Set.toList items))
    (Map.toList (unwrapDatabase database)))
