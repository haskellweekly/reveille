module Reveille
  ( defaultMain
  ) where

import Reveille.Author (Author(authorName, authorUrl, authorFeed))
import Reveille.Authors (authors)
import Reveille.Database (Database, initialDatabase, updateDatabase, getRecentDatabaseItems)
import Reveille.Item (Item(itemName, itemUrl, itemTime), toItem)
import Reveille.Name (fromName)
import Reveille.Url (fromUrl)

import qualified Control.Concurrent as Concurrent
import qualified Control.Concurrent.Async as Async
import qualified Control.Concurrent.STM as Stm
import qualified Control.Exception as Exception
import qualified Control.Monad as Monad
import qualified Data.ByteString.Lazy as LazyBytes
import qualified Data.Foldable as Foldable
import qualified Data.List as List
import qualified Data.Map as Map
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
import qualified Text.Printf as Printf
import qualified Text.XML as Xml

defaultMain :: IO ()
defaultMain = do
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
    Left _ -> fail "invalid item"
    Right items -> pure items

  pure (Set.fromList items)

itemToEntry :: (Author, Item) -> Xml.Node
itemToEntry (author, item) =
  let url = fromUrl (itemUrl item)
  in xmlNode "entry" []
    [ xmlNode "title" [] [xmlContent (fromName (itemName item))]
    , xmlNode "id" [] [xmlContent url]
    , xmlNode "updated" [] [xmlContent (rfc3339 (itemTime item))]
    , xmlNode "link" [("href", url)] []
    , xmlNode "author" []
      [ xmlNode "name" [] [xmlContent (fromName (authorName author))]
      , xmlNode "uri" [] [xmlContent (fromUrl (authorUrl author))]
      ]
    ]
