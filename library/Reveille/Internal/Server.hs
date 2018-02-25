module Reveille.Internal.Server where

import qualified Control.Concurrent.STM as Stm
import qualified Control.Exception as Exception
import qualified Data.ByteString as Bytes
import qualified Data.ByteString.Lazy as LazyBytes
import qualified Data.Either as Either
import qualified Data.List as List
import qualified Data.Map as Map
import qualified Data.Maybe as Maybe
import qualified Data.Ord as Ord
import qualified Data.Set as Set
import qualified Data.Text as Text
import qualified Data.Time as Time
import qualified Network.HTTP.Types as Http
import qualified Network.Wai as Wai
import qualified Network.Wai.Handler.Warp as Warp
import qualified Reveille.Internal.Author as Author
import qualified Reveille.Internal.Database as Database
import qualified Reveille.Internal.Entry as Entry
import qualified Reveille.Internal.Item as Item
import qualified Reveille.Internal.Name as Name
import qualified Reveille.Internal.Unicode as Unicode
import qualified Reveille.Internal.Url as Url
import qualified Text.Printf as Printf
import qualified Text.XML as Xml

startServer :: Stm.TVar Database.Database -> IO ()
startServer database =
  Warp.runSettings serverSettings (makeApplication database)

serverSettings :: Warp.Settings
serverSettings = Warp.setBeforeMainLoop
  beforeMainLoop
  (Warp.setLogger
    logger
    (Warp.setOnExceptionResponse
      onExceptionResponse
      (Warp.setPort port (Warp.setServerName serverName Warp.defaultSettings))
    )
  )

beforeMainLoop :: IO ()
beforeMainLoop = putStrLn "Starting server ..."

logger :: Wai.Request -> Http.Status -> Maybe Integer -> IO ()
logger request status _ = Printf.printf
  "%s %s%s %d\n"
  (Either.fromRight "?" (Unicode.fromUtf8 (Wai.requestMethod request)))
  (Either.fromRight "?" (Unicode.fromUtf8 (Wai.rawPathInfo request)))
  (Either.fromRight "?" (Unicode.fromUtf8 (Wai.rawQueryString request)))
  (Http.statusCode status)

onExceptionResponse :: Exception.SomeException -> Wai.Response
onExceptionResponse _ =
  Wai.responseLBS Http.internalServerError500 [] LazyBytes.empty

port :: Warp.Port
port = 8080

serverName :: Bytes.ByteString
serverName = Bytes.empty

makeApplication :: Stm.TVar Database.Database -> Wai.Application
makeApplication database request respond = do
  db <- Stm.readTVarIO database
  now <- Time.getCurrentTime
  let response = routeRequest request db now
  respond response

routeRequest
  :: Wai.Request -> Database.Database -> Time.UTCTime -> Wai.Response
routeRequest request database now =
  case (requestMethod request, requestPath request) of
    ("GET", []) -> getIndexHandler database now
    ("GET", ["authors.opml"]) -> getAuthorsHandler database
    ("GET", ["favicon.ico"]) -> getFaviconHandler
    ("GET", ["feed.atom"]) -> getFeedHandler database now
    ("GET", ["health-check"]) -> getHealthCheckHandler
    ("GET", ["robots.txt"]) -> getRobotsHandler
    _ -> defaultHandler

requestMethod :: Wai.Request -> String
requestMethod request =
  Either.fromRight "?" (Unicode.fromUtf8 (Wai.requestMethod request))

requestPath :: Wai.Request -> [String]
requestPath request = map Text.unpack (Wai.pathInfo request)

getIndexHandler :: Database.Database -> Time.UTCTime -> Wai.Response
getIndexHandler database now
  = let
      items = getRecentDatabaseEntries database now
      authors = Database.getDatabaseAuthors database
      settings = Xml.def { Xml.rsXMLDeclaration = False }
      doctype = Xml.Doctype (Text.pack "html") Nothing
      prologue = Xml.Prologue [] (Just doctype) []
      htmlHead = xmlNode
        "head"
        []
        [ xmlNode "meta" [("charset", "utf-8")] []
        , xmlNode "title" [] [xmlContent "Haskell Weekly"]
        , xmlNode
          "link"
          [ ("rel", "alternate")
          , ("type", "application/atom+xml")
          , ("href", "feed.atom")
          ]
          []
        ]
      htmlBody = xmlNode
        "body"
        []
        [ xmlNode "h1" [] [xmlContent "Haskell Weekly"]
        , xmlNode
          "p"
          []
          [ xmlContent "Subscribe to "
          , xmlNode "a" [("href", "feed.atom")] [xmlContent "the Atom feed"]
          , xmlContent "."
          ]
        , xmlNode
          "ol"
          []
          (map
            (\entry -> xmlNode
              "li"
              []
              [ xmlNode
                "a"
                [("href", Url.fromUrl (Item.itemUrl (Entry.entryItem entry)))]
                [ xmlContent
                    (Name.fromName (Item.itemName (Entry.entryItem entry)))
                ]
              , xmlContent " by "
              , xmlContent
                (Name.fromName (Author.authorName (Entry.entryAuthor entry)))
              , xmlContent " on "
              , xmlContent
                (Time.formatTime
                  Time.defaultTimeLocale
                  "%B %-e"
                  (Item.itemTime (Entry.entryItem entry))
                )
              ]
            )
            items
          )
        , xmlNode "h2" [] [xmlContent "Authors"]
        , xmlNode
          "ul"
          []
          (map
            (\author -> xmlNode
              "li"
              []
              [ xmlNode
                  "a"
                  [("href", Url.fromUrl (Author.authorUrl author))]
                  [xmlContent (Name.fromName (Author.authorName author))]
              ]
            )
            (Set.toAscList authors)
          )
        ]
      html = xmlElement "html" [] [htmlHead, htmlBody]
      document = Xml.Document prologue html []
      body = Xml.renderLBS settings document
    in Wai.responseLBS
      Http.ok200
      [(Http.hContentType, Unicode.toUtf8 "text/html; charset=utf-8")]
      body

getAuthorsHandler :: Database.Database -> Wai.Response
getAuthorsHandler database =
  let
    authors = Database.getDatabaseAuthors database
    status = Http.ok200
    headers = [(Http.hContentType, Unicode.toUtf8 "text/xml")]
    opmlHead = xmlNode "head" [] []
    makeOutline author feed = xmlNode
      "outline"
      [ ("text", Name.fromName (Author.authorName author))
      , ("type", "rss")
      , ("xmlUrl", Url.fromUrl feed)
      ]
      []
    toOutline author = do
      feed <- Author.authorFeed author
      pure (makeOutline author feed)
    opmlBody =
      xmlNode "body" [] (Maybe.mapMaybe toOutline (Set.toAscList authors))
    opml = xmlElement "opml" [("version", "1.0")] [opmlHead, opmlBody]
    document = xmlDocument opml
    body = Xml.renderLBS Xml.def document
  in Wai.responseLBS status headers body

getFaviconHandler :: Wai.Response
getFaviconHandler = Wai.responseLBS Http.ok200 [] favicon

getFeedHandler :: Database.Database -> Time.UTCTime -> Wai.Response
getFeedHandler database now =
  let
    title = xmlNode "title" [] [xmlContent "Haskell Weekly"]
    id_ = xmlNode "id" [] [xmlContent "https://haskellweekly.news/"]
    updated = xmlNode "updated" [] [xmlContent (rfc3339 now)]
    link = xmlNode
      "link"
      [("rel", "self"), ("href", "http://localhost:3000/feed.atom")]
      []
    items = getRecentDatabaseEntries database now
    entries = map entryToXml items
    feed = xmlElement
      "feed"
      [("xmlns", "http://www.w3.org/2005/Atom")]
      (title : id_ : updated : link : entries)
    document = xmlDocument feed
  in Wai.responseLBS
    Http.ok200
    [(Http.hContentType, Unicode.toUtf8 "application/atom+xml")]
    (Xml.renderLBS Xml.def document)

getHealthCheckHandler :: Wai.Response
getHealthCheckHandler = Wai.responseLBS Http.ok200 [] LazyBytes.empty

getRobotsHandler :: Wai.Response
getRobotsHandler = Wai.responseLBS
  Http.ok200
  []
  (LazyBytes.fromStrict
    (Unicode.toUtf8 (unlines ["User-Agent: *", "Disallow:"]))
  )

defaultHandler :: Wai.Response
defaultHandler = Wai.responseLBS Http.notFound404 [] LazyBytes.empty

xmlName :: String -> Xml.Name
xmlName string = Xml.Name (Text.pack string) Nothing Nothing

xmlElement :: String -> [(String, String)] -> [Xml.Node] -> Xml.Element
xmlElement name attributes children = Xml.Element
  (xmlName name)
  (Map.fromList (map (\(k, v) -> (xmlName k, Text.pack v)) attributes))
  children

xmlNode :: String -> [(String, String)] -> [Xml.Node] -> Xml.Node
xmlNode name attributes children =
  Xml.NodeElement (xmlElement name attributes children)

xmlContent :: String -> Xml.Node
xmlContent string = Xml.NodeContent (Text.pack string)

xmlDocument :: Xml.Element -> Xml.Document
xmlDocument element = Xml.Document (Xml.Prologue [] Nothing []) element []

rfc3339 :: Time.UTCTime -> String
rfc3339 time =
  Time.formatTime Time.defaultTimeLocale "%Y-%m-%dT%H:%M:%S%QZ" time

entryToXml :: Entry.Entry -> Xml.Node
entryToXml entry
  = let
      url = Url.fromUrl (Item.itemUrl (Entry.entryItem entry))
      title = xmlNode
        "title"
        []
        [xmlContent (Name.fromName (Item.itemName (Entry.entryItem entry)))]
      id_ = xmlNode "id" [] [xmlContent url]
      updated = xmlNode
        "updated"
        []
        [xmlContent (rfc3339 (Item.itemTime (Entry.entryItem entry)))]
      link = xmlNode "link" [("href", url)] []
      author = xmlNode
        "author"
        []
        [ xmlNode
          "name"
          []
          [ xmlContent
              (Name.fromName (Author.authorName (Entry.entryAuthor entry)))
          ]
        , xmlNode
          "uri"
          []
          [ xmlContent
              (Url.fromUrl (Author.authorUrl (Entry.entryAuthor entry)))
          ]
        ]
    in xmlNode "entry" [] [title, id_, updated, link, author]

getRecentDatabaseEntries :: Database.Database -> Time.UTCTime -> [Entry.Entry]
getRecentDatabaseEntries database now =
  sortEntriesByTime (filterItems now (Database.getDatabaseEntries database))

filterItems :: Time.UTCTime -> Set.Set Entry.Entry -> Set.Set Entry.Entry
filterItems now entries =
  Set.filter (\entry -> isNotTooNew now entry && isNotTooOld now entry) entries

isNotTooNew :: Time.UTCTime -> Entry.Entry -> Bool
isNotTooNew now entry = Item.itemTime (Entry.entryItem entry) <= now

isNotTooOld :: Time.UTCTime -> Entry.Entry -> Bool
isNotTooOld now entry =
  Item.itemTime (Entry.entryItem entry) > twoWeeksBefore now

sortEntriesByTime :: Set.Set Entry.Entry -> [Entry.Entry]
sortEntriesByTime items = List.sortBy compareEntriesByTime (Set.toList items)

compareEntriesByTime :: Entry.Entry -> Entry.Entry -> Ordering
compareEntriesByTime =
  Ord.comparing (\entry -> Ord.Down (Item.itemTime (Entry.entryItem entry)))

twoWeeksBefore :: Time.UTCTime -> Time.UTCTime
twoWeeksBefore time = Time.addUTCTime (-twoWeeks) time

twoWeeks :: Time.NominalDiffTime
twoWeeks = 14 * Time.nominalDay

-- This is pretty ridiculous.
favicon :: LazyBytes.ByteString
favicon = LazyBytes.pack
  [ 0x00
  , 0x00
  , 0x01
  , 0x00
  , 0x01
  , 0x00
  , 0x10
  , 0x10
  , 0x02
  , 0x00
  , 0x01
  , 0x00
  , 0x01
  , 0x00
  , 0xb0
  , 0x00
  , 0x00
  , 0x00
  , 0x16
  , 0x00
  , 0x00
  , 0x00
  , 0x28
  , 0x00
  , 0x00
  , 0x00
  , 0x10
  , 0x00
  , 0x00
  , 0x00
  , 0x20
  , 0x00
  , 0x00
  , 0x00
  , 0x01
  , 0x00
  , 0x01
  , 0x00
  , 0x00
  , 0x00
  , 0x00
  , 0x00
  , 0x40
  , 0x00
  , 0x00
  , 0x00
  , 0x00
  , 0x00
  , 0x00
  , 0x00
  , 0x00
  , 0x00
  , 0x00
  , 0x00
  , 0x02
  , 0x00
  , 0x00
  , 0x00
  , 0x02
  , 0x00
  , 0x00
  , 0x00
  , 0x79
  , 0x48
  , 0x70
  , 0x00
  , 0x00
  , 0x00
  , 0x00
  , 0x00
  , 0x00
  , 0x00
  , 0x00
  , 0x00
  , 0x00
  , 0x00
  , 0x00
  , 0x00
  , 0x00
  , 0x00
  , 0x00
  , 0x00
  , 0x00
  , 0x00
  , 0x00
  , 0x00
  , 0x00
  , 0x00
  , 0x00
  , 0x00
  , 0x00
  , 0x00
  , 0x00
  , 0x00
  , 0x00
  , 0x00
  , 0x00
  , 0x00
  , 0x00
  , 0x00
  , 0x00
  , 0x00
  , 0x00
  , 0x00
  , 0x00
  , 0x00
  , 0x00
  , 0x00
  , 0x00
  , 0x00
  , 0x00
  , 0x00
  , 0x00
  , 0x00
  , 0x00
  , 0x00
  , 0x00
  , 0x00
  , 0x00
  , 0x00
  , 0x00
  , 0x00
  , 0x00
  , 0x00
  , 0x00
  , 0x00
  , 0x00
  , 0x00
  , 0x00
  , 0x00
  , 0x00
  , 0x00
  , 0x00
  , 0x00
  , 0x00
  , 0x00
  , 0x00
  , 0x00
  , 0x00
  , 0x00
  , 0x00
  , 0x00
  , 0x00
  , 0x00
  , 0x00
  , 0x00
  , 0x00
  , 0x00
  , 0x00
  , 0x00
  , 0x00
  , 0x00
  , 0x00
  , 0x00
  , 0x00
  , 0x00
  , 0x00
  , 0x00
  , 0x00
  , 0x00
  , 0x00
  , 0x00
  , 0x00
  , 0x00
  , 0x00
  , 0x00
  , 0x00
  , 0x00
  , 0x00
  , 0x00
  , 0x00
  , 0x00
  , 0x00
  , 0x00
  , 0x00
  , 0x00
  , 0x00
  , 0x00
  , 0x00
  , 0x00
  , 0x00
  , 0x00
  , 0x00
  , 0x00
  , 0x00
  , 0x00
  ]
