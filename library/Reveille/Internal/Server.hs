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
import qualified Reveille.Internal.Author as Reveille
import qualified Reveille.Internal.Database as Reveille
import qualified Reveille.Internal.Entry as Reveille
import qualified Reveille.Internal.Item as Reveille
import qualified Reveille.Internal.Name as Reveille
import qualified Reveille.Internal.Unicode as Reveille
import qualified Reveille.Internal.Url as Reveille
import qualified Text.Printf as Printf
import qualified Text.XML as Xml

startServer :: Stm.TVar Reveille.Database -> IO ()
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
  (Either.fromRight "?" (Reveille.fromUtf8 (Wai.requestMethod request)))
  (Either.fromRight "?" (Reveille.fromUtf8 (Wai.rawPathInfo request)))
  (Either.fromRight "?" (Reveille.fromUtf8 (Wai.rawQueryString request)))
  (Http.statusCode status)

onExceptionResponse :: Exception.SomeException -> Wai.Response
onExceptionResponse _ =
  Wai.responseLBS Http.internalServerError500 [] LazyBytes.empty

port :: Warp.Port
port = 8080

serverName :: Bytes.ByteString
serverName = Bytes.empty

makeApplication :: Stm.TVar Reveille.Database -> Wai.Application
makeApplication database request respond = do
  db <- Stm.readTVarIO database
  now <- Time.getCurrentTime
  let response = routeRequest request db now
  respond response

routeRequest
  :: Wai.Request -> Reveille.Database -> Time.UTCTime -> Wai.Response
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
  Either.fromRight "?" (Reveille.fromUtf8 (Wai.requestMethod request))

requestPath :: Wai.Request -> [String]
requestPath request = map Text.unpack (Wai.pathInfo request)

getIndexHandler :: Reveille.Database -> Time.UTCTime -> Wai.Response
getIndexHandler database now
  = let
      items = getRecentDatabaseEntries database now
      authors = Reveille.getDatabaseAuthors database
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
                [ ( "href"
                  , Reveille.fromUrl
                    (Reveille.itemUrl (Reveille.entryItem entry))
                  )
                ]
                [ xmlContent
                    (Reveille.fromName
                      (Reveille.itemName (Reveille.entryItem entry))
                    )
                ]
              , xmlContent " by "
              , xmlContent
                (Reveille.fromName
                  (Reveille.authorName (Reveille.entryAuthor entry))
                )
              , xmlContent " on "
              , xmlContent
                (Time.formatTime
                  Time.defaultTimeLocale
                  "%B %-e"
                  (Reveille.itemTime (Reveille.entryItem entry))
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
                  [("href", Reveille.fromUrl (Reveille.authorUrl author))]
                  [xmlContent (Reveille.fromName (Reveille.authorName author))]
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
      [(Http.hContentType, Reveille.toUtf8 "text/html; charset=utf-8")]
      body

getAuthorsHandler :: Reveille.Database -> Wai.Response
getAuthorsHandler database =
  let
    authors = Reveille.getDatabaseAuthors database
    status = Http.ok200
    headers = [(Http.hContentType, Reveille.toUtf8 "text/xml")]
    opmlHead = xmlNode "head" [] []
    makeOutline author feed = xmlNode
      "outline"
      [ ("text", Reveille.fromName (Reveille.authorName author))
      , ("type", "rss")
      , ("xmlUrl", Reveille.fromUrl feed)
      ]
      []
    toOutline author = do
      feed <- Reveille.authorFeed author
      pure (makeOutline author feed)
    opmlBody =
      xmlNode "body" [] (Maybe.mapMaybe toOutline (Set.toAscList authors))
    opml = xmlElement "opml" [("version", "1.0")] [opmlHead, opmlBody]
    document = xmlDocument opml
    body = Xml.renderLBS Xml.def document
  in Wai.responseLBS status headers body

getFaviconHandler :: Wai.Response
getFaviconHandler = Wai.responseLBS Http.ok200 [] favicon

getFeedHandler :: Reveille.Database -> Time.UTCTime -> Wai.Response
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
    [(Http.hContentType, Reveille.toUtf8 "application/atom+xml")]
    (Xml.renderLBS Xml.def document)

getHealthCheckHandler :: Wai.Response
getHealthCheckHandler = Wai.responseLBS Http.ok200 [] LazyBytes.empty

getRobotsHandler :: Wai.Response
getRobotsHandler = Wai.responseLBS
  Http.ok200
  []
  (LazyBytes.fromStrict
    (Reveille.toUtf8 (unlines ["User-Agent: *", "Disallow:"]))
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

entryToXml :: Reveille.Entry -> Xml.Node
entryToXml entry
  = let
      url = Reveille.fromUrl (Reveille.itemUrl (Reveille.entryItem entry))
      title = xmlNode
        "title"
        []
        [ xmlContent
            (Reveille.fromName (Reveille.itemName (Reveille.entryItem entry)))
        ]
      id_ = xmlNode "id" [] [xmlContent url]
      updated = xmlNode
        "updated"
        []
        [xmlContent (rfc3339 (Reveille.itemTime (Reveille.entryItem entry)))]
      link = xmlNode "link" [("href", url)] []
      author = xmlNode
        "author"
        []
        [ xmlNode
          "name"
          []
          [ xmlContent
              (Reveille.fromName
                (Reveille.authorName (Reveille.entryAuthor entry))
              )
          ]
        , xmlNode
          "uri"
          []
          [ xmlContent
              (Reveille.fromUrl
                (Reveille.authorUrl (Reveille.entryAuthor entry))
              )
          ]
        ]
    in xmlNode "entry" [] [title, id_, updated, link, author]

getRecentDatabaseEntries
  :: Reveille.Database -> Time.UTCTime -> [Reveille.Entry]
getRecentDatabaseEntries database now =
  sortEntriesByTime (filterItems now (Reveille.getDatabaseEntries database))

filterItems :: Time.UTCTime -> Set.Set Reveille.Entry -> Set.Set Reveille.Entry
filterItems now entries =
  Set.filter (\entry -> isNotTooNew now entry && isNotTooOld now entry) entries

isNotTooNew :: Time.UTCTime -> Reveille.Entry -> Bool
isNotTooNew now entry = Reveille.itemTime (Reveille.entryItem entry) <= now

isNotTooOld :: Time.UTCTime -> Reveille.Entry -> Bool
isNotTooOld now entry =
  Reveille.itemTime (Reveille.entryItem entry) > twoWeeksBefore now

sortEntriesByTime :: Set.Set Reveille.Entry -> [Reveille.Entry]
sortEntriesByTime items = List.sortBy compareEntriesByTime (Set.toList items)

compareEntriesByTime :: Reveille.Entry -> Reveille.Entry -> Ordering
compareEntriesByTime = Ord.comparing
  (\entry -> Ord.Down (Reveille.itemTime (Reveille.entryItem entry)))

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
