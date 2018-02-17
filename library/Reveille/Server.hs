module Reveille.Server
  ( startServer
  ) where

import Data.Function ((&))
import Reveille.Author (Author, authorName, authorUrl)
import Reveille.Authors (authors)
import Reveille.Database (Database, getRecentDatabaseItems)
import Reveille.Item (Item, itemName, itemUrl, itemTime)
import Reveille.Name (fromName)
import Reveille.Url (fromUrl)

import qualified Control.Concurrent.STM as Stm
import qualified Data.ByteString as Bytes
import qualified Data.ByteString.Lazy as LazyBytes
import qualified Data.Map as Map
import qualified Data.Set as Set
import qualified Data.Text as Text
import qualified Data.Text.Encoding as Text
import qualified Data.Time as Time
import qualified Network.HTTP.Types as Http
import qualified Network.Wai as Wai
import qualified Network.Wai.Handler.Warp as Warp
import qualified Text.Printf as Printf
import qualified Text.XML as Xml

startServer :: Stm.TVar Database -> IO ()
startServer database = Warp.runSettings settings (makeApplication database)

settings :: Warp.Settings
settings = Warp.defaultSettings
  & Warp.setBeforeMainLoop (putStrLn "Starting server ...")
  & Warp.setLogger (\ request status _ -> Printf.printf
    "%s %s%s %d\n"
    (Text.unpack (Text.decodeUtf8 (Wai.requestMethod request)))
    (Text.unpack (Text.decodeUtf8 (Wai.rawPathInfo request)))
    (Text.unpack (Text.decodeUtf8 (Wai.rawQueryString request)))
    (Http.statusCode status))
  & Warp.setOnExceptionResponse (\ _ ->
    Wai.responseLBS Http.internalServerError500 [] LazyBytes.empty)
  & Warp.setPort 8080
  & Warp.setServerName Bytes.empty

makeApplication :: Stm.TVar Database -> Wai.Application
makeApplication database request respond = do
  let method = Text.unpack (Text.decodeUtf8 (Wai.requestMethod request))
  let path = map Text.unpack (Wai.pathInfo request)
  response <- case (method, path) of
    ("GET", ["feed.atom"]) -> getFeedHandler database
    ("GET", ["health-check"]) -> getHealthCheckHandler
    ("GET", ["robots.txt"]) -> getRobotsHandler
    ("GET", ["favicon.ico"]) -> getFaviconHandler
    ("GET", []) -> getIndexHandler database
    _ -> defaultHandler
  respond response

getFeedHandler :: Stm.TVar Database -> IO Wai.Response
getFeedHandler database = do
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
  pure (Wai.responseLBS
    Http.ok200
    [(Http.hContentType, Text.encodeUtf8 (Text.pack "application/atom+xml"))]
    (Xml.renderLBS Xml.def document))

getHealthCheckHandler :: Applicative m => m Wai.Response
getHealthCheckHandler = pure (Wai.responseLBS Http.ok200 [] LazyBytes.empty)

getRobotsHandler :: Applicative m => m Wai.Response
getRobotsHandler = pure (Wai.responseLBS
  Http.ok200
  []
  (LazyBytes.fromStrict (Text.encodeUtf8 (Text.pack (unlines
    [ "User-Agent: *"
    , "Disallow:"
    ])))))

getFaviconHandler :: Applicative m => m Wai.Response
getFaviconHandler = pure (Wai.responseLBS
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

getIndexHandler :: Stm.TVar Database -> IO Wai.Response
getIndexHandler database = do
  db <- Stm.readTVarIO database
  now <- Time.getCurrentTime
  let items = getRecentDatabaseItems db now
  let
    xmlSettings = Xml.def { Xml.rsXMLDeclaration = False }
    doctype = Xml.Doctype (Text.pack "html") Nothing
    prologue = Xml.Prologue [] (Just doctype) []
    htmlHead = xmlNode "head" []
      [ xmlNode "meta" [("charset", "utf-8")] []
      , xmlNode "title" [] [xmlContent "Haskell Weekly"]
      , xmlNode "link"
        [ ("rel", "alternate")
        , ("type", "application/atom+xml")
        , ("href", "feed.atom")
        ] []
      ]
    htmlBody = xmlNode "body" []
      [ xmlNode "h1" [] [xmlContent "Haskell Weekly"]
      , xmlNode "p" []
        [ xmlContent "Subscribe to "
        , xmlNode "a" [("href", "feed.atom")] [xmlContent "the Atom feed"]
        , xmlContent "."
        ]
      , xmlNode "ol" [] (map
        (\ (author, item) -> xmlNode "li" []
          [ xmlNode "a" [("href", fromUrl (itemUrl item))] [xmlContent (fromName (itemName item))]
          , xmlContent " by "
          , xmlContent (fromName (authorName author))
          , xmlContent " on "
          , xmlContent (Time.formatTime Time.defaultTimeLocale "%B %-e" (itemTime item))
          ])
        items)
      , xmlNode "h2" [] [xmlContent "Authors"]
      , xmlNode "ul" [] (map
        (\ author -> xmlNode "li" []
          [ xmlNode "a"
            [("href", fromUrl (authorUrl author))]
            [xmlContent (fromName (authorName author))]
          ])
        (Set.toAscList authors))
      ]
    html = xmlElement "html" [] [htmlHead, htmlBody]
    document = Xml.Document prologue html []
    body = Xml.renderLBS xmlSettings document
  pure (Wai.responseLBS
    Http.ok200
    [(Http.hContentType, Text.encodeUtf8 (Text.pack "text/html; charset=utf-8"))]
    body)

defaultHandler :: Applicative m => m Wai.Response
defaultHandler = pure (Wai.responseLBS Http.notFound404 [] LazyBytes.empty)

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
