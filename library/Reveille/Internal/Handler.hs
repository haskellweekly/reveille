module Reveille.Internal.Handler where

import qualified Data.Aeson as Aeson
import qualified Data.ByteString.Lazy as LazyBytes
import qualified Data.List as List
import qualified Data.Map as Map
import qualified Data.Maybe as Maybe
import qualified Data.Ord as Ord
import qualified Data.Set as Set
import qualified Data.Text as Text
import qualified Data.Time as Time
import qualified JsonFeed
import qualified Network.HTTP.Types as Http
import qualified Network.URI as Uri
import qualified Network.Wai as Wai
import qualified Reveille.Internal.Author as Author
import qualified Reveille.Internal.Database as Database
import qualified Reveille.Internal.Entry as Entry
import qualified Reveille.Internal.Favicon as Favicon
import qualified Reveille.Internal.Item as Item
import qualified Reveille.Internal.Name as Name
import qualified Reveille.Internal.Unicode as Unicode
import qualified Reveille.Internal.Url as Url
import qualified Text.XML as Xml

getIndexHandler :: Database.Database -> Time.UTCTime -> Wai.Response
getIndexHandler database now
  = let
      items = getRecentDatabaseEntries database now
      authors = Database.getDatabaseAuthors database
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
          [ xmlContent "Subscribe to the "
          , xmlNode "a" [("href", "feed.atom")] [xmlContent "Atom feed"]
          , xmlContent "."
          ]
        , xmlNode "ol" [("class", "entries")] (map entryToHtml items)
        , xmlNode "h2" [] [xmlContent "Authors"]
        , xmlNode
          "p"
          []
          [ xmlContent "Download the "
          , xmlNode "a" [("href", "authors.opml")] [xmlContent "authors OPML"]
          , xmlContent "."
          ]
        , xmlNode
          "ul"
          [("class", "authors")]
          (map authorToHtml (Set.toAscList authors))
        ]
      html = xmlElement
        "html"
        [("xmlns", "http://www.w3.org/1999/xhtml")]
        [htmlHead, htmlBody]
      document = Xml.Document prologue html []
      body = Xml.renderLBS Xml.def document
    in Wai.responseLBS Http.ok200 [contentType "application/xhtml+xml"] body

entryToHtml :: Entry.Entry -> Xml.Node
entryToHtml entry = xmlNode
  "li"
  [("class", "entry")]
  [ xmlNode
    "a"
    [ ("class", "entry-name")
    , ("href", Url.fromUrl (Item.itemUrl (Entry.entryItem entry)))
    ]
    [xmlContent (Name.fromName (Item.itemName (Entry.entryItem entry)))]
  , xmlContent " by "
  , xmlNode
    "span"
    [("class", "entry-author")]
    [xmlContent (Name.fromName (Author.authorName (Entry.entryAuthor entry)))]
  , xmlContent " on "
  , xmlNode
    "time"
    [ ("class", "entry-time")
    , ( "datetime"
      , Time.formatTime
        Time.defaultTimeLocale
        "%Y-%m-%dT%H:%M:%SZ"
        (Item.itemTime (Entry.entryItem entry))
      )
    ]
    [ xmlContent
        (Time.formatTime
          Time.defaultTimeLocale
          "%B %-e"
          (Item.itemTime (Entry.entryItem entry))
        )
    ]
  ]

authorToHtml :: Author.Author -> Xml.Node
authorToHtml author = xmlNode
  "li"
  [("class", "author")]
  [ xmlNode
      "a"
      [ ("class", "author-name")
      , ("href", Url.fromUrl (Author.authorUrl author))
      ]
      [xmlContent (Name.fromName (Author.authorName author))]
  ]

getAuthorsHandler :: Database.Database -> Wai.Response
getAuthorsHandler database =
  let
    authors = Database.getDatabaseAuthors database
    status = Http.ok200
    headers = [contentType "text/xml"]
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
getFaviconHandler = Wai.responseLBS Http.ok200 [] Favicon.favicon

getFeedAtomHandler :: Database.Database -> Time.UTCTime -> Wai.Response
getFeedAtomHandler database now
  = let
      title = xmlNode "title" [] [xmlContent "Reveille"]
      id_ = xmlNode "id" [] [xmlContent rootUrl]
      updated = xmlNode "updated" [] [xmlContent (rfc3339 now)]
      link =
        xmlNode "link" [("rel", "self"), ("href", rootUrl ++ "/feed.atom")] []
      items = getRecentDatabaseEntries database now
      entries = map entryToAtom items
      feed = xmlElement
        "feed"
        [("xmlns", "http://www.w3.org/2005/Atom")]
        (title : id_ : updated : link : entries)
      document = xmlDocument feed
    in Wai.responseLBS
      Http.ok200
      [contentType "application/atom+xml"]
      (Xml.renderLBS Xml.def document)

getFeedJsonHandler :: Database.Database -> Time.UTCTime -> Wai.Response
getFeedJsonHandler database now = let
  status = Http.ok200
  headers = [contentType "application/json"]
  items = map toJsonFeedItem (getRecentDatabaseEntries database now)
  feed = defaultJsonFeed
    { JsonFeed.feedFeedUrl = Just (unsafeToJsonFeedUrl (rootUrl ++ "feed.json"))
    , JsonFeed.feedHomePageUrl = Just (unsafeToJsonFeedUrl rootUrl)
    , JsonFeed.feedItems = items
    , JsonFeed.feedTitle = Text.pack "Reveille"
    }
  body = JsonFeed.renderFeed feed
  in Wai.responseLBS status headers body

unsafeToJsonFeedUrl :: String -> JsonFeed.Url
unsafeToJsonFeedUrl = JsonFeed.Url . Maybe.fromJust . Uri.parseAbsoluteURI

toJsonFeedAuthor :: Author.Author -> JsonFeed.Author
toJsonFeedAuthor author = defaultJsonFeedAuthor
  { JsonFeed.authorName = Just (Name.unwrapName (Author.authorName author))
  , JsonFeed.authorUrl = Just (unsafeToJsonFeedUrl (Url.fromUrl (Author.authorUrl author)))
  }

defaultJsonFeedAuthor :: JsonFeed.Author
defaultJsonFeedAuthor = JsonFeed.Author
  { JsonFeed.authorAvatar = Nothing
  , JsonFeed.authorName = Nothing
  , JsonFeed.authorUrl = Nothing
  }

toJsonFeedItem :: Entry.Entry -> JsonFeed.Item
toJsonFeedItem entry = let
  author = Entry.entryAuthor entry
  item = Entry.entryItem entry
  in defaultJsonFeedItem
    { JsonFeed.itemAuthor = Just (toJsonFeedAuthor author)
    , JsonFeed.itemDatePublished = Just (Item.itemTime item)
    , JsonFeed.itemId = Aeson.String (Text.pack (Url.fromUrl (Item.itemUrl item)))
    , JsonFeed.itemTitle = Just (Name.unwrapName (Item.itemName item))
    , JsonFeed.itemUrl = Just (unsafeToJsonFeedUrl (Url.fromUrl (Item.itemUrl item)))
    }

defaultJsonFeedItem :: JsonFeed.Item
defaultJsonFeedItem = JsonFeed.Item
  { JsonFeed.itemAttachments = Nothing
  , JsonFeed.itemAuthor = Nothing
  , JsonFeed.itemBannerImage = Nothing
  , JsonFeed.itemContentHtml = Nothing
  , JsonFeed.itemContentText = Nothing
  , JsonFeed.itemDateModified = Nothing
  , JsonFeed.itemDatePublished = Nothing
  , JsonFeed.itemExternalUrl = Nothing
  , JsonFeed.itemId = Aeson.Null
  , JsonFeed.itemImage = Nothing
  , JsonFeed.itemSummary = Nothing
  , JsonFeed.itemTags = Nothing
  , JsonFeed.itemTitle = Nothing
  , JsonFeed.itemUrl = Nothing
  }

defaultJsonFeed :: JsonFeed.Feed
defaultJsonFeed = JsonFeed.Feed
  { JsonFeed.feedAuthor = Nothing
  , JsonFeed.feedDescription = Nothing
  , JsonFeed.feedExpired = Nothing
  , JsonFeed.feedFavicon = Nothing
  , JsonFeed.feedFeedUrl = Nothing
  , JsonFeed.feedHomePageUrl = Nothing
  , JsonFeed.feedHubs = Nothing
  , JsonFeed.feedIcon = Nothing
  , JsonFeed.feedItems = []
  , JsonFeed.feedNextUrl = Nothing
  , JsonFeed.feedTitle = Text.empty
  , JsonFeed.feedUserComment = Nothing
  , JsonFeed.feedVersion = unsafeToJsonFeedUrl "https://jsonfeed.org/version/1"
  }

getFeedRssHandler :: Database.Database -> Time.UTCTime -> Wai.Response
getFeedRssHandler database now = let
  status = Http.ok200
  headers = [contentType "application/rss+xml"]
  items = map entryToRss (getRecentDatabaseEntries database now)
  title = xmlNode "title" [] [xmlContent "Reveille"]
  description = xmlNode "description" [] []
  link = xmlNode "link" [] [xmlContent rootUrl]
  channel = xmlNode "channel" [] (title : description : link : items)
  rss = xmlElement "rss" [("version", "2.0")] [channel]
  document = xmlDocument rss
  body = Xml.renderLBS Xml.def document
  in Wai.responseLBS status headers body

entryToRss :: Entry.Entry -> Xml.Node
entryToRss entry = let item = Entry.entryItem entry in xmlNode "item" []
  [ xmlNode "title" [] [xmlContent (Name.fromName (Item.itemName item))]
  , xmlNode "guid" [] [xmlContent (Url.fromUrl (Item.itemUrl item))]
  , xmlNode "pubDate" [] [xmlContent (rfc822 (Item.itemTime item))]
  ]

rfc822 :: Time.UTCTime -> String
rfc822 time = Time.formatTime Time.defaultTimeLocale "%a, %d %b %Y %H:%M:%S %Z" time

rootUrl :: String
rootUrl = "https://reveille.haskellweekly.news/"

getHealthCheckHandler :: Wai.Response
getHealthCheckHandler = Wai.responseLBS
  Http.ok200
  [contentType "application/xml"]
  (Xml.renderLBS Xml.def (xmlDocument (xmlElement "true" [] [])))

getRobotsHandler :: Wai.Response
getRobotsHandler = Wai.responseLBS
  Http.ok200
  [contentType "text/plain; charset=utf-8"]
  (LazyBytes.fromStrict
    (Unicode.toUtf8 (unlines ["User-Agent: *", "Disallow:"]))
  )

contentType :: String -> Http.Header
contentType mime = (Http.hContentType, Unicode.toUtf8 mime)

defaultHandler :: Wai.Response
defaultHandler = Wai.responseLBS
  Http.notFound404
  [contentType "application/xml"]
  (Xml.renderLBS Xml.def (xmlDocument (xmlElement "not-found" [] [])))

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

entryToAtom :: Entry.Entry -> Xml.Node
entryToAtom entry
  = let
      item = Entry.entryItem entry
      author = Entry.entryAuthor entry
      url = Url.fromUrl (Item.itemUrl item)
      title =
        xmlNode "title" [] [xmlContent (Name.fromName (Item.itemName item))]
      id_ = xmlNode "id" [] [xmlContent url]
      updated =
        xmlNode "updated" [] [xmlContent (rfc3339 (Item.itemTime item))]
      link = xmlNode "link" [("href", url)] []
      author_ = xmlNode
        "author"
        []
        [ xmlNode
          "name"
          []
          [xmlContent (Name.fromName (Author.authorName author))]
        , xmlNode "uri" [] [xmlContent (Url.fromUrl (Author.authorUrl author))]
        ]
    in xmlNode "entry" [] [title, id_, updated, link, author_]

getRecentDatabaseEntries :: Database.Database -> Time.UTCTime -> [Entry.Entry]
getRecentDatabaseEntries database now =
  sortEntriesByTime (filterItems now (Database.getDatabaseEntries database))

filterItems :: Time.UTCTime -> Set.Set Entry.Entry -> Set.Set Entry.Entry
filterItems now entries =
  Set.filter (\entry -> isNotTooNew now entry && isNotTooOld now entry) entries

isNotTooNew :: Time.UTCTime -> Entry.Entry -> Bool
isNotTooNew now entry = entryTime entry <= now

isNotTooOld :: Time.UTCTime -> Entry.Entry -> Bool
isNotTooOld now entry = entryTime entry > twoWeeksBefore now

sortEntriesByTime :: Set.Set Entry.Entry -> [Entry.Entry]
sortEntriesByTime items = List.sortBy compareEntriesByTime (Set.toList items)

compareEntriesByTime :: Entry.Entry -> Entry.Entry -> Ordering
compareEntriesByTime = Ord.comparing (\entry -> Ord.Down (entryTime entry))

entryTime :: Entry.Entry -> Time.UTCTime
entryTime entry = Item.itemTime (Entry.entryItem entry)

twoWeeksBefore :: Time.UTCTime -> Time.UTCTime
twoWeeksBefore time = Time.addUTCTime (negate twoWeeks) time

twoWeeks :: Time.NominalDiffTime
twoWeeks = 14 * Time.nominalDay
