module Main
  ( main
  ) where

import qualified Control.Concurrent.STM as Stm
import qualified Data.Either as Either
import qualified Data.Foldable as Foldable
import qualified Data.Map as Map
import qualified Data.Set as Set
import qualified Data.Text as Text
import qualified Data.Text.Encoding as Text
import qualified Data.Time as Time
import qualified Network.HTTP.Client as Client
import qualified Network.HTTP.Client.TLS as Client
import qualified Network.HTTP.Types as Http
import qualified Network.Wai as Wai
import qualified Network.Wai.Handler.Warp as Warp
import qualified Text.Feed.Constructor as Feed
import qualified Text.Feed.Export as Feed
import qualified Text.Feed.Import as Feed
import qualified Text.Feed.Query as Feed
import qualified Text.Feed.Types as Feed
import qualified Text.Printf as Printf
import qualified Text.XML as Xml

main :: IO ()
main = do
  manager <- Client.newTlsManager
  database <- Stm.newTVarIO initialDatabase

  Foldable.for_ sources (\ source -> do
    Printf.printf "- %s <%s>\n"
      (fromName (sourceName source))
      (fromUrl (sourceUrl source))

    items <- getSourceItems manager source
    Stm.atomically (Stm.modifyTVar database (updateDatabase source items))

    Foldable.for_ items (\ item -> do
      Printf.printf "  - %s: %s\n"
        (maybe "0000-00-00" (Time.formatTime Time.defaultTimeLocale "%Y-%m-%d") (itemTime item))
        (fromName (itemName item))))

  Warp.run 3000 (\ request respond -> do
    let method = Text.unpack (Text.decodeUtf8 (Wai.requestMethod request))
    let path = map Text.unpack (Wai.pathInfo request)
    case (method, path) of
      ("GET", ["feed.atom"]) -> do
        let feed = Feed.newFeed Feed.AtomKind
        let element = Feed.xmlFeed feed
        let conduitElement = Either.fromRight undefined (Xml.fromXMLElement element)
        let document = Xml.Document (Xml.Prologue [] Nothing []) conduitElement []
        respond (Wai.responseLBS
          Http.ok200
          [(Http.hContentType, Text.encodeUtf8 (Text.pack "application/atom+xml"))]
          (Xml.renderLBS Xml.def document))
      _ -> respond (Wai.responseLBS Http.notFound404 [] mempty))

sources :: Set.Set Source
sources = Set.fromList
  [ toSource
    "Taylor Fausak"
    "http://taylor.fausak.me"
    (Just "http://taylor.fausak.me/sitemap.atom")
  , toSource
    "FP Complete"
    "https://www.fpcomplete.com"
    (Just "https://www.fpcomplete.com/blog/atom.xml")
  ]

getSourceItems :: Client.Manager -> Source -> IO (Set.Set Item)
getSourceItems manager source = do
  url <- case sourceFeed source of
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

data Source = Source
  { sourceName :: Name
  , sourceUrl :: Url
  , sourceFeed :: Maybe Url
  } deriving (Eq, Ord, Show)

toSource :: String -> String -> Maybe String -> Source
toSource name url feed = Source
  { sourceName = toName name
  , sourceUrl = toUrl url
  , sourceFeed = fmap toUrl feed
  }

data Item = Item
  { itemName :: Name
  , itemUrl :: Url
  , itemTime :: Maybe Time.UTCTime
  } deriving (Eq, Ord, Show)

toItem :: Feed.Item -> Maybe Item
toItem feedItem = do
  name <- Feed.getItemTitle feedItem
  url <- Feed.getItemLink feedItem
  time <- Feed.getItemPublishDate feedItem
  pure Item
    { itemName = Name name
    , itemUrl = Url url
    , itemTime = time
    }

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
  { unwrapDatabase :: Map.Map Source (Set.Set Item)
  } deriving (Eq, Ord, Show)

initialDatabase :: Database
initialDatabase = Database Map.empty

updateDatabase :: Source -> Set.Set Item -> Database -> Database
updateDatabase source items database = Database (Map.insertWith Set.union source items (unwrapDatabase database))
