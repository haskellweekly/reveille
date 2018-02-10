module Main
  ( main
  ) where

import qualified Data.Foldable as Foldable
import qualified Data.Set as Set
import qualified Data.Text as Text
import qualified Data.Time as Time
import qualified Network.HTTP.Client as Client
import qualified Network.HTTP.Client.TLS as Client
import qualified Text.Feed.Import as Feed
import qualified Text.Feed.Query as Feed
import qualified Text.Feed.Types as Feed
import qualified Text.Printf as Printf

main :: IO ()
main = do
  manager <- Client.newTlsManager

  Foldable.for_ sources (\ source -> do
    Printf.printf "- %s <%s>\n"
      (fromName (sourceName source))
      (fromUrl (sourceUrl source))

    Foldable.for_ (sourceFeed source) (\ url -> do
      request <- Client.parseUrlThrow (fromUrl url)
      response <- Client.httpLbs request manager

      Foldable.for_ (Feed.parseFeedSource (Client.responseBody response)) (\ feed -> do
        Foldable.for_ (Feed.feedItems feed) (\ feedItem -> do
          Foldable.for_ (toItem feedItem) (\ item -> do
            Printf.printf "  - %s: %s\n"
              (maybe "0000-00-00" (Time.formatTime Time.defaultTimeLocale "%Y-%m-%d") (itemTime item))
              (fromName (itemName item)))))))

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
