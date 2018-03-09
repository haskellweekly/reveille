module Reveille.Internal.Item where

import qualified Data.Monoid as Monoid
import qualified Data.Set as Set
import qualified Data.Text as Text
import qualified Data.Time as Time
import qualified Network.URI as Uri
import qualified Reveille.Internal.Name as Name
import qualified Reveille.Internal.Url as Url
import qualified Text.Feed.Query as Feed
import qualified Text.Feed.Types as Feed

data Item = Item
  { itemName :: Name.Name
  , itemUrl :: Url.Url
  , itemTime :: Time.UTCTime
  } deriving (Eq, Ord, Show)

toItem :: Url.Url -> Feed.Item -> Either ItemError Item
toItem feedUrl feedItem = do
  name <- case Feed.getItemTitle feedItem of
    Nothing -> Left ItemErrorNoName
    Just title -> case Name.toName (Text.unpack title) of
      Left nameError -> Left (ItemErrorBadName nameError)
      Right name -> Right name

  url <- case Feed.getItemLink feedItem of
    Nothing -> Left ItemErrorNoUrl
    Just linkText ->
      let link = Text.unpack linkText
      in
        case Url.toUrl link of
          Left urlError -> case Uri.parseURIReference link of
            Nothing -> Left (ItemErrorBadUrl urlError)
            Just path ->
              Right (Url.Url (Uri.relativeTo path (Url.unwrapUrl feedUrl)))
          Right url -> Right url

  time <- case Feed.getItemPublishDateString feedItem of
    Nothing -> Left ItemErrorNoTime
    Just text -> case parseTime (Text.unpack text) of
      Nothing -> Left (ItemErrorInvalidTime text)
      Just time -> Right time

  pure Item {itemName = name, itemUrl = url, itemTime = time}

parseTime :: Time.ParseTime t => String -> Maybe t
parseTime string = Monoid.getFirst
  (foldMap (\format -> parseTimeWith format string) timeFormats)

parseTimeWith :: (Monad m, Time.ParseTime t) => String -> String -> m t
parseTimeWith format string =
  Time.parseTimeM False Time.defaultTimeLocale format string

timeFormats :: Set.Set String
timeFormats = Set.fromList
  [ "%Y-%m-%dT%H:%M:%S%Q%Z"
  , "%Y-%-m-%dT%H:%M:%S%Q%Z"
  , "%a, %d %b %Y %H:%M:%S %Z"
  , "%a, %e %b %Y %H:%M:%S %Z"
  , "%Y-%m-%d"
  ]

data ItemError
  = ItemErrorNoName
  | ItemErrorBadName Name.NameError
  | ItemErrorNoUrl
  | ItemErrorBadUrl Url.UrlError
  | ItemErrorNoTime
  | ItemErrorInvalidTime Text.Text
  deriving (Eq, Ord, Show)
