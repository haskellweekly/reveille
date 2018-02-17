module Reveille.Item
  ( Item(..)
  , toItem
  , ItemError(..)
  ) where

import Reveille.Name (Name, toName, NameError)
import Reveille.Url (Url, toUrl, UrlError)

import qualified Control.Arrow as Arrow
import qualified Data.Text as Text
import qualified Data.Time as Time
import qualified Text.Feed.Query as Feed
import qualified Text.Feed.Types as Feed

data Item = Item
  { itemName :: Name
  , itemUrl :: Url
  , itemTime :: Time.UTCTime
  } deriving (Eq, Ord, Show)

toItem :: Feed.Item -> Either ItemError Item
toItem feedItem = do
  rawName <- maybeToEither ItemErrorNoName (Feed.getItemTitle feedItem)
  name <- Arrow.left ItemErrorBadName (toName (Text.unpack rawName))
  rawUrl <- maybeToEither ItemErrorNoUrl (Feed.getItemLink feedItem)
  url <- Arrow.left ItemErrorBadUrl (toUrl (Text.unpack rawUrl))
  maybeTime <- maybeToEither ItemErrorNoTime (Feed.getItemPublishDate feedItem)
  time <- maybeToEither ItemErrorInvalidTime maybeTime
  pure Item
    { itemName = name
    , itemUrl = url
    , itemTime = time
    }

maybeToEither :: l -> Maybe r -> Either l r
maybeToEither l m = case m of
  Nothing -> Left l
  Just r -> Right r

data ItemError
  = ItemErrorNoName
  | ItemErrorBadName NameError
  | ItemErrorNoUrl
  | ItemErrorBadUrl UrlError
  | ItemErrorNoTime
  | ItemErrorInvalidTime
  deriving (Eq, Ord, Show)
