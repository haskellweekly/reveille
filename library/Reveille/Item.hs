module Reveille.Item
  ( Item(..)
  , toItem
  ) where

import Reveille.Name (Name, toName)
import Reveille.Url (Url, toUrl)

import qualified Data.Text as Text
import qualified Data.Time as Time
import qualified Text.Feed.Query as Feed
import qualified Text.Feed.Types as Feed

data Item = Item
  { itemName :: Name
  , itemUrl :: Url
  , itemTime :: Time.UTCTime
  } deriving (Eq, Ord, Show)

toItem :: Feed.Item -> Either String Item
toItem feedItem = do
  rawName <- maybeToEither "missing item name" (Feed.getItemTitle feedItem)
  name <- either (Left . show) Right (toName (Text.unpack rawName))
  rawUrl <- maybeToEither "missing item url" (Feed.getItemLink feedItem)
  url <- either (Left . show) Right (toUrl (Text.unpack rawUrl))
  maybeTime <- maybeToEither "missing item time" (Feed.getItemPublishDate feedItem)
  time <- maybeToEither "invalid item time" maybeTime
  pure Item
    { itemName = name
    , itemUrl = url
    , itemTime = time
    }

maybeToEither :: l -> Maybe r -> Either l r
maybeToEither l m = case m of
  Nothing -> Left l
  Just r -> Right r
