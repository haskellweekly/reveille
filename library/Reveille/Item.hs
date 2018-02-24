module Reveille.Item
  ( Item(..)
  , toItem
  , ItemError(..)
  ) where

import qualified Data.Text as Text
import qualified Data.Time as Time
import qualified Reveille.Name as Reveille
import qualified Reveille.Url as Reveille
import qualified Text.Feed.Query as Feed
import qualified Text.Feed.Types as Feed

data Item = Item
  { itemName :: Reveille.Name
  , itemUrl :: Reveille.Url
  , itemTime :: Time.UTCTime
  } deriving (Eq, Ord, Show)

toItem :: Feed.Item -> Either ItemError Item
toItem feedItem = do
  name <- case Feed.getItemTitle feedItem of
    Nothing -> Left ItemErrorNoName
    Just title -> case Reveille.toName (Text.unpack title) of
      Left nameError -> Left (ItemErrorBadName nameError)
      Right name -> Right name

  url <- case Feed.getItemLink feedItem of
    Nothing -> Left ItemErrorNoUrl
    Just link -> case Reveille.toUrl (Text.unpack link) of
      Left urlError -> Left (ItemErrorBadUrl urlError)
      Right url -> Right url

  time <- case Feed.getItemPublishDate feedItem of
    Nothing -> Left ItemErrorNoTime
    Just Nothing -> Left ItemErrorInvalidTime
    Just (Just time) -> Right time

  pure Item {itemName = name, itemUrl = url, itemTime = time}

data ItemError
  = ItemErrorNoName
  | ItemErrorBadName Reveille.NameError
  | ItemErrorNoUrl
  | ItemErrorBadUrl Reveille.UrlError
  | ItemErrorNoTime
  | ItemErrorInvalidTime
  deriving (Eq, Ord, Show)
