module Reveille.Author
  ( Author(..)
  , toAuthor
  , AuthorError(..)
  ) where

import Reveille.Name (Name, toName, NameError)
import Reveille.Url (Url, toUrl, UrlError)

import qualified Control.Arrow as Arrow

data Author = Author
  { authorName :: Name
  , authorUrl :: Url
  , authorFeed :: Maybe Url
  } deriving (Eq, Ord, Show)

toAuthor :: String -> String -> Maybe String -> Either AuthorError Author
toAuthor rawName rawUrl rawMaybeFeed = do
  name <- Arrow.left AuthorErrorBadName (toName rawName)
  url <- Arrow.left AuthorErrorBadUrl (toUrl rawUrl)
  maybeFeed <- case rawMaybeFeed of
    Nothing -> pure Nothing
    Just rawFeedUrl -> do
      feed <- Arrow.left AuthorErrorBadFeed (toUrl rawFeedUrl)
      pure (Just feed)
  pure Author
    { authorName = name
    , authorUrl = url
    , authorFeed = maybeFeed
    }

data AuthorError
  = AuthorErrorBadName NameError
  | AuthorErrorBadUrl UrlError
  | AuthorErrorBadFeed UrlError
  deriving (Eq, Ord, Show)
