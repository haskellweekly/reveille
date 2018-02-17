module Reveille.Author
  ( Author(..)
  , toAuthor
  ) where

import Reveille.Name (Name, toName)
import Reveille.Url (Url, toUrl)

data Author = Author
  { authorName :: Name
  , authorUrl :: Url
  , authorFeed :: Maybe Url
  } deriving (Eq, Ord, Show)

toAuthor :: String -> String -> Maybe String -> Either String Author
toAuthor rawName rawUrl rawMaybeFeed = do
  name <- toName rawName
  url <- either (Left . show) Right (toUrl rawUrl)
  maybeFeed <- case rawMaybeFeed of
    Nothing -> pure Nothing
    Just rawFeedUrl -> do
      feed <- either (Left . show) Right (toUrl rawFeedUrl)
      pure (Just feed)
  pure Author
    { authorName = name
    , authorUrl = url
    , authorFeed = maybeFeed
    }
