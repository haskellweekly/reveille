module Reveille.Internal.Author where

import qualified Reveille.Internal.Name as Reveille
import qualified Reveille.Internal.Url as Reveille

data Author = Author
  { authorName :: Reveille.Name
  , authorUrl :: Reveille.Url
  , authorFeed :: Maybe Reveille.Url
  } deriving (Eq, Ord, Show)

toAuthor :: String -> String -> Maybe String -> Either AuthorError Author
toAuthor rawName rawUrl maybeRawFeed = do
  name <- case Reveille.toName rawName of
    Left nameError -> Left (AuthorErrorBadName nameError)
    Right name -> Right name

  url <- case Reveille.toUrl rawUrl of
    Left urlError -> Left (AuthorErrorBadUrl urlError)
    Right url -> Right url

  maybeFeed <- case maybeRawFeed of
    Nothing -> Right Nothing
    Just rawFeed -> case Reveille.toUrl rawFeed of
      Left urlError -> Left (AuthorErrorBadFeed urlError)
      Right feed -> Right (Just feed)

  pure Author {authorName = name, authorUrl = url, authorFeed = maybeFeed}

data AuthorError
  = AuthorErrorBadName Reveille.NameError
  | AuthorErrorBadUrl Reveille.UrlError
  | AuthorErrorBadFeed Reveille.UrlError
  deriving (Eq, Ord, Show)
