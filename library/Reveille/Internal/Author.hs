module Reveille.Internal.Author where

import qualified Reveille.Internal.Name as Name
import qualified Reveille.Internal.Url as Url

data Author = Author
  { authorName :: Name.Name
  , authorUrl :: Url.Url
  , authorFeed :: Maybe Url.Url
  } deriving (Eq, Ord, Show)

toAuthor :: String -> String -> Maybe String -> Either AuthorError Author
toAuthor rawName rawUrl maybeRawFeed = do
  name <- case Name.toName rawName of
    Left nameError -> Left (AuthorErrorBadName nameError)
    Right name -> Right name

  url <- case Url.toUrl rawUrl of
    Left urlError -> Left (AuthorErrorBadUrl urlError)
    Right url -> Right url

  maybeFeed <- case maybeRawFeed of
    Nothing -> Right Nothing
    Just rawFeed -> case Url.toUrl rawFeed of
      Left urlError -> Left (AuthorErrorBadFeed urlError)
      Right feed -> Right (Just feed)

  pure Author {authorName = name, authorUrl = url, authorFeed = maybeFeed}

data AuthorError
  = AuthorErrorBadName Name.NameError
  | AuthorErrorBadUrl Url.UrlError
  | AuthorErrorBadFeed Url.UrlError
  deriving (Eq, Ord, Show)
