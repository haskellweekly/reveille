module Reveille.Internal.Url where

import qualified Control.Monad as Monad
import qualified Network.URI as Uri

newtype Url = Url
  { unwrapUrl :: Uri.URI
  } deriving (Eq, Ord, Show)

toUrl :: String -> Either UrlError Url
toUrl string = do
  Monad.when (null string) (Left UrlErrorEmpty)
  case Uri.parseAbsoluteURI string of
    Nothing -> Left (UrlErrorInvalid string)
    Just uri -> Right (Url uri)

fromUrl :: Url -> String
fromUrl url = Uri.uriToString id (unwrapUrl url) ""

data UrlError
  = UrlErrorEmpty
  | UrlErrorInvalid String
  deriving (Eq, Ord, Show)
