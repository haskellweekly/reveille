module Reveille.Url
  ( Url
  , toUrl
  , fromUrl
  ) where

import qualified Control.Monad as Monad
import qualified Network.URI as Uri

newtype Url = Url
  { unwrapUrl :: Uri.URI
  } deriving (Eq, Ord, Show)

toUrl :: String -> Either String Url
toUrl string = do
  Monad.when (null string) (fail "empty URL")
  case Uri.parseAbsoluteURI string of
    Nothing -> fail "invalid URL"
    Just uri -> pure (Url uri)

fromUrl :: Url -> String
fromUrl url = Uri.uriToString id (unwrapUrl url) ""
