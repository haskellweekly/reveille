module Reveille.Url
  ( Url
  , toUrl
  , fromUrl
  ) where

import qualified Data.Text as Text

newtype Url = Url
  { unwrapUrl :: Text.Text
  } deriving (Eq, Ord, Show)

toUrl :: String -> Url
toUrl string = Url (Text.pack string)

fromUrl :: Url -> String
fromUrl url = Text.unpack (unwrapUrl url)
