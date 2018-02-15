module Reveille.Name
  ( Name
  , toName
  , fromName
  ) where

import qualified Data.Text as Text

newtype Name = Name
  { unwrapName :: Text.Text
  } deriving (Eq, Ord, Show)

toName :: String -> Name
toName string = Name (Text.pack string)

fromName :: Name -> String
fromName name = Text.unpack (unwrapName name)
