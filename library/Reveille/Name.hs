module Reveille.Name
  ( Name
  , toName
  , fromName
  ) where

import qualified Control.Monad as Monad
import qualified Data.Text as Text

newtype Name = Name
  { unwrapName :: Text.Text
  } deriving (Eq, Ord, Show)

toName :: String -> Either String Name
toName string = do
  Monad.when (null string) (fail "empty name")
  pure (Name (Text.pack string))

fromName :: Name -> String
fromName name = Text.unpack (unwrapName name)
