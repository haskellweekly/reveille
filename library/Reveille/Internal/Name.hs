module Reveille.Internal.Name where

import qualified Control.Monad as Monad
import qualified Data.Text as Text

newtype Name = Name
  { unwrapName :: Text.Text
  } deriving (Eq, Ord, Show)

toName :: String -> Either NameError Name
toName string = do
  Monad.when (null string) (Left NameErrorEmpty)
  Right (Name (Text.pack string))

fromName :: Name -> String
fromName name = Text.unpack (unwrapName name)

data NameError
  = NameErrorEmpty
  deriving (Eq, Ord, Show)
