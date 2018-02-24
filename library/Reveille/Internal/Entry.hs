module Reveille.Internal.Entry where

import qualified Reveille.Internal.Author as Reveille
import qualified Reveille.Internal.Item as Reveille

data Entry = Entry
  { entryAuthor :: Reveille.Author
  , entryItem :: Reveille.Item
  } deriving (Eq, Ord, Show)
