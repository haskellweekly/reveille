module Reveille.Internal.Entry where

import qualified Reveille.Internal.Author as Author
import qualified Reveille.Internal.Item as Item

data Entry = Entry
  { entryAuthor :: Author.Author
  , entryItem :: Item.Item
  } deriving (Eq, Ord, Show)
