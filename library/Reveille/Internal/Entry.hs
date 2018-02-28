module Reveille.Internal.Entry where

import qualified Data.Set as Set
import qualified Reveille.Internal.Author as Author
import qualified Reveille.Internal.Item as Item

data Entry = Entry
  { entryAuthor :: Author.Author
  , entryItem :: Item.Item
  } deriving (Eq, Ord, Show)

toEntries :: Functor f => Author.Author -> f Item.Item -> f Entry
toEntries author items = fmap (Entry author) items

toEntrySet :: Author.Author -> Set.Set Item.Item -> Set.Set Entry
toEntrySet author items = Set.map (Entry author) items
