module Reveille.Entry
  ( Entry(..)
  ) where

import qualified Reveille.Author as Reveille
import qualified Reveille.Item as Reveille

data Entry = Entry
  { entryAuthor :: Reveille.Author
  , entryItem :: Reveille.Item
  } deriving (Eq, Ord, Show)
