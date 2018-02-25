module Reveille.Internal.Database where

import qualified Data.Map as Map
import qualified Data.Set as Set
import qualified Reveille.Internal.Author as Author
import qualified Reveille.Internal.Entry as Entry
import qualified Reveille.Internal.Item as Item

newtype Database = Database
  { unwrapDatabase :: Map.Map Author.Author (Set.Set Item.Item)
  } deriving (Eq, Ord, Show)

initialDatabase :: Database
initialDatabase = Database Map.empty

addDatabaseAuthor :: Author.Author -> Database -> Database
addDatabaseAuthor author database = addDatabaseItems author Set.empty database

addDatabaseItems :: Author.Author -> Set.Set Item.Item -> Database -> Database
addDatabaseItems author items database =
  Database (Map.insertWith Set.union author items (unwrapDatabase database))

addDatabaseEntries :: Set.Set Entry.Entry -> Database -> Database
addDatabaseEntries entries database = foldr
  (\entry -> addDatabaseItems
    (Entry.entryAuthor entry)
    (Set.singleton (Entry.entryItem entry))
  )
  database
  entries

getDatabaseAuthors :: Database -> Set.Set Author.Author
getDatabaseAuthors database = Map.keysSet (unwrapDatabase database)

getDatabaseEntries :: Database -> Set.Set Entry.Entry
getDatabaseEntries database = Set.unions
  (map
    (\(author, items) -> Set.map (Entry.Entry author) items)
    (Map.toList (unwrapDatabase database))
  )
