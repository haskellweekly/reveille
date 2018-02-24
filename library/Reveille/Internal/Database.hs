module Reveille.Internal.Database where

import qualified Data.Map as Map
import qualified Data.Set as Set
import qualified Reveille.Internal.Author as Reveille
import qualified Reveille.Internal.Entry as Reveille
import qualified Reveille.Internal.Item as Reveille

newtype Database = Database
  { unwrapDatabase :: Map.Map Reveille.Author (Set.Set Reveille.Item)
  } deriving (Eq, Ord, Show)

initialDatabase :: Database
initialDatabase = Database Map.empty

addDatabaseAuthor :: Reveille.Author -> Database -> Database
addDatabaseAuthor author database = addDatabaseItems author Set.empty database

addDatabaseItems
  :: Reveille.Author -> Set.Set Reveille.Item -> Database -> Database
addDatabaseItems author items database =
  Database (Map.insertWith Set.union author items (unwrapDatabase database))

addDatabaseEntries :: Set.Set Reveille.Entry -> Database -> Database
addDatabaseEntries entries database = foldr
  (\entry -> addDatabaseItems
    (Reveille.entryAuthor entry)
    (Set.singleton (Reveille.entryItem entry))
  )
  database
  entries

getDatabaseAuthors :: Database -> Set.Set Reveille.Author
getDatabaseAuthors database = Map.keysSet (unwrapDatabase database)

getDatabaseEntries :: Database -> Set.Set Reveille.Entry
getDatabaseEntries database = Set.unions
  (map
    (\(author, items) -> Set.map (Reveille.Entry author) items)
    (Map.toList (unwrapDatabase database))
  )
