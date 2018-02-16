module Reveille.Database
  ( Database
  , initialDatabase
  , updateDatabase
  , getRecentDatabaseItems
  ) where

import Data.Function ((&))
import Reveille.Author (Author)
import Reveille.Item (Item, itemTime)

import qualified Data.List as List
import qualified Data.Map as Map
import qualified Data.Maybe as Maybe
import qualified Data.Ord as Ord
import qualified Data.Set as Set
import qualified Data.Time as Time

newtype Database = Database
  { unwrapDatabase :: Map.Map Author (Set.Set Item)
  } deriving (Eq, Ord, Show)

initialDatabase :: Database
initialDatabase = Database Map.empty

updateDatabase :: Author -> Set.Set Item -> Database -> Database
updateDatabase author items database = Database (Map.insertWith Set.union author items (unwrapDatabase database))

getRecentDatabaseItems :: Database -> Time.UTCTime -> [(Author, Item)]
getRecentDatabaseItems database now =
  let twoWeeksAgo = Time.addUTCTime (-14 * Time.nominalDay) now
  in database
    & unwrapDatabase
    & Map.toList
    & concatMap (\ (author, items) -> items
      & Set.toList
      & Maybe.mapMaybe (\ item ->
        if itemTime item > now then
          Nothing
        else if itemTime item < twoWeeksAgo then
          Nothing
        else
          Just (author, item)))
    & List.sortBy (Ord.comparing (\ (_, item) -> Ord.Down (itemTime item)))
