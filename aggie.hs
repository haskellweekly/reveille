module Main
  ( main
  ) where

import qualified Data.Foldable as Foldable
import qualified Data.Set as Set
import qualified Data.Text as Text
import qualified Text.Printf as Printf

main :: IO ()
main = do
  Foldable.for_ sources (\ source -> do
    Printf.printf "- %s <%s>\n"
      (fromName (sourceName source))
      (fromUrl (sourceUrl source)))

sources :: Set.Set Source
sources = Set.fromList
  [ Source
    { sourceName = toName "Taylor Fausak"
    , sourceUrl = toUrl "http://taylor.fausak.me"
    , sourceFeed = Just (toUrl "http://taylor.fausak.me/sitemap.atom")
    }
  ]

data Source = Source
  { sourceName :: Name
  , sourceUrl :: Url
  , sourceFeed :: Maybe Url
  } deriving (Eq, Ord, Show)

newtype Name = Name
  { unwrapName :: Text.Text
  } deriving (Eq, Ord, Show)

toName :: String -> Name
toName string = Name (Text.pack string)

fromName :: Name -> String
fromName name = Text.unpack (unwrapName name)

newtype Url = Url
  { unwrapUrl :: Text.Text
  } deriving (Eq, Ord, Show)

toUrl :: String -> Url
toUrl string = Url (Text.pack string)

fromUrl :: Url -> String
fromUrl url = Text.unpack (unwrapUrl url)
