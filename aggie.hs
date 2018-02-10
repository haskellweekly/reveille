module Main
  ( main
  ) where

import qualified Data.Foldable as Foldable
import qualified Data.Set as Set
import qualified Data.Text as Text
import qualified Network.HTTP.Client as Client
import qualified Network.HTTP.Client.TLS as Client
import qualified Text.Printf as Printf
import qualified Text.Feed.Import as Feed

main :: IO ()
main = do
  manager <- Client.newTlsManager

  Foldable.for_ sources (\ source -> do
    Printf.printf "- %s <%s>\n"
      (fromName (sourceName source))
      (fromUrl (sourceUrl source))

    Foldable.for_ (sourceFeed source) (\ url -> do
      request <- Client.parseUrlThrow (fromUrl url)
      response <- Client.httpLbs request manager

      Foldable.for_ (Feed.parseFeedSource (Client.responseBody response)) (\ feed -> do
        print feed)))

sources :: Set.Set Source
sources = Set.fromList
  [ Source
    { sourceName = toName "Taylor Fausak"
    , sourceUrl = toUrl "http://taylor.fausak.me"
    , sourceFeed = Just (toUrl "http://taylor.fausak.me/sitemap.atom")
    }
  , Source
    { sourceName = toName "FP Complete"
    , sourceUrl = toUrl "https://www.fpcomplete.com"
    , sourceFeed = Just (toUrl "https://www.fpcomplete.com/blog/atom.xml")
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
