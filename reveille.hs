{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE NumericUnderscores #-}

module Main ( main ) where

import qualified Control.Concurrent as Concurrent
import qualified Control.Concurrent.Async as Async
import qualified Control.Concurrent.STM as Stm
import qualified Control.Exception as Exception
import qualified Control.Monad as Monad
import qualified Data.Aeson as Aeson
import qualified Data.Char as Char
import qualified Data.Functor.Identity as Identity
import qualified Data.List as List
import qualified Data.Maybe as Maybe
import qualified Data.Ord as Ord
import qualified Data.Set as Set
import qualified Data.Text as Text
import qualified Data.Text.Encoding as Text
import qualified Data.Text.Lazy as LazyText
import qualified Data.Text.Lazy.Encoding as LazyText
import qualified Data.Time as Time
import qualified Data.Vector as Vector
import qualified Data.Version as Version
import qualified Lucid
import qualified Network.HTTP.Client as Client
import qualified Network.HTTP.Client.TLS as Tls
import qualified Network.HTTP.Types as Http
import qualified Network.URI as Uri
import qualified Network.Wai as Wai
import qualified Network.Wai.Handler.Warp as Warp
import qualified Paths_reveille as Package
import qualified Say
import qualified System.Console.GetOpt as Console
import qualified System.Environment as Environment
import qualified System.Exit as Exit
import qualified Text.Atom.Feed as Atom
import qualified Text.Atom.Feed.Export as Atom
import qualified Text.Feed.Import as Feed
import qualified Text.Feed.Query as Feed
import qualified Text.Feed.Types as Feed
import qualified Text.Read as Read

main :: IO ()
main = do
  say "getting config"
  config <- getConfig
  items <- Stm.newTVarIO Set.empty
  Async.race_ (runServer config items) (runWorker config items)

runServer :: Config f -> Stm.TVar (Set.Set Item) -> IO ()
runServer config items = do
  say "starting server"
  Warp.runSettings (toSettings config) (application items)

application :: Stm.TVar (Set.Set Item) -> Wai.Application
application itemsVar request respond = let
  method = fmap Text.unpack (Text.decodeUtf8' (Wai.requestMethod request))
  path = map Text.unpack (Wai.pathInfo request)
  in case path of
    [] -> case method of
      Right "GET" -> do
        items <- Stm.readTVarIO itemsVar
        respond (htmlResponse Http.ok200 [] (index items))
      _ -> respond (htmlResponse Http.methodNotAllowed405 [] (Lucid.toHtml "method not allowed"))
    ["feed.atom"] -> case method of
      Right "GET" -> do
        now <- Time.getCurrentTime
        items <- Stm.readTVarIO itemsVar
        respond (atomResponse Http.ok200 [] (toFeed now items))
      _ -> respond (htmlResponse Http.methodNotAllowed405 [] (Lucid.toHtml "method not allowed"))
    _ -> respond (htmlResponse Http.notFound404 [] (Lucid.toHtml "not found"))

index :: Set.Set Item -> Lucid.Html ()
index items = Lucid.doctypehtml_ do
  Lucid.head_ do
    Lucid.meta_ [Lucid.charset_ (Text.pack "utf-8")]
    Lucid.title_ (Lucid.toHtml "Haskell Weekly \x2192 Reveille")
  Lucid.body_ do
    Lucid.h1_ (Lucid.toHtml "Haskell Weekly \x2192 Reveille")
    Lucid.ul_ do
      Monad.forM_
        (take 100 (List.sortOn (Ord.Down . itemTime) (Set.toList items)))
        \ item -> do
          Lucid.li_ do
            Lucid.a_
              [Lucid.href_ (Text.pack (renderUrl (unwrapItemUrl (itemUrl item))))]
              (Lucid.toHtml (unwrapTitle (itemTitle item)))
            Lucid.toHtml " by "
            Lucid.toHtml (unwrapName (authorName (itemAuthor item)))
            Lucid.toHtml " on "
            Lucid.toHtml (Time.formatTime Time.defaultTimeLocale "%B %-d" (itemTime item))

toFeed :: Time.UTCTime -> Set.Set Item -> Atom.Feed
toFeed now items = let
  feed = Atom.nullFeed
    (Text.pack "https://reveille.haskellweekly.news/feed.atom")
    (Atom.TextString (Text.pack "Haskell Weekly \x2192 Reveille"))
    (Text.pack (Time.formatTime Time.defaultTimeLocale "%Y-%m-%dT%H:%M:%SZ" now))
  entries = map toEntry (take 100 (List.sortOn (Ord.Down . itemTime) (Set.toList items)))
  in feed { Atom.feedEntries = entries }

toEntry :: Item -> Atom.Entry
toEntry item = let
  entry = Atom.nullEntry
    (Text.pack (renderUrl (unwrapItemUrl (itemUrl item))))
    (Atom.TextString (unwrapTitle (itemTitle item)))
    (Text.pack (Time.formatTime Time.defaultTimeLocale "%Y-%m-%dT%H:%M:%SZ" (itemTime item)))
  author = itemAuthor item
  person = Atom.nullPerson
    { Atom.personName = unwrapName (authorName author)
    , Atom.personURI = fmap (Text.pack . renderUrl . unwrapSiteUrl) (authorSite author)
    }
  in entry { Atom.entryAuthors = [person] }

atomResponse :: Http.Status -> [Http.Header] -> Atom.Feed -> Wai.Response
atomResponse status headers feed = Wai.responseLBS
  status
  ((Http.hContentType, Text.encodeUtf8 (Text.pack "")) : headers)
  (LazyText.encodeUtf8 (Maybe.fromMaybe LazyText.empty (Atom.textFeed feed)))

htmlResponse :: Http.Status -> [Http.Header] -> Lucid.Html a -> Wai.Response
htmlResponse status headers html = Wai.responseLBS
  status
  ((Http.hContentType, Text.encodeUtf8 (Text.pack "text/html; charset=utf-8")) : headers)
  (Lucid.renderBS html)

toSettings :: Config f -> Warp.Settings
toSettings config = Warp.setPort (configPort config) Warp.defaultSettings

runWorker :: Config Identity.Identity -> Stm.TVar (Set.Set Item) -> IO ()
runWorker config items = do
  say "starting worker"
  let airtableApiKey = Identity.runIdentity (configAirtableApiKey config)
  manager <- Tls.newTlsManager
  Monad.forever do
    say "getting authors"
    authors <- getAuthors airtableApiKey manager Vector.empty Nothing
    say ("got " <> pluralize (length authors) "author" <> " total")
    say "syncing authors"
    Monad.forM_ authors \ author ->
      Exception.catch (syncAuthor manager items author) \ exception ->
        say (show (exception :: Exception.SomeException))
    say "done syncing authors"
    Concurrent.threadDelay 600_000_000

syncAuthor :: Client.Manager -> Stm.TVar (Set.Set Item) -> Author -> IO ()
syncAuthor manager items author = do
  say ("syncing " <> Text.unpack (unwrapName (authorName author)))
  case authorFeed author of
    Nothing -> say "no feed"
    Just feedUrl -> do
      initialRequest <- Client.parseUrlThrow (renderUrl (unwrapFeedUrl feedUrl))
      let
        request = initialRequest
          { Client.requestHeaders =
            [ ( Http.hUserAgent
              , Text.encodeUtf8 (Text.pack ("haskellweekly/reveille-" <> Version.showVersion Package.version))
              )
            ]
          }
      response <- Client.httpLbs request manager
      feed <- case Feed.parseFeedSource (Client.responseBody response) of
        Nothing -> fail "invalid feed"
        Just feed -> pure feed
      let feedItems = Feed.getFeedItems feed
      say ("found " <> pluralize (length feedItems) "item")
      newItems <- case traverse (toItem author) feedItems of
        Left message -> fail message
        Right newItems -> pure (Set.fromList newItems)
      Stm.atomically (Stm.modifyTVar' items (Set.union newItems))
      pure ()

toItem :: Author -> Feed.Item -> Either String Item
toItem author item = do
  title <- case Feed.getItemTitle item of
    Nothing -> Left "missing item title"
    Just text -> Right (Title text)
  url <- case Feed.getItemLink item of
    Nothing -> Left "missing item url"
    Just text -> case toUrl text of
      Left _ -> case authorSite author of
        Nothing -> Left "invalid item url"
        Just siteUrl -> case toUrl (Text.pack (renderUrl (unwrapSiteUrl siteUrl)) <> text) of
          Left _ -> Left "invalid item url"
          Right url -> Right (ItemUrl url)
      Right url -> Right (ItemUrl url)
  time <- case Feed.getItemPublishDate item of
    Nothing -> Left "missing item time"
    Just Nothing -> Left "invalid item time"
    Just (Just utcTime) -> Right utcTime
  Right Item
    { itemAuthor = author
    , itemTime = time
    , itemTitle = title
    , itemUrl = url
    }

getAuthors :: AirtableApiKey -> Client.Manager -> Vector.Vector Author -> Maybe Offset -> IO (Vector.Vector Author)
getAuthors apiKey manager oldAuthors maybeOffset = do
  say "getting one page of authors"
  initialRequest <- Client.parseUrlThrow "https://api.airtable.com/v0/appDuA0ZgWSqSaDY3/Authors"
  let
    query = Http.renderSimpleQuery False case maybeOffset of
      Nothing -> []
      Just offset ->
        [ ( Text.encodeUtf8 (Text.pack "offset")
          , Text.encodeUtf8 (unwrapOffset offset)
          )
        ]
    request = initialRequest
      { Client.requestHeaders =
        [ ( Http.hAuthorization
          , Text.encodeUtf8 (Text.pack "Bearer " <> unwrapAirtableApiKey apiKey)
          )
        ]
      , Client.queryString = query
      }
  response <- Client.httpLbs request manager
  result <- case Aeson.eitherDecode' (Client.responseBody response) of
    Left message -> fail message
    Right airtable -> pure airtable
  let authors = airtableRecords result
  say ("got " <> pluralize (length authors) "author" <> " from this page")
  let newAuthors = oldAuthors <> airtableRecords result
  case airtableOffset result of
    Nothing -> pure newAuthors
    Just offset -> getAuthors apiKey manager newAuthors (Just offset)

toUrl :: Text.Text -> Either String Url
toUrl text = case Uri.parseURI (Text.unpack text) of
  Nothing -> Left ("invalid Url: " <> show text)
  Just uri -> Right (Url uri)

renderUrl :: Url -> String
renderUrl url = Uri.uriToString id (unwrapUrl url) ""

defaultConfig :: Config Maybe
defaultConfig = Config
  { configAirtableApiKey = Nothing
  , configHelp = HelpHide
  , configPort = 8080
  , configVersion = VersionHide
  }

getConfig :: IO (Config Identity.Identity)
getConfig = do
  arguments <- Environment.getArgs
  let
    (updates, unexpectedArguments, unknownOptions, errorMessages) =
      Console.getOpt' Console.Permute options arguments
  Monad.forM_ unexpectedArguments \ argument ->
    putStrLn ("WARNING: unexpected argument `" <> argument <> "'")
  Monad.forM_ unknownOptions \ option ->
    putStrLn ("WARNING: unknown option `" <> option <> "'")
  Monad.forM_ errorMessages \ message ->
    putStr ("ERROR: " <> message)
  Monad.unless (null errorMessages) Exit.exitFailure
  config <- case Monad.foldM applyUpdate defaultConfig updates of
    Left message -> Exit.die ("ERROR: " <> message)
    Right config -> pure config
  Monad.when (configHelp config == HelpShow) do
    header <- Environment.getProgName
    putStr (Console.usageInfo header options)
    Exit.exitFailure
  Monad.when (configVersion config == VersionShow) do
    putStrLn (Version.showVersion Package.version)
    Exit.exitFailure
  airtableApiKey <- case configAirtableApiKey config of
    Nothing -> Exit.die "ERROR: missing Airtable API key"
    Just airtableApiKey -> pure airtableApiKey
  pure config { configAirtableApiKey = Identity.Identity airtableApiKey }

applyUpdate :: Config Maybe -> Update -> Either String (Config Maybe)
applyUpdate config update = update config

options :: [Console.OptDescr Update]
options =
  [ optionWithArgument "airtable-api-key" "API_KEY"
    "sets the Airtable API key"
    \ string config -> if all Char.isSpace string
      then Left ("invalid Airtable API key: " <> show string)
      else Right config { configAirtableApiKey = Just (AirtableApiKey (Text.pack string)) }
  , optionWithoutArgument "help"
    "shows the help"
    \ config -> Right config { configHelp = HelpShow }
  , optionWithArgument "port" "PORT"
    "sets the HTTP server port"
    \ string config -> case Read.readEither string of
      Left message -> Left ("invalid port (" <> message <> "): " <> show string)
      Right port -> Right config { configPort = port }
  , optionWithoutArgument "version"
    "shows the version"
    \ config -> Right config { configVersion = VersionShow }
  ]

optionWithArgument
  :: String -> String -> String -> (String -> Update) -> Console.OptDescr Update
optionWithArgument name meta help update =
  Console.Option [] [name] (Console.ReqArg update meta) help

optionWithoutArgument :: String -> String -> Update -> Console.OptDescr Update
optionWithoutArgument name help update =
  Console.Option [] [name] (Console.NoArg update) help

say :: String -> IO ()
say message = do
  now <- Time.getCurrentTime
  Say.sayString
    ( Time.formatTime Time.defaultTimeLocale "%Y-%m-%dT%H:%M:%S.%3q%z " now
    <> message
    )

pluralize :: (Eq a, Num a, Show a) => a -> String -> String
pluralize count word =
  show count <> " " <> word <> if count == 1 then "" else "s"

type Update = Config Maybe -> Either String (Config Maybe)

newtype AirtableApiKey = AirtableApiKey
  { unwrapAirtableApiKey :: Text.Text
  } deriving (Eq, Ord, Show)

data Help
  = HelpHide
  | HelpShow
  deriving (Eq, Ord, Show)

data Version
  = VersionHide
  | VersionShow
  deriving (Eq, Ord, Show)

data Item = Item
  { itemAuthor :: ! Author
  , itemTime :: ! Time.UTCTime
  , itemTitle :: ! Title
  , itemUrl :: ! ItemUrl
  } deriving (Eq, Ord, Show)

newtype Title = Title
  { unwrapTitle :: Text.Text
  } deriving (Eq, Ord, Show)

newtype ItemUrl = ItemUrl
  { unwrapItemUrl :: Url
  } deriving (Eq, Ord, Show)

newtype SiteUrl = SiteUrl
  { unwrapSiteUrl :: Url
  } deriving (Eq, Ord, Show)

instance Aeson.FromJSON SiteUrl where
  parseJSON = fmap SiteUrl . Aeson.parseJSON

newtype FeedUrl = FeedUrl
  { unwrapFeedUrl :: Url
  } deriving (Eq, Ord, Show)

instance Aeson.FromJSON FeedUrl where
  parseJSON = fmap FeedUrl . Aeson.parseJSON

data Config f = Config
  { configAirtableApiKey :: ! (f AirtableApiKey)
  , configHelp :: ! Help
  , configPort :: ! Warp.Port
  , configVersion :: ! Version
  }

data Airtable a = Airtable
  { airtableRecords :: ! (Vector.Vector a)
  , airtableOffset :: ! (Maybe Offset)
  } deriving (Eq, Ord, Show)

instance Aeson.FromJSON a => Aeson.FromJSON (Airtable a) where
  parseJSON = Aeson.withObject "Airtable" \ object -> do
    records <- object Aeson..: Text.pack "records"
    offset <- object Aeson..:? Text.pack "offset"
    pure Airtable
      { airtableRecords = records
      , airtableOffset = offset
      }

data Author = Author
  { authorName :: ! Name
  , authorSite :: ! (Maybe SiteUrl)
  , authorFeed :: ! (Maybe FeedUrl)
  } deriving (Eq, Ord, Show)

instance Aeson.FromJSON Author where
  parseJSON = Aeson.withObject "Author" \ object -> do
    fields <- object Aeson..: Text.pack "fields"
    name <- fields Aeson..: Text.pack "Name"
    site <- fields Aeson..:? Text.pack "Site"
    feed <- fields Aeson..:? Text.pack "Feed"
    pure Author
      { authorName = name
      , authorSite = site
      , authorFeed = feed
      }

newtype Offset = Offset
  { unwrapOffset :: Text.Text
  } deriving (Eq, Ord, Show)

instance Aeson.FromJSON Offset where
  parseJSON = fmap Offset . Aeson.parseJSON

newtype Name = Name
  { unwrapName :: Text.Text
  } deriving (Eq, Ord, Show)

instance Aeson.FromJSON Name where
  parseJSON = fmap Name . Aeson.parseJSON

newtype Url = Url
  { unwrapUrl :: Uri.URI
  } deriving (Eq, Ord, Show)

instance Aeson.FromJSON Url where
  parseJSON json = do
    text <- Aeson.parseJSON json
    either fail pure (toUrl text)
