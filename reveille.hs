{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Main
  ( main
  ) where

import qualified Conduit
import qualified Control.Concurrent as Concurrent
import qualified Control.Concurrent.Async as Async
import qualified Control.Concurrent.STM as Stm
import qualified Control.Exception as Exception
import qualified Control.Monad as Monad
import qualified Control.Monad.IO.Class as IO
import qualified Data.ByteString.Lazy as LazyBytes
import qualified Data.List as List
import qualified Data.Map as Map
import qualified Data.Maybe as Maybe
import qualified Data.NonNull as NonNull
import qualified Data.Ord as Ord
import qualified Data.Set as Set
import qualified Data.Text as Text
import qualified Data.Text.Encoding as Encoding
import qualified Data.Time as Time
import qualified Data.XML.Types as Xml
import qualified Lucid
import qualified Network.HTTP.Simple as Http
import qualified Network.URI as Uri
import qualified Network.Wai.Middleware.Gzip as Middleware
import qualified Network.Wai.Middleware.RequestLogger as Middleware
import qualified System.Environment as Environment
import qualified Text.Atom.Conduit.Parse as Atom
import qualified Text.Atom.Conduit.Render as Atom
import qualified Text.Atom.Types as Atom
import qualified Text.RSS.Conduit.Parse as Rss
import qualified Text.RSS.Types as Rss
import qualified Text.Read as Read
import qualified Text.XML.Stream.Parse as Xml
import qualified Text.XML.Stream.Render as Xml
import qualified URI.ByteString as Urib
import qualified Web.Scotty as Scotty

main :: IO ()
main = do
  port <- getPort
  database <- Stm.newTVarIO initialDatabase
  Async.race_ (runAggregator database) (runServer port database)

getPort :: IO Int
getPort = do
  maybePort <- Environment.lookupEnv "PORT"
  pure (Maybe.fromMaybe 8080 (Read.readMaybe (Maybe.fromMaybe "" maybePort)))

type Database = Map.Map Author (Set.Set Item)

runAggregator :: Stm.TVar Database -> IO ()
runAggregator database =
  Monad.forever $ do
    putStrLn "Aggregating ..."
    authors <- fmap Map.keysSet (Stm.readTVarIO database)
    mapM_ (updateAuthor database) authors
    putStrLn "Done."
    Concurrent.threadDelay (60 * 60 * 1000000)

updateAuthor :: Stm.TVar Database -> Author -> IO ()
updateAuthor database author = do
  items <- fetchItems (authorFeed author)
  Stm.atomically
    (Stm.modifyTVar database (Map.insertWith Set.union author items))
  Concurrent.threadDelay 1000000

fetchItems :: Feed -> IO (Set.Set Item)
fetchItems feed = do
  request <- Http.parseRequest (Text.unpack (renderUri (feedLink feed)))
  items <-
    case feedType feed of
      FeedTypeAtom ->
        fetchFeed request Atom.atomFeed Atom.feedEntries fromAtomEntry
      FeedTypeRss ->
        fetchFeed request Rss.rssDocument Rss.channelItems fromRssItem
  pure (Set.fromList (Maybe.catMaybes items))

fetchFeed ::
     Http.Request
  -> Conduit.ConduitM Xml.Event Conduit.Void (Conduit.ResourceT IO) (Maybe feed)
  -> (feed -> [item])
  -> (item -> Maybe Item)
  -> IO [Maybe Item]
fetchFeed request parse extract convert = do
  result <-
    Conduit.runConduitRes
      (Conduit.catchC
         (Conduit.fuse
            (Http.httpSource request Http.getResponseBody)
            (Conduit.fuse (Xml.parseBytes Xml.def) parse))
         (\(_ :: Exception.SomeException) -> pure Nothing))
  case result of
    Nothing -> pure []
    Just feed -> pure (map convert (extract feed))

fromRssItem :: Rss.RssItem' -> Maybe Item
fromRssItem rssItem = do
  time <- Rss.itemPubDate rssItem
  rssLink <- Rss.itemLink rssItem
  link <- fromRssUri rssLink
  pure Item {itemTime = time, itemName = Rss.itemTitle rssItem, itemLink = link}

fromRssUri :: Rss.RssURI -> Maybe Uri.URI
fromRssUri (Rss.RssURI urib) =
  parseUri (Encoding.decodeUtf8 (Urib.serializeURIRef' urib))

fromAtomEntry :: Atom.AtomEntry -> Maybe Item
fromAtomEntry atomEntry = do
  link <- parseUri (Atom.entryId atomEntry)
  pure
    Item
      { itemTime = Atom.entryUpdated atomEntry
      , itemName = fromAtomText (Atom.entryTitle atomEntry)
      , itemLink = link
      }

fromAtomText :: Atom.AtomText -> Text.Text
fromAtomText atomText =
  case atomText of
    Atom.AtomPlainText textType text ->
      case textType of
        Atom.TypeHTML -> text -- TODO
        Atom.TypeText -> text
    Atom.AtomXHTMLText text -> text -- TODO

runServer :: Int -> Stm.TVar Database -> IO ()
runServer port database = Scotty.scotty port (applicationWith database)

applicationWith :: Stm.TVar Database -> Scotty.ScottyM ()
applicationWith database = do
  Scotty.middleware Middleware.logStdout
  Scotty.middleware (Middleware.gzip Middleware.def)
  Scotty.get "/" (getRootAction database)
  Scotty.get "/feed.atom" (getFeedAction database)

getRootAction :: Stm.TVar Database -> Scotty.ActionM ()
getRootAction var = do
  now <- Scotty.liftAndCatchIO Time.getCurrentTime
  database <- Scotty.liftAndCatchIO (Stm.readTVarIO var)
  Scotty.html (Lucid.renderText (entriesToHtml (getRecentEntries now database)))

entriesToHtml :: [Entry] -> Lucid.Html ()
entriesToHtml entries =
  Lucid.doctypehtml_ $ do
    Lucid.head_ $ do
      Lucid.meta_ [Lucid.charset_ "utf-8"]
      Lucid.title_ "Reveille"
      Lucid.link_ [Lucid.rel_ "alternate", Lucid.href_ "/feed.atom"]
    Lucid.body_ $ do
      Lucid.h1_ "Reveille"
      Lucid.ul_ (foldMap entryToHtml entries)
      Lucid.p_
        (Lucid.a_
           [Lucid.href_ "https://github.com/haskellweekly/reveille"]
           "github.com/haskellweekly/reveille")

entryToHtml :: Entry -> Lucid.Html ()
entryToHtml entry =
  Lucid.li_
    (mconcat
       [ Lucid.a_
           [Lucid.href_ (renderUri (itemLink (entryItem entry)))]
           (Lucid.toHtml (itemName (entryItem entry)))
       , Lucid.span_ " by "
       , Lucid.a_
           [Lucid.href_ (renderUri (authorLink (entryAuthor entry)))]
           (Lucid.toHtml (authorName (entryAuthor entry)))
       , Lucid.span_ " on "
       , Lucid.time_
           [ Lucid.datetime_
               (renderTime "%Y-%m-%dT%H:%M:%S%Q%z" (itemTime (entryItem entry)))
           ]
           (Lucid.toHtml
              (renderTime "%A, %B %e, %Y" (itemTime (entryItem entry))))
       ])

getFeedAction :: Stm.TVar Database -> Scotty.ActionM ()
getFeedAction var = do
  now <- Scotty.liftAndCatchIO Time.getCurrentTime
  database <- Scotty.liftAndCatchIO (Stm.readTVarIO var)
  xml <- renderAtom (toAtomFeed now (getRecentEntries now database))
  Scotty.setHeader "Content-Type" "application/atom+xml"
  Scotty.raw xml

renderAtom :: IO.MonadIO io => Atom.AtomFeed -> io LazyBytes.ByteString
renderAtom atom =
  IO.liftIO
    (Conduit.connect
       (Conduit.fuse
          (Atom.renderAtomFeed atom)
          (Conduit.fuse (Xml.renderBuilder Xml.def) Conduit.builderToByteString))
       Conduit.sinkLazy)

toAtomFeed :: Time.UTCTime -> [Entry] -> Atom.AtomFeed
toAtomFeed time entries =
  Atom.AtomFeed
    { Atom.feedAuthors = []
    , Atom.feedCategories = []
    , Atom.feedContributors = []
    , Atom.feedEntries = map toAtomEntry entries
    , Atom.feedGenerator = Nothing
    , Atom.feedIcon = Nothing
    , Atom.feedId = "https://reveille.haskellweekly.news/feed.atom"
    , Atom.feedLinks = []
    , Atom.feedLogo = Nothing
    , Atom.feedRights = Nothing
    , Atom.feedSubtitle = Nothing
    , Atom.feedTitle = Atom.AtomPlainText Atom.TypeText "Reveille"
    , Atom.feedUpdated =
        Maybe.fromMaybe
          time
          (Maybe.listToMaybe
             (List.sortOn Ord.Down (map (itemTime . entryItem) entries)))
    }

toAtomEntry :: Entry -> Atom.AtomEntry
toAtomEntry entry =
  Atom.AtomEntry
    { Atom.entryAuthors = Maybe.catMaybes [toAtomPerson (entryAuthor entry)]
    , Atom.entryCategories = []
    , Atom.entryContent = Nothing
    , Atom.entryContributors = []
    , Atom.entryId = renderUri (itemLink (entryItem entry))
    , Atom.entryLinks = []
    , Atom.entryPublished = Nothing
    , Atom.entryRights = Nothing
    , Atom.entrySource = Nothing
    , Atom.entrySummary = Nothing
    , Atom.entryTitle =
        Atom.AtomPlainText Atom.TypeText (itemName (entryItem entry))
    , Atom.entryUpdated = itemTime (entryItem entry)
    }

toAtomPerson :: Author -> Maybe Atom.AtomPerson
toAtomPerson author = do
  name <- NonNull.fromNullable (authorName author)
  uri <- toUrib (authorLink author)
  Just
    Atom.AtomPerson
      { Atom.personName = name
      , Atom.personEmail = ""
      , Atom.personUri = Just (Atom.AtomURI uri)
      }

toUrib :: Uri.URI -> Maybe (Urib.URIRef Urib.Absolute)
toUrib uri =
  rightToMaybe
    (Urib.parseURI
       Urib.laxURIParserOptions
       (Encoding.encodeUtf8 (renderUri uri)))

rightToMaybe :: Either a b -> Maybe b
rightToMaybe either_ =
  case either_ of
    Left _left -> Nothing
    Right right -> Just right

getRecentEntries :: Time.UTCTime -> Database -> [Entry]
getRecentEntries time database =
  List.sortOn
    (Ord.Down . itemTime . entryItem)
    (map
       (\(author, item) -> Entry {entryAuthor = author, entryItem = item})
       (concatMap
          (\(author, items) ->
             map
               (\item -> (author, item))
               (filter (isRecent time) (Set.toList items)))
          (Map.toList database)))

isRecent :: Time.UTCTime -> Item -> Bool
isRecent time item =
  itemTime item >= Time.addUTCTime (-8 * Time.nominalDay) time

data Entry = Entry
  { entryAuthor :: Author
  , entryItem :: Item
  } deriving (Eq, Ord, Show)

data Author = Author
  { authorName :: Text.Text
  , authorLink :: Uri.URI
  , authorFeed :: Feed
  } deriving (Eq, Ord, Show)

data Feed = Feed
  { feedType :: FeedType
  , feedLink :: Uri.URI
  } deriving (Eq, Ord, Show)

data FeedType
  = FeedTypeAtom
  | FeedTypeRss
  deriving (Eq, Ord, Show)

data Item = Item
  { itemTime :: Time.UTCTime
  , itemName :: Text.Text
  , itemLink :: Uri.URI
  } deriving (Eq, Ord, Show)

initialDatabase :: Database
initialDatabase =
  Map.fromDistinctAscList
    (zip (Set.toAscList initialAuthors) (repeat Set.empty))

toAuthor :: Text.Text -> Text.Text -> FeedType -> Text.Text -> Maybe Author
toAuthor name rawLink type_ rawFeed = do
  link <- parseUri rawLink
  feed <- parseUri rawFeed
  Just
    Author
      { authorName = name
      , authorLink = link
      , authorFeed = Feed {feedType = type_, feedLink = feed}
      }

parseUri :: Text.Text -> Maybe Uri.URI
parseUri text = Uri.parseAbsoluteURI (Text.unpack text)

renderUri :: Uri.URI -> Text.Text
renderUri uri = Text.pack (Uri.uriToString id uri "")

renderTime :: Text.Text -> Time.UTCTime -> Text.Text
renderTime format time =
  Text.pack (Time.formatTime Time.defaultTimeLocale (Text.unpack format) time)

{-
  TODO: Fetching the feeds for these authors doesn't work for one reason or
  another.

  "Abhiroop Sarkar"
  "Andrey Mokhov"
  "BEKK Open"
  "Bassel Mabsout"
  "Blue Dinosaur"
  "Capital Match Tech Blog"
  "Chris Warburton"
  "Codurance"
  "Csongor Kiss"
  "Edvard H\252binette"
  "Edward Z. Yang"
  "FP Complete"
  "Fintan Halpenny"
  "Francesco Gazzetta"
  "F\233lix Baylac-Jacqu\233"
  "GRAKN.AI"
  "Getty Ritter"
  "Gil Mizrahi"
  "Isaac Shapira"
  "J Haigh"
  "Javascript to Elm"
  "Joe Vargas"
  "John A De Goes"
  "Joseph Cieslik"
  "Keera Studios"
  "Matt Noonan"
  "Michael Gattozzi"
  "Michael Xavier"
  "Mike Ledger"
  "Monday Morning Haskell"
  "Morphism"
  "Nikita Volkov"
  "Pusher"
  "Russell O'Connor"
  "Saurabh Nanda"
  "SeatGeek"
  "The Haskell Cast"
  "The Joy of Haskell"
  "Tobias Dammers"
  "Tomas Petricek"
  "Uber Engineering"
  "Well-Typed"
-}
initialAuthors :: Set.Set Author
initialAuthors =
  Set.fromList
    (Maybe.catMaybes
       [ toAuthor "Abhiroop Sarkar" "https://abhiroop.github.io" FeedTypeRss "https://abhiroop.github.io/feed.xml"
       , toAuthor "Alex Beal" "http://www.usrsb.in" FeedTypeRss "http://www.usrsb.in/rss.xml"
       , toAuthor "Alexander Thiemann" "https://www.athiemann.net" FeedTypeRss "https://www.athiemann.net/feed.xml"
       , toAuthor "Alexis King" "https://lexi-lambda.github.io" FeedTypeAtom "https://lexi-lambda.github.io/feeds/all.atom.xml"
       , toAuthor "Alp Mestanogullari" "http://alpmestan.com" FeedTypeRss "http://alpmestan.com/rss.xml"
       , toAuthor "Andre Van Der Merwe" "http://www.andrevdm.com" FeedTypeAtom "http://www.andrevdm.com/atom.xml"
       , toAuthor "Andrej Bauer" "http://math.andrej.com" FeedTypeRss "http://math.andrej.com/feed/"
       , toAuthor "Andrey Mokhov" "https://blogs.ncl.ac.uk/andreymokhov/" FeedTypeRss "https://blogs.ncl.ac.uk/andreymokhov/feed/"
       , toAuthor "Andy Arvanitis" "http://andyarvanitis.com" FeedTypeAtom "https://feeds.feedburner.com/AndyArvanitis"
       , toAuthor "Andy Shiue" "https://andyshiue.github.io" FeedTypeRss "https://andyshiue.github.io/feed.xml"
       , toAuthor "Anthony Cowley" "https://www.arcadianvisions.com/blog/" FeedTypeRss "http://www.arcadianvisions.com/blog/rss.xml"
       , toAuthor "Arun Raghavan" "https://arunraghavan.net" FeedTypeRss "https://arunraghavan.net/feed/"
       , toAuthor "Attila Domokos" "http://www.adomokos.com" FeedTypeAtom "http://www.adomokos.com/feeds/posts/default"
       , toAuthor "Bartosz Milewski" "https://bartoszmilewski.com" FeedTypeRss "https://bartoszmilewski.com/feed/"
       , toAuthor "Bassel Mabsout" "https://ipfs.io/ipfs/QmfN5DojVnEsf1Une3DFwfUiFpfWnQf31f61qgybiXVeQE/blog/" FeedTypeRss "https://ipfs.io/ipfs/QmfN5DojVnEsf1Une3DFwfUiFpfWnQf31f61qgybiXVeQE/index.xml"
       , toAuthor "BEKK Open" "https://open.bekk.no" FeedTypeRss "https://open.bekk.no/feed/Technology"
       , toAuthor "Ben Gamari" "https://bgamari.github.io" FeedTypeRss "https://bgamari.github.io/rss.xml"
       , toAuthor "Benjamin Kovach" "https://www.kovach.me" FeedTypeAtom "https://www.kovach.me/atom.xml"
       , toAuthor "Blue Dinosaur" "https://blue-dinosaur.github.io" FeedTypeRss "https://blue-dinosaur.github.io/feed.xml"
       , toAuthor "Brent Yorgey" "https://byorgey.wordpress.com" FeedTypeRss "https://byorgey.wordpress.com/feed/"
       , toAuthor "Brian McKenna" "https://brianmckenna.org/blog/" FeedTypeAtom "https://brianmckenna.org/blog/feed"
       , toAuthor "Capital Match Tech Blog" "https://tech-blog.capital-match.com" FeedTypeRss "https://tech-blog.capital-match.com/feed.rss"
       , toAuthor "Carl Baatz" "http://baatz.io" FeedTypeAtom "http://baatz.io/atom.xml"
       , toAuthor "Carlos Morera" "https://carlosmchica.github.io" FeedTypeRss "https://carlosmchica.github.io/feed.xml"
       , toAuthor "Chad Austin" "https://chadaustin.me" FeedTypeAtom "http://feeds2.feedburner.com/chadaustin"
       , toAuthor "Channable" "https://tech.channable.com" FeedTypeAtom "https://tech.channable.com/atom.xml"
       , toAuthor "Chris Allen" "http://bitemyapp.com" FeedTypeAtom "http://bitemyapp.com/atom.xml"
       , toAuthor "Chris Done" "https://chrisdone.com" FeedTypeRss "https://chrisdone.com/rss.xml"
       , toAuthor "Chris Ford" "https://literateprogrammer.blogspot.com" FeedTypeAtom "https://literateprogrammer.blogspot.com/feeds/posts/default"
       , toAuthor "Chris Martin" "https://chris-martin.org" FeedTypeRss "https://chris-martin.org/rss.xml"
       , toAuthor "Chris Penner" "https://chrispenner.ca" FeedTypeAtom "https://chrispenner.ca/atom.xml"
       , toAuthor "Chris Smith" "https://cdsmith.wordpress.com" FeedTypeRss "https://cdsmith.wordpress.com/feed/"
       , toAuthor "Chris Warburton" "http://chriswarbo.net" FeedTypeRss "http://chriswarbo.net/blog.atom"
       , toAuthor "Chris" "https://two-wrongs.com" FeedTypeRss "https://two-wrongs.com/feed.xml"
       , toAuthor "Christian Charukiewicz" "https://charukiewi.cz" FeedTypeAtom "https://charukiewi.cz/atom.xml"
       , toAuthor "Code Podcast" "https://codepodcast.com" FeedTypeRss "https://feeds.soundcloud.com/users/soundcloud:users:201515747/sounds.rss"
       , toAuthor "Codurance" "https://codurance.com" FeedTypeRss "https://codurance.com/atom.xml"
       , toAuthor "Cody Goodman" "https://codygman.github.io" FeedTypeAtom "https://codygman.github.io/atom.xml"
       , toAuthor "Colin Woodbury" "https://www.fosskers.ca" FeedTypeRss "https://www.fosskers.ca/rss-en"
       , toAuthor "Concert" "https://medium.com/@concertdaw" FeedTypeRss "https://medium.com/feed/@concertdaw"
       , toAuthor "Csongor Kiss" "http://kcsongor.github.io" FeedTypeRss "http://kcsongor.github.io/feed.xml"
       , toAuthor "Dan Burton" "https://unknownparallel.wordpress.com" FeedTypeRss "https://unknownparallel.wordpress.com/feed/"
       , toAuthor "Dan Oprescu" "https://trandi.wordpress.com" FeedTypeRss "https://trandi.wordpress.com/feed/"
       , toAuthor "Daniel Bolivar" "https://www.dabolivar.com" FeedTypeRss "https://www.dabolivar.com/index.xml"
       , toAuthor "Daniel Patterson" "https://dbp.io" FeedTypeRss "https://dbp.io/rss.xml"
       , toAuthor "Daniel Wright" "https://dpwright.com" FeedTypeAtom "https://dpwright.com/atom.xml"
       , toAuthor "Danny Gratzer" "https://jozefg.bitbucket.io" FeedTypeRss "https://jozefg.bitbucket.io/rss.xml"
       , toAuthor "David Joyner" "https://medium.com/@djoyner" FeedTypeRss "https://medium.com/feed/@djoyner"
       , toAuthor "David Lettier" "https://lettier.github.io" FeedTypeRss "https://lettier.github.io/rss.xml"
       , toAuthor "Deni Bertovic" "https://denibertovic.com" FeedTypeRss "https://denibertovic.com/rss.xml"
       , toAuthor "Dennis Felsing" "https://hookrace.net" FeedTypeAtom "https://hookrace.net/blog/feed/"
       , toAuthor "Dennis Gosnell" "https://functor.tokyo" FeedTypeAtom "https://functor.tokyo/blog/feed"
       , toAuthor "Divam" "https://dfordivam.github.io" FeedTypeAtom "https://dfordivam.github.io/atom.xml"
       , toAuthor "Echo Nolan" "http://www.echonolan.net" FeedTypeAtom "http://www.echonolan.net/atom.xml"
       , toAuthor "Edvard H\xfc\&binette" "https://m0ar.github.io/safe-streaming/" FeedTypeRss "https://m0ar.github.io/safe-streaming/feed.xml"
       , toAuthor "Edward Kmett" "http://comonad.com/reader/" FeedTypeRss "http://comonad.com/reader/feed/"
       , toAuthor "Edward Z. Yang" "http://blog.ezyang.com" FeedTypeRss "https://feeds.feedburner.com/ezyang"
       , toAuthor "Eli Bendersky" "https://eli.thegreenplace.net" FeedTypeAtom "https://eli.thegreenplace.net/feeds/all.atom.xml"
       , toAuthor "Eta Programming Language" "https://blog.eta-lang.org" FeedTypeRss "https://blog.eta-lang.org/feed"
       , toAuthor "Ezequiel Alvarez" "http://clrnd.com.ar" FeedTypeAtom "http://clrnd.com.ar/atom.xml"
       , toAuthor "F\xe9lix Baylac-Jacqu\xe9" "https://alternativebit.fr" FeedTypeAtom "https://alternativebit.fr/posts/index.xml"
       , toAuthor "Fintan Halpenny" "https://medium.com/@fintan.halpenny" FeedTypeAtom "https://medium.com/feed/@fintan.halpenny"
       , toAuthor "FP Complete" "https://www.fpcomplete.com" FeedTypeRss "https://www.fpcomplete.com/blog/atom.xml"
       , toAuthor "Francesco Gazzetta" "https://fgaz.me" FeedTypeRss "https://fgaz.me/atom.xml"
       , toAuthor "Francesco Mazzoli" "https://mazzo.li" FeedTypeAtom "https://mazzo.li/atom.xml"
       , toAuthor "Fredrik Harrysson" "https://medium.com/@folsen" FeedTypeRss "https://medium.com/feed/@folsen"
       , toAuthor "Front Row" "http://tech.frontrowed.com" FeedTypeAtom "http://tech.frontrowed.com/feed.xml"
       , toAuthor "Functional Geekery" "https://www.functionalgeekery.com" FeedTypeRss "https://www.functionalgeekery.com/feed/"
       , toAuthor "Gabriel Gonzalez" "http://www.haskellforall.com" FeedTypeAtom "http://www.haskellforall.com/feeds/posts/default"
       , toAuthor "Getty Ritter" "https://blog.infinitenegativeutility.com" FeedTypeRss "https://blog.infinitenegativeutility.com/rss"
       , toAuthor "Gil Mizrahi" "https://gilmi.me" FeedTypeRss "https://gilmi.me/rss"
       , toAuthor "Google Summer of Code" "https://summerofcode.withgoogle.com" FeedTypeAtom "http://feeds.feedburner.com/GoogleOpenSourceBlog"
       , toAuthor "GRAKN.AI" "https://blog.grakn.ai" FeedTypeAtom "https://blog.grakn.ai/feed"
       , toAuthor "Gustav Behm" "https://rootmos.github.io" FeedTypeAtom "https://rootmos.github.io/feed.xml"
       , toAuthor "Hackage" "https://blog.hackage.haskell.org" FeedTypeRss "https://blog.hackage.haskell.org/rss.xml"
       , toAuthor "Harold Carr" "http://haroldcarr.com" FeedTypeAtom "http://haroldcarr.com/atom.xml"
       , toAuthor "Haskell at Work" "https://haskell-at-work.com" FeedTypeAtom "https://haskell-at-work.com/atom.xml"
       , toAuthor "Haskell for Mac" "http://blog.haskellformac.com" FeedTypeRss "http://blog.haskellformac.com/feed"
       , toAuthor "Haskell Tools" "https://haskelltools.blogspot.com" FeedTypeAtom "https://haskelltools.blogspot.com/feeds/posts/default"
       , toAuthor "Haskell Weekly" "https://haskellweekly.news" FeedTypeAtom "https://haskellweekly.news/haskell-weekly.atom"
       , toAuthor "Henri Verroken" "https://deliquus.com" FeedTypeAtom "https://deliquus.com/atom.xml"
       , toAuthor "HolidayCheck" "http://techblog.holidaycheck.com" FeedTypeRss "http://techblog.holidaycheck.com/feed.xml"
       , toAuthor "Hypothesis" "https://hypothesis.works" FeedTypeRss "https://hypothesis.works/articles/feed/"
       , toAuthor "Igal Tabachnik" "https://hmemcpy.com" FeedTypeAtom "https://hmemcpy.com/atom.xml"
       , toAuthor "IRIS Connect" "https://engineers.irisconnect.net" FeedTypeRss "https://engineers.irisconnect.net/rss.xml"
       , toAuthor "Isaac Elliott" "http://blog.ielliott.io" FeedTypeAtom "http://blog.ielliott.io/feed.xml"
       , toAuthor "Isaac Shapira" "http://mutanatum.com" FeedTypeRss "http://mutanatum.com/atom.xml"
       , toAuthor "J Haigh" "https://debugsteven.github.io" FeedTypeRss "https://debugsteven.github.io/feed.xml"
       , toAuthor "Jacob Errington" "https://jerrington.me" FeedTypeAtom "https://jerrington.me/atom.xml"
       , toAuthor "Jake Pittis" "https://jpittis.ca" FeedTypeRss "https://jpittis.ca/index.xml"
       , toAuthor "Jan Mas Rovira" "https://janmasrovira.gitlab.io/ascetic-slug/" FeedTypeRss "https://janmasrovira.gitlab.io/ascetic-slug/index.xml"
       , toAuthor "Jared Weakly" "https://jaredweakly.com" FeedTypeRss "https://jaredweakly.com/feed/"
       , toAuthor "Jaro Reinders" "https://noughtmare.gitlab.io" FeedTypeAtom "https://noughtmare.gitlab.io/atom.xml"
       , toAuthor "Jaseem Abid" "https://jaseemabid.github.io" FeedTypeRss "https://jaseemabid.github.io/feed.xml"
       , toAuthor "Jason Shipman" "https://jship.github.io" FeedTypeAtom "https://jship.github.io/atom.xml"
       , toAuthor "Jasper Van der Jeugt" "https://jaspervdj.be" FeedTypeRss "https://jaspervdj.be/rss.xml"
       , toAuthor "Javascript to Elm" "http://jstoelm.com" FeedTypeRss "https://jstoelm.libsyn.com/rss"
       , toAuthor "Jean-Louis Giordano" "https://jawaninja.com" FeedTypeAtom "https://jawaninja.com/atom.xml"
       , toAuthor "Jens Petersen" "https://juhp.blogspot.com" FeedTypeAtom "https://juhp.blogspot.com/feeds/posts/default"
       , toAuthor "Jeremy Mikkola" "http://jeremymikkola.com" FeedTypeAtom "http://jeremymikkola.com/atom.xml"
       , toAuthor "Jeroen Keiren" "https://www.jeroenkeiren.nl" FeedTypeRss "https://www.jeroenkeiren.nl/feed/"
       , toAuthor "Jezen Thomas" "https://jezenthomas.com" FeedTypeRss "https://jezenthomas.com/feed.xml"
       , toAuthor "Joachim Breitner" "https://www.joachim-breitner.de" FeedTypeRss "https://www.joachim-breitner.de/blog_feed.rss"
       , toAuthor "Joe Nelson" "https://begriffs.com" FeedTypeAtom "https://begriffs.com/atom.xml"
       , toAuthor "Joe Vargas" "http://jxv.io" FeedTypeRss "http://jxv.io/blog/rss.xml"
       , toAuthor "John A De Goes" "http://degoes.net" FeedTypeRss "http://degoes.net/feed.xml"
       , toAuthor "John Baez" "https://johncarlosbaez.wordpress.com" FeedTypeRss "https://johncarlosbaez.wordpress.com/feed/"
       , toAuthor "John Mendon\xe7\&a" "https://mendo.zone" FeedTypeAtom "https://mendo.zone/feed.xml"
       , toAuthor "Jonathan Fischoff" "https://medium.com/@jonathangfischoff" FeedTypeRss "https://medium.com/feed/@jonathangfischoff"
       , toAuthor "Jonathan Lange" "https://jml.io" FeedTypeAtom "https://jml.io/feeds/all.atom.xml"
       , toAuthor "Joseph Cieslik" "https://torchhound.github.io" FeedTypeAtom "https://torchhound.github.io/rss.xml"
       , toAuthor "Juan Pedro Villa Isaza" "https://jpvillaisaza.github.io" FeedTypeAtom "https://jpvillaisaza.github.io/feed"
       , toAuthor "Julie Moronuki" "https://argumatronic.com" FeedTypeRss "https://argumatronic.com/rss.xml"
       , toAuthor "Justin Le" "https://blog.jle.im" FeedTypeRss "http://feeds.feedburner.com/incodeblog"
       , toAuthor "Jyri-Matti L\xe4hteenm\xe4ki" "https://blog.lahteenmaki.net" FeedTypeRss "https://blog.lahteenmaki.net/rss.xml"
       , toAuthor "Keera Studios" "http://keera.co.uk/blog/" FeedTypeRss "http://keera.co.uk/blog/feed/"
       , toAuthor "Kris Jenkins" "http://blog.jenkster.com" FeedTypeRss "https://feeds.feedburner.com/KrisJenkinsBlog"
       , toAuthor "Kwang Yul Seo" "https://kseo.github.io" FeedTypeAtom "https://kseo.github.io/atom.xml"
       , toAuthor "Kyle Kingsbury" "https://aphyr.com" FeedTypeAtom "https://aphyr.com/posts.atom"
       , toAuthor "Lambda the Ultimate" "http://lambda-the-ultimate.org" FeedTypeRss "http://lambda-the-ultimate.org/rss.xml"
       , toAuthor "Laurence Emms" "https://whatthefunctional.wordpress.com" FeedTypeRss "https://whatthefunctional.wordpress.com/feed/"
       , toAuthor "Li-yao Xia" "https://blog.poisson.chat" FeedTypeRss "https://blog.poisson.chat/rss.xml"
       , toAuthor "Libby Horacek" "https://medium.com/@horrorcheck" FeedTypeRss "https://medium.com/feed/@horrorcheck"
       , toAuthor "LiquidHaskell" "https://ucsd-progsys.github.io/liquidhaskell-blog/" FeedTypeAtom "https://ucsd-progsys.github.io/liquidhaskell-blog/atom.xml"
       , toAuthor "Luis Pedro Coelho" "https://metarabbit.wordpress.com" FeedTypeRss "https://metarabbit.wordpress.com/feed/"
       , toAuthor "Luke Picciau" "https://itscode.red" FeedTypeRss "https://itscode.red/index.xml"
       , toAuthor "Marcelo Zabani" "https://mzabani.wordpress.com" FeedTypeRss "https://mzabani.wordpress.com/feed"
       , toAuthor "Marco Sampellegrini" "https://alpacaaa.net/blog/" FeedTypeRss "https://alpacaaa.net/blog/index.xml"
       , toAuthor "Mark Karpov" "https://markkarpov.com" FeedTypeAtom "https://markkarpov.com/feed.atom"
       , toAuthor "Mark Seemann" "http://blog.ploeh.dk" FeedTypeAtom "http://blog.ploeh.dk/atom.xml"
       , toAuthor "Mark Wotton" "https://www.shimweasel.com" FeedTypeAtom "https://www.shimweasel.com/atom.xml"
       , toAuthor "Matt Noonan" "http://storm-country.com" FeedTypeRss "http://storm-country.com/rss"
       , toAuthor "Matt Parsons" "https://www.parsonsmatt.org" FeedTypeRss "https://www.parsonsmatt.org/feed.xml"
       , toAuthor "Matthew Mongeau" "https://halogenandtoast.com" FeedTypeRss "https://halogenandtoast.com/rss/"
       , toAuthor "Matthew Pickering" "https://mpickering.github.io" FeedTypeAtom "https://mpickering.github.io/atom.xml"
       , toAuthor "Michael Burge" "http://www.michaelburge.us" FeedTypeAtom "http://www.michaelburge.us/feed.xml"
       , toAuthor "Michael Gattozzi" "https://mgattozzi.com" FeedTypeAtom "https://mgattozzi.com/feed"
       , toAuthor "Michael Snoyman" "https://www.snoyman.com" FeedTypeAtom "https://www.snoyman.com/feed"
       , toAuthor "Michael Xavier" "https://michaelxavier.net" FeedTypeAtom "https://michaelxavier.net/rss.xml"
       , toAuthor "Microsoft Research Podcast" "https://www.microsoft.com/en-us/research/blog/category/podcast/" FeedTypeRss "https://www.microsoft.com/en-us/research/blog/category/podcast/feed/"
       , toAuthor "Mike Ledger" "https://quasimal.com" FeedTypeRss "https://quasimal.com/feed.xml"
       , toAuthor "Mikhail Glushenkov" "http://coldwa.st/e/" FeedTypeAtom "http://feeds.feedburner.com/ChurningAndChurning"
       , toAuthor "Mistral Contrastin" "https://dodisturb.me" FeedTypeAtom "https://dodisturb.me/atom.xml"
       , toAuthor "Monday Morning Haskell" "https://mmhaskell.com" FeedTypeRss "https://mmhaskell.com/blog?format=rss"
       , toAuthor "Moritz Kiefer" "https://purelyfunctional.org" FeedTypeAtom "https://purelyfunctional.org/atom.xml"
       , toAuthor "Morphism" "https://www.morphism.tech" FeedTypeRss "https://www.morphism.tech/feed/"
       , toAuthor "Nam C." "https://namc.in" FeedTypeAtom "https://namc.in/feed.xml"
       , toAuthor "Nathan Maxson" "https://joyfulmantis.github.io" FeedTypeAtom "https://joyfulmantis.github.io/atom.xml"
       , toAuthor "Neil Mitchell" "https://neilmitchell.blogspot.com" FeedTypeAtom "https://neilmitchell.blogspot.com/feeds/posts/default"
       , toAuthor "Nicolas Mattia" "http://nmattia.com" FeedTypeAtom "http://nmattia.com/atom.xml?type=blog"
       , toAuthor "Nikita Volkov" "https://nikita-volkov.github.io" FeedTypeRss "https://nikita-volkov.github.io/feed.xml"
       , toAuthor "Noon van der Silk" "https://silky.github.io" FeedTypeAtom "https://silky.github.io/atom.xml"
       , toAuthor "Nuno Alexandre" "https://nunoalexandre.com" FeedTypeRss "https://nunoalexandre.com/feed.xml"
       , toAuthor "O\x308mer Sinan A\x306\&acan" "http://osa1.net" FeedTypeAtom "http://osa1.net/rss.xml"
       , toAuthor "Oleg Grenrus" "http://oleg.fi" FeedTypeAtom "http://oleg.fi/gists/atom.xml"
       , toAuthor "Oliver Charles" "https://ocharles.org.uk" FeedTypeRss "https://ocharles.org.uk/blog/posts.rss"
       , toAuthor "Osanai Kazuyoshi" "http://syocy.hatenablog.com" FeedTypeAtom "http://syocy.hatenablog.com/feed"
       , toAuthor "Oskar Wickstr\xf6m" "https://wickstrom.tech" FeedTypeRss "https://wickstrom.tech/feed.xml"
       , toAuthor "Parnell Springmeyer" "https://ixmatus.net" FeedTypeAtom "https://ixmatus.net/atom.xml"
       , toAuthor "Patrick Thompson" "http://blog.sumtypeofway.com" FeedTypeRss "http://blog.sumtypeofway.com/rss/"
       , toAuthor "Paul Bone" "https://paul.bone.id.au" FeedTypeRss "http://paul.bone.id.au/blog.xml"
       , toAuthor "Paul Chiusano" "https://pchiusano.github.io" FeedTypeRss "https://pchiusano.github.io/feed.xml"
       , toAuthor "Paul Johnson" "https://paulspontifications.blogspot.com" FeedTypeAtom "https://paulspontifications.blogspot.com/feeds/posts/default"
       , toAuthor "Payton Turnage" "https://paytonturnage.com" FeedTypeRss "https://paytonturnage.com/feed.xml"
       , toAuthor "Phil Freeman" "https://medium.com/@paf31" FeedTypeRss "https://medium.com/feed/@paf31"
       , toAuthor "Philip Wadler" "https://wadler.blogspot.com" FeedTypeAtom "https://wadler.blogspot.com/feeds/posts/default"
       , toAuthor "Philipp Maier" "https://blog.akii.de" FeedTypeAtom "https://blog.akii.de/feed.atom"
       , toAuthor "Philipp Schuster" "https://haskellexists.blogspot.com" FeedTypeAtom "https://haskellexists.blogspot.com/feeds/posts/default"
       , toAuthor "Piyush P. Kurur" "https://cse.iitk.ac.in/users/ppk/" FeedTypeAtom "https://cse.iitk.ac.in/users/ppk/posts/feeds/atom.xml"
       , toAuthor "Programming Research Laboratory" "https://prl.ccs.neu.edu/blog/" FeedTypeAtom "https://prl.ccs.neu.edu/blog/feeds/all.atom.xml"
       , toAuthor "Pusher" "https://making.pusher.com" FeedTypeRss "https://making.pusher.com/feed.xml"
       , toAuthor "Queensland Functional Programming Lab" "https://qfpl.io" FeedTypeAtom "https://qfpl.io/atom.xml"
       , toAuthor "Quentin Duval" "https://deque.blog" FeedTypeRss "https://deque.blog/feed/"
       , toAuthor "Ramakrishnan Muthukrishnan" "https://rkrishnan.org" FeedTypeAtom "https://rkrishnan.org/atom.xml"
       , toAuthor "Richard Cook" "https://blog.rcook.org" FeedTypeAtom "https://blog.rcook.org/atom.xml"
       , toAuthor "Richard Eisenberg" "https://typesandkinds.wordpress.com" FeedTypeRss "https://typesandkinds.wordpress.com/feed/"
       , toAuthor "Richard Feldman" "https://dev.to/rtfeldman" FeedTypeRss "https://dev.to/feed/rtfeldman"
       , toAuthor "Roman Cheplyaka" "https://ro-che.info" FeedTypeRss "https://ro-che.info/articles/rss.xml"
       , toAuthor "Russell O'Connor" "http://r6.ca" FeedTypeRss "http://r6.ca/blog/feed.atom"
       , toAuthor "Ryan Scott" "https://ryanglscott.github.io" FeedTypeRss "https://ryanglscott.github.io/feed.xml"
       , toAuthor "Sam Tay" "https://samtay.github.io" FeedTypeAtom "https://samtay.github.io/atom.xml"
       , toAuthor "Samuel G\xe9lineau" "https://gelisam.blogspot.com" FeedTypeAtom "https://gelisam.blogspot.com/feeds/posts/default"
       , toAuthor "Sandy Maguire" "http://reasonablypolymorphic.com" FeedTypeAtom "http://reasonablypolymorphic.com/atom.xml"
       , toAuthor "Sascha Wilde" "http://blogs.intevation.de/wilde/" FeedTypeRss "http://blogs.intevation.de/wilde/index.xml"
       , toAuthor "Saurabh Nanda" "https://medium.com/@saurabhnanda" FeedTypeAtom "https://medium.com/feed/@saurabhnanda"
       , toAuthor "School of Haskell" "https://www.schoolofhaskell.com" FeedTypeAtom "https://www.schoolofhaskell.com/recent-content/feed"
       , toAuthor "Scott Nonnenberg" "https://blog.scottnonnenberg.com" FeedTypeAtom "https://blog.scottnonnenberg.com/atom.xml"
       , toAuthor "SeatGeek" "https://chairnerd.seatgeek.com" FeedTypeRss "https://chairnerd.seatgeek.com/atom.xml"
       , toAuthor "Sebastian Graf" "http://fixpt.de" FeedTypeAtom "http://fixpt.de/atom.xml"
       , toAuthor "Sergei Trofimovich" "https://trofi.github.io" FeedTypeAtom "https://trofi.github.io/feed/atom.xml"
       , toAuthor "Simon Marlow" "https://simonmar.github.io" FeedTypeAtom "https://simonmar.github.io/atom.xml"
       , toAuthor "Simon Thompson" "https://profsjt.blogspot.com" FeedTypeAtom "https://profsjt.blogspot.com/feeds/posts/default"
       , toAuthor "Small Improvements" "https://tech.small-improvements.com" FeedTypeRss "https://tech.small-improvements.com/feed/"
       , toAuthor "Snap Framework" "http://snapframework.com/blog" FeedTypeAtom "http://snapframework.com/blog/feed.xml"
       , toAuthor "Spock" "https://www.spock.li/blog/" FeedTypeRss "https://www.spock.li/feed.xml"
       , toAuthor "Stack Builders" "https://www.stackbuilders.com" FeedTypeAtom "https://www.stackbuilders.com/tutorials/atom.xml"
       , toAuthor "Stackage" "https://www.stackage.org" FeedTypeAtom "https://www.stackage.org/blog/feed"
       , toAuthor "Stefano Dacchille" "https://futtetennismo.me" FeedTypeAtom "https://futtetennismo.me/feed.xml"
       , toAuthor "Stephan Boyer" "https://www.stephanboyer.com" FeedTypeAtom "https://www.stephanboyer.com/atom"
       , toAuthor "Stephen Diehl" "http://www.stephendiehl.com" FeedTypeAtom "http://www.stephendiehl.com/feed.atom"
       , toAuthor "Steve Shogren" "http://deliberate-software.com" FeedTypeRss "http://deliberate-software.com/index.xml"
       , toAuthor "Steven Syrek" "https://medium.com/@sjsyrek" FeedTypeRss "https://medium.com/feed/@sjsyrek"
       , toAuthor "Steven Vandevelde" "https://icidasset.com" FeedTypeAtom "https://icidasset.com/feed.xml"
       , toAuthor "Summer of Haskell" "https://summer.haskell.org/news.html" FeedTypeRss "http://summer.haskell.org/news.xml"
       , toAuthor "Sylvain Henry" "http://www.sylvain-henry.info/home/index.html" FeedTypeAtom "http://www.sylvain-henry.info/home/atom.xml"
       , toAuthor "Takt" "https://code.takt.com" FeedTypeRss "https://code.takt.com/feed"
       , toAuthor "Taylor Fausak" "http://taylor.fausak.me" FeedTypeAtom "http://taylor.fausak.me/sitemap.atom"
       , toAuthor "Tebello M. Thejane" "https://medium.com/@zyxoas" FeedTypeRss "https://medium.com/feed/@zyxoas"
       , toAuthor "The GHC Blog" "https://ghc.haskell.org/trac/ghc/blog" FeedTypeRss "https://ghc.haskell.org/trac/ghc/blog?format=rss"
       , toAuthor "The Haskell Cast" "https://www.haskellcast.com" FeedTypeRss "https://www.haskellcast.com/feed.xml"
       , toAuthor "The Initial Commit" "https://theinitialcommit.com" FeedTypeRss "https://theinitialcommit.com/feed.xml"
       , toAuthor "The Joy of Haskell" "https://joyofhaskell.com/blog.html" FeedTypeRss "https://joyofhaskell.com/rss.xml"
       , toAuthor "The regex blog" "http://regex.uk/blog/" FeedTypeRss "http://regex.uk/blog/rss.xml"
       , toAuthor "Tim Cotten" "https://blog.cotten.io" FeedTypeRss "https://blog.cotten.io/feed"
       , toAuthor "Tim Humphries" "https://teh.id.au" FeedTypeAtom "https://teh.id.au/posts/atom.xml"
       , toAuthor "Tobias Dammers" "https://programming.tobiasdammers.nl" FeedTypeRss "https://programming.tobiasdammers.nl/blog/rss"
       , toAuthor "Tom Prior" "http://www.prigrammer.com" FeedTypeRss "http://www.prigrammer.com/?feed=rss2"
       , toAuthor "Tomas Petricek" "http://tomasp.net" FeedTypeAtom "http://tomasp.net/rss.xml"
       , toAuthor "Tommaso Piazza" "http://allocinit.io" FeedTypeAtom "http://allocinit.io/feed.xml"
       , toAuthor "Tweag I/O" "https://www.tweag.io" FeedTypeRss "https://www.tweag.io/rss.xml"
       , toAuthor "Twilio" "https://www.twilio.com/blog/" FeedTypeRss "https://www.twilio.com/blog/feed"
       , toAuthor "Uber Engineering" "https://eng.uber.com" FeedTypeRss "https://eng.uber.com/feed/"
       , toAuthor "Vados" "https://vadosware.io" FeedTypeRss "https://vadosware.io/index.xml"
       , toAuthor "Vaibhav Sagar" "http://vaibhavsagar.com" FeedTypeAtom "http://vaibhavsagar.com/atom.xml"
       , toAuthor "Vincent Hanquez" "http://tab.snarc.org" FeedTypeAtom "http://tab.snarc.org/rss.xml"
       , toAuthor "Well-Typed" "https://www.well-typed.com/" FeedTypeRss "https://www.well-typed.com/blog/atom.xml"
       , toAuthor "Will Fancher" "https://elvishjerricco.github.io" FeedTypeRss "https://elvishjerricco.github.io/feed.xml"
       , toAuthor "Will Yager" "https://yager.io" FeedTypeRss "https://yager.io/feed/"
       , toAuthor "Windows Command Line Tools For Developers" "https://blogs.msdn.microsoft.com/commandline/" FeedTypeRss "https://blogs.msdn.microsoft.com/commandline/feed/"
       , toAuthor "Wit.ai" "https://medium.com/wit-ai" FeedTypeRss "https://medium.com/feed/wit-ai"
       , toAuthor "Yesod" "https://www.yesodweb.com" FeedTypeAtom "https://www.yesodweb.com/feed"
       , toAuthor "Zach Kessin" "https://get-finch.com" FeedTypeRss "http://get-finch.com/feed.xml"
       , toAuthor "Zw3rk Tech" "https://medium.com/@zw3rk" FeedTypeRss "https://medium.com/feed/@zw3rk"
       ])
