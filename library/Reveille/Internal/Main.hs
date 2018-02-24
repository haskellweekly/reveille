module Reveille.Internal.Main where

import qualified Control.Concurrent.Async as Async
import qualified Control.Concurrent.STM as Stm
import qualified Data.Set as Set
import qualified Network.HTTP.Client.TLS as Client
import qualified Reveille.Internal.Aggregator as Reveille
import qualified Reveille.Internal.Author as Reveille
import qualified Reveille.Internal.Database as Reveille
import qualified Reveille.Internal.Server as Reveille

defaultMain :: IO ()
defaultMain = do
  manager <- Client.newTlsManager
  database <- Stm.newTVarIO Reveille.initialDatabase

  Stm.atomically
    (mapM_
      (\result -> case result of
        Left authorError -> fail (show authorError)
        Right author ->
          Stm.modifyTVar database (Reveille.addDatabaseAuthor author)
      )
      initialAuthors
    )

  Async.race_
    (Reveille.startAggregator manager database)
    (Reveille.startServer database)

initialAuthors :: Set.Set (Either Reveille.AuthorError Reveille.Author)
initialAuthors = Set.fromList
  (map
    toAuthor
    [ ("Alex Beal", "http://www.usrsb.in", Just "http://www.usrsb.in/rss.xml")
    , ( "Alexis King"
      , "https://lexi-lambda.github.io"
      , Just "https://lexi-lambda.github.io/feeds/all.atom.xml"
      )
    , ( "Andre Van Der Merwe"
      , "http://www.andrevdm.com/"
      , Just "http://www.andrevdm.com/atom.xml"
      )
    , ( "Attila Domokos"
      , "http://www.adomokos.com"
      , Just "http://www.adomokos.com/feeds/posts/default"
      )
    , ("Ben Gamari", "https://tomsmalley.github.io", Nothing)
    , ( "Brent Yorgey"
      , "https://byorgey.wordpress.com"
      , Just "https://byorgey.wordpress.com/feed/"
      )
    , ( "Concert"
      , "https://medium.com/@concertdaw"
      , Just "https://medium.com/feed/@concertdaw"
      )
    , ( "Daniel Bolivar"
      , "https://www.dabolivar.com"
      , Just "https://www.dabolivar.com/index.xml"
      )
    , ("Daniel Patterson", "https://dbp.io", Just "https://dbp.io/rss.xml")
    , ( "Eli Bendersky"
      , "https://eli.thegreenplace.net"
      , Just "https://eli.thegreenplace.net/feeds/all.atom.xml"
      )
    , ( "FP Complete"
      , "https://www.fpcomplete.com"
      , Just "https://www.fpcomplete.com/blog/atom.xml"
      )
    , ( "Gabriel Gonzalez"
      , "http://www.haskellforall.com"
      , Just "http://www.haskellforall.com/feeds/posts/default"
      )
    , ( "Google Summer of Code"
      , "https://summerofcode.withgoogle.com"
      , Just "http://feeds.feedburner.com/GoogleOpenSourceBlog"
      )
    , ( "Haskell at Work"
      , "https://haskell-at-work.com"
      , Just
        "https://www.youtube.com/feeds/videos.xml?channel_id=UCUgxpaK7ySR-z6AXA5-uDuw"
      )
    , ( "Haskell Weekly"
      , "https://haskellweekly.news"
      , Just "https://haskellweekly.news/haskell-weekly.atom"
      )
    , ( "Humble Bundle"
      , "https://www.humblebundle.com"
      , Just "http://blog.humblebundle.com/rss"
      )
    , ( "Ibnu D. Aji"
      , "https://ibnuda.gitlab.io"
      , Nothing
      ) -- https://gitlab.com/ibnuda/ibnuda.gitlab.io/issues/2
    , ( "Joachim Breitner"
      , "https://www.joachim-breitner.de"
      , Just "https://www.joachim-breitner.de/blog_feed.rss"
      )
    , ( "Julie Moronuki"
      , "https://argumatronic.com"
      , Just "https://argumatronic.com/rss.xml"
      )
    , ( "Luke Picciau"
      , "https://itscode.red"
      , Just "https://itscode.red/index.xml"
      )
    , ( "Mark Karpov"
      , "https://markkarpov.com"
      , Just "https://markkarpov.com/feed.atom"
      )
    , ( "Matt Noonan"
      , "http://storm-country.com"
      , Just "http://storm-country.com/rss"
      )
    , ( "Michael Snoyman"
      , "https://www.snoyman.com"
      , Just "https://www.snoyman.com/feed"
      )
    , ( "Mistral Contrastin"
      , "https://dodisturb.me"
      , Just "https://dodisturb.me/atom.xml"
      )
    , ( "Monday Morning Haskell"
      , "https://mmhaskell.com"
      , Just "https://mmhaskell.com/blog?format=rss"
      )
    , ( "Nuno Alexandre"
      , "https://nunoalexandre.com"
      , Just "https://nunoalexandre.com/feed.xml"
      )
    , ( "Oskar Wickstr\xf6m"
      , "https://wickstrom.tech"
      , Just "https://wickstrom.tech/feed.xml"
      )
    , ( "Patrick Thompson"
      , "http://blog.sumtypeofway.com"
      , Just "http://blog.sumtypeofway.com/rss/"
      )
    , ( "Ryan Scott"
      , "https://ryanglscott.github.io"
      , Just "https://ryanglscott.github.io/feed.xml"
      )
    , ( "Sandy Maguire"
      , "http://reasonablypolymorphic.com"
      , Just "http://reasonablypolymorphic.com/atom.xml"
      )
    , ( "Siddharth Bhat"
      , "https://pixel-druid.com"
      , Nothing
      ) -- https://github.com/bollu/pixeldruid/issues/1
    , ( "Stackage"
      , "https://www.stackage.org"
      , Just "https://www.stackage.org/blog/feed"
      )
    , ( "Taylor Fausak"
      , "http://taylor.fausak.me"
      , Just "http://taylor.fausak.me/sitemap.atom"
      )
    , ("Tom Smalley", "https://tomsmalley.github.io", Nothing)
    , ( "Tweag I/O"
      , "https://www.tweag.io"
      , Just "https://www.tweag.io/rss.xml"
      )
    , ( "Vaibhav Sagar"
      , "http://vaibhavsagar.com"
      , Just "http://vaibhavsagar.com/atom.xml"
      )
    ]
  )

toAuthor
  :: (String, String, Maybe String)
  -> Either Reveille.AuthorError Reveille.Author
toAuthor (name, url, feed) = Reveille.toAuthor name url feed
