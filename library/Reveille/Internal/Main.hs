module Reveille.Internal.Main where

import qualified Control.Concurrent.Async as Async
import qualified Control.Concurrent.STM as Stm
import qualified Data.Set as Set
import qualified Network.Connection as Connection
import qualified Network.HTTP.Client as Client
import qualified Network.HTTP.Client.TLS as Client
import qualified Reveille.Internal.Aggregator as Aggregator
import qualified Reveille.Internal.Author as Author
import qualified Reveille.Internal.Database as Database
import qualified Reveille.Internal.Server as Server

defaultMain :: IO ()
defaultMain = do
  manager <- Client.newManager managerSettings
  database <- Stm.newTVarIO Database.initialDatabase

  Stm.atomically (mapM_ (addAuthor database) initialAuthors)

  Async.race_
    (Aggregator.startAggregator manager database)
    (Server.startServer database)

managerSettings :: Client.ManagerSettings
managerSettings = Client.mkManagerSettings
  (Connection.TLSSettingsSimple
    { Connection.settingDisableCertificateValidation = True
    , Connection.settingDisableSession = False
    , Connection.settingUseServerName = False
    }
  )
  Nothing

addAuthor
  :: Stm.TVar Database.Database
  -> Either Author.AuthorError Author.Author
  -> Stm.STM ()
addAuthor database result = case result of
  Left authorError -> fail (show authorError)
  Right author -> Stm.modifyTVar database (Database.addDatabaseAuthor author)

initialAuthors :: Set.Set (Either Author.AuthorError Author.Author)
initialAuthors =
  Set.fromList (map (uncurry3 Author.toAuthor) rawInitialAuthors)

uncurry3 :: (a -> b -> c -> d) -> (a, b, c) -> d
uncurry3 f (x, y, z) = f x y z

-- Includes authors from issues 97 through 80.
rawInitialAuthors :: [(String, String, Maybe String)]
rawInitialAuthors =
  [ ("Alex Beal", "http://www.usrsb.in", Just "http://www.usrsb.in/rss.xml")
  , ("Alexis King", "https://lexi-lambda.github.io", Just "https://lexi-lambda.github.io/feeds/all.atom.xml")
  , ("Andre Van Der Merwe", "http://www.andrevdm.com/", Just "http://www.andrevdm.com/atom.xml")
  , ("Andrey Mokhov", "https://blogs.ncl.ac.uk/andreymokhov/", Just "https://blogs.ncl.ac.uk/andreymokhov/feed/")
  , ("Arun Raghavan", "https://arunraghavan.net", Just "https://arunraghavan.net/feed/")
  , ("Attila Domokos", "http://www.adomokos.com", Just "http://www.adomokos.com/feeds/posts/default")
  , ("Bassel Mabsout", "https://ipfs.io/ipfs/QmfN5DojVnEsf1Une3DFwfUiFpfWnQf31f61qgybiXVeQE/blog/", Just "https://ipfs.io/ipfs/QmfN5DojVnEsf1Une3DFwfUiFpfWnQf31f61qgybiXVeQE/index.xml")
  , ("Ben Gamari", "https://bgamari.github.io", Just "https://bgamari.github.io/rss.xml")
  , ("Blue Dinosaur", "https://blue-dinosaur.github.io", Just "https://blue-dinosaur.github.io/feed.xml")
  , ("Brent Yorgey", "https://byorgey.wordpress.com", Just "https://byorgey.wordpress.com/feed/")
  , ("Capital Match Tech Blog", "https://tech-blog.capital-match.com", Just "https://tech-blog.capital-match.com/feed.rss")
  , ("Carlos Morera", "https://carlosmchica.github.io", Just "https://carlosmchica.github.io/feed.xml")
  , ("Chris Martin", "https://chris-martin.org", Just "https://chris-martin.org/rss.xml")
  , ("Chris Penner", "https://chrispenner.ca", Just "https://chrispenner.ca/atom.xml")
  , ("Chris Warburton", "http://chriswarbo.net", Just "http://chriswarbo.net/blog.atom")
  , ("Codurance", "https://codurance.com", Just "https://codurance.com/atom.xml")
  , ("Colin Woodbury", "http://fosskers.ca", Just "http://fosskers.ca/rss-en")
  , ("Concert", "https://medium.com/@concertdaw", Just "https://medium.com/feed/@concertdaw")
  , ("Csongor Kiss", "http://kcsongor.github.io", Just "http://kcsongor.github.io/feed.xml")
  , ("Dan Oprescu", "https://trandi.wordpress.com", Just "https://trandi.wordpress.com/feed/")
  , ("Daniel Bolivar", "https://www.dabolivar.com", Just "https://www.dabolivar.com/index.xml")
  , ("Daniel Patterson", "https://dbp.io", Just "https://dbp.io/rss.xml")
  , ("Divam", "https://dfordivam.github.io", Nothing) -- https://github.com/dfordivam/dfordivam.github.io/issues/1
  , ("Edward Kmett", "http://comonad.com/reader/", Just "http://comonad.com/reader/feed/")
  , ("Eli Bendersky", "https://eli.thegreenplace.net", Just "https://eli.thegreenplace.net/feeds/all.atom.xml")
  , ("Eli Bendersky", "https://eli.thegreenplace.net", Just "https://eli.thegreenplace.net/feeds/all.atom.xml")
  , ("F\xe9lix Baylac-Jacqu\xe9", "https://alternativebit.fr", Just "https://alternativebit.fr/posts/index.xml")
  , ("Fintan Halpenny", "https://medium.com/@fintan.halpenny", Just "https://medium.com/feed/@fintan.halpenny")
  , ("FP Complete", "https://www.fpcomplete.com", Just "https://www.fpcomplete.com/blog/atom.xml")
  , ("Front Row", "http://tech.frontrowed.com", Just "http://tech.frontrowed.com/feed.xml")
  , ("Functional Works", "https://functional.works-hub.com", Nothing) -- https://twitter.com/taylorfausak/status/971367969510092800
  , ("Gabriel Gonzalez", "http://www.haskellforall.com", Just "http://www.haskellforall.com/feeds/posts/default")
  , ("Getty Ritter", "https://blog.infinitenegativeutility.com", Just "https://blog.infinitenegativeutility.com/rss")
  , ("Gil Mizrahi", "https://gilmi.me", Just "https://gilmi.me/rss")
  , ("Giovanni Parra", "http://entulho.fiatjaf.alhur.es", Just "http://entulho.fiatjaf.alhur.es/feed.xml")
  , ("Google Summer of Code", "https://summerofcode.withgoogle.com", Just "http://feeds.feedburner.com/GoogleOpenSourceBlog")
  , ("Haskell at Work", "https://haskell-at-work.com", Just "https://haskell-at-work.com/atom.xml")
  , ("Haskell Serbia", "https://haskell-serbia.com", Nothing) -- https://github.com/haskell-serbia/haskell-serbia/issues/27
  , ("Haskell Weekly", "https://haskellweekly.news", Just "https://haskellweekly.news/haskell-weekly.atom")
  , ("HolidayCheck", "http://techblog.holidaycheck.com", Just "http://techblog.holidaycheck.com/feed.xml")
  , ("Humble Bundle", "https://www.humblebundle.com", Nothing) -- https://github.com/haskellweekly/reveille/issues/1
  , ("Ibnu D. Aji", "https://ibnuda.gitlab.io", Nothing) -- https://gitlab.com/ibnuda/ibnuda.gitlab.io/issues/2
  , ("Isaac Elliott", "http://blog.ielliott.io", Just "http://blog.ielliott.io/feed.xml")
  , ("Jan Mas Rovira", "https://janmasrovira.gitlab.io/ascetic-slug/", Just "https://janmasrovira.gitlab.io/ascetic-slug/index.xml")
  , ("Javascript to Elm", "http://jstoelm.com", Just "https://jstoelm.libsyn.com/rss")
  , ("Jean-Louis Giordano", "https://jawaninja.com", Just "https://jawaninja.com/atom.xml")
  , ("Joachim Breitner", "https://www.joachim-breitner.de", Just "https://www.joachim-breitner.de/blog_feed.rss")
  , ("Joe Vargas", "http://jxv.io", Just "http://jxv.io/blog/rss.xml")
  , ("John Mendon\xe7\&a", "https://mendo.zone", Just "https://mendo.zone/feed.xml")
  , ("Jonathan Merritt", "https://lancelet.github.io", Nothing) -- https://github.com/lancelet/blog-source/issues/1
  , ("Julie Moronuki", "https://argumatronic.com", Just "https://argumatronic.com/rss.xml")
  , ("Justin Le", "https://blog.jle.im", Just "http://feeds.feedburner.com/incodeblog")
  , ("Jyri-Matti L\xe4hteenm\xe4ki", "https://blog.lahteenmaki.net", Just "https://blog.lahteenmaki.net/rss.xml")
  , ("Laurence Emms", "https://whatthefunctional.wordpress.com", Just "https://whatthefunctional.wordpress.com/feed/")
  , ("LiquidHaskell", "https://ucsd-progsys.github.io/liquidhaskell-blog/", Just "https://ucsd-progsys.github.io/liquidhaskell-blog/atom.xml")
  , ("Luke Picciau", "https://itscode.red", Just "https://itscode.red/index.xml")
  , ("Marcelo Zabani", "https://mzabani.wordpress.com", Just "https://mzabani.wordpress.com/feed")
  , ("Mark Karpov", "https://markkarpov.com", Just "https://markkarpov.com/feed.atom")
  , ("Mateusz Kowalczyk", "http://fuuzetsu.co.uk/blog/", Just "http://fuuzetsu.co.uk/blog/atom.xml")
  , ("Matt Noonan", "http://storm-country.com", Just "http://storm-country.com/rss")
  , ("Matt Parsons", "https://www.parsonsmatt.org", Just "https://www.parsonsmatt.org/feed.xml")
  , ("Matthew Pickering", "https://mpickering.github.io", Just "https://mpickering.github.io/atom.xml")
  , ("Michael Snoyman", "https://www.snoyman.com", Just "https://www.snoyman.com/feed")
  , ("Microsoft Research Podcast", "https://www.microsoft.com/en-us/research/blog/category/podcast/", Just "https://www.microsoft.com/en-us/research/blog/category/podcast/feed/")
  , ("Mike Ledger", "https://quasimal.com", Just "https://quasimal.com/feed.xml")
  , ("Mistral Contrastin", "https://dodisturb.me", Just "https://dodisturb.me/atom.xml")
  , ("Monday Morning Haskell", "https://mmhaskell.com", Just "https://mmhaskell.com/blog?format=rss")
  , ("Moritz Kiefer", "https://purelyfunctional.org", Just "https://purelyfunctional.org/atom.xml")
  , ("Nam C.", "https://namc.in", Just "https://namc.in/feed.xml")
  , ("Neil Mitchell", "https://neilmitchell.blogspot.com", Just "https://neilmitchell.blogspot.com/feeds/posts/default")
  , ("Nuno Alexandre", "https://nunoalexandre.com", Just "https://nunoalexandre.com/feed.xml")
  , ("Oleg Grenrus", "http://oleg.fi", Just "http://oleg.fi/gists/atom.xml")
  , ("Osanai Kazuyoshi", "http://syocy.hatenablog.com", Just "http://syocy.hatenablog.com/feed")
  , ("Oskar Wickstr\xf6m", "https://wickstrom.tech", Just "https://wickstrom.tech/feed.xml")
  , ("Parnell Springmeyer", "https://ixmatus.net", Just "https://ixmatus.net/atom.xml")
  , ("Patrick Thompson", "http://blog.sumtypeofway.com", Just "http://blog.sumtypeofway.com/rss/")
  , ("Paul Johnson", "https://paulspontifications.blogspot.com", Just "https://paulspontifications.blogspot.com/feeds/posts/default")
  , ("Payton Turnage", "https://paytonturnage.com", Just "https://paytonturnage.com/feed.xml")
  , ("Queensland Functional Programming Lab", "https://qfpl.io", Just "https://qfpl.io/atom.xml")
  , ("Raphael Baron", "http://rbaron.net", Nothing) -- Sent email because no GitHub repo.
  , ("Renzo Carbonara", "https://ren.zone", Nothing) -- https://github.com/k0001/k0001.github.com/issues/1
  , ("Richard Cook", "https://blog.rcook.org", Just "https://blog.rcook.org/atom.xml")
  , ("Roman Cheplyaka", "https://ro-che.info", Just "https://ro-che.info/articles/rss.xml")
  , ("Ryan Scott", "https://ryanglscott.github.io", Just "https://ryanglscott.github.io/feed.xml")
  , ("Samuel G\xe9lineau", "https://gelisam.blogspot.com", Just "https://gelisam.blogspot.com/feeds/posts/default")
  , ("Sandy Maguire", "http://reasonablypolymorphic.com", Just "http://reasonablypolymorphic.com/atom.xml")
  , ("Saurabh Nanda", "https://medium.com/@saurabhnanda", Just "https://medium.com/feed/@saurabhnanda")
  , ("School of Haskell", "https://www.schoolofhaskell.com", Just "https://www.schoolofhaskell.com/recent-content/feed")
  , ("Sebastian Graf", "http://fixpt.de", Just "http://fixpt.de/atom.xml")
  , ("Siddharth Bhat", "https://pixel-druid.com", Nothing) -- https://github.com/bollu/pixeldruid/issues/1
  , ("Stack Builders", "https://www.stackbuilders.com", Nothing) -- https://twitter.com/taylorfausak/status/968103399593054208
  , ("Stackage", "https://www.stackage.org", Just "https://www.stackage.org/blog/feed")
  , ("Stefano Dacchille", "https://futtetennismo.me", Just "https://futtetennismo.me/feed.xml")
  , ("Stephen Diehl", "http://www.stephendiehl.com", Just "http://www.stephendiehl.com/feed.atom")
  , ("Steve Shogren", "http://deliberate-software.com", Just "http://deliberate-software.com/index.xml")
  , ("Steven Syrek", "https://medium.com/@sjsyrek", Just "https://medium.com/feed/@sjsyrek")
  , ("Summer of Haskell", "https://summer.haskell.org/news.html", Just "http://summer.haskell.org/news.xml")
  , ("Sylvain Henry", "http://www.sylvain-henry.info/home/index.html", Just "http://www.sylvain-henry.info/home/atom.xml")
  , ("Taylor Fausak", "http://taylor.fausak.me", Just "http://taylor.fausak.me/sitemap.atom")
  , ("The GHC Blog", "https://ghc.haskell.org/trac/ghc/blog", Just "https://ghc.haskell.org/trac/ghc/blog?format=rss")
  , ("Tom Smalley", "https://tomsmalley.github.io", Nothing) -- Not really a blog.
  , ("Tweag I/O", "https://www.tweag.io", Just "https://www.tweag.io/rss.xml")
  , ("Uver Engineering", "https://eng.uber.com", Just "https://eng.uber.com/feed/")
  , ("Vaibhav Sagar", "http://vaibhavsagar.com", Just "http://vaibhavsagar.com/atom.xml")
  , ("Well-Typed", "https://www.well-typed.com", Nothing) -- https://twitter.com/taylorfausak/status/972087304067207168
  , ("Zw3rk Tech", "https://medium.com/@zw3rk", Just "https://medium.com/feed/@zw3rk")
  ]
