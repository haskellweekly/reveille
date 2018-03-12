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

-- Includes authors from issues up to and including 97..
rawInitialAuthors :: [(String, String, Maybe String)]
rawInitialAuthors =
  [ ("Alex Beal", "http://www.usrsb.in", Just "http://www.usrsb.in/rss.xml")
  , ("Abhiroop Sarkar", "https://abhiroop.github.io", Just "https://abhiroop.github.io/feed.xml")
  , ("Alexander Thiemann", "https://www.athiemann.net", Just "https://www.athiemann.net/feed.xml")
  , ("Alexis King", "https://lexi-lambda.github.io", Just "https://lexi-lambda.github.io/feeds/all.atom.xml")
  , ("Alp Mestanogullari", "http://alpmestan.com", Just "http://alpmestan.com/rss.xml")
  , ("Andre Van Der Merwe", "http://www.andrevdm.com", Just "http://www.andrevdm.com/atom.xml")
  , ("Andrej Bauer", "http://math.andrej.com", Just "http://math.andrej.com/feed/")
  , ("Andrey Mokhov", "https://blogs.ncl.ac.uk/andreymokhov/", Just "https://blogs.ncl.ac.uk/andreymokhov/feed/")
  , ("Andy Arvanitis", "http://andyarvanitis.com", Just "https://feeds.feedburner.com/AndyArvanitis")
  , ("Andy Shiue", "https://andyshiue.github.io", Just "https://andyshiue.github.io/feed.xml")
  , ("Anthony Cowley", "https://www.arcadianvisions.com/blog/", Just "http://www.arcadianvisions.com/blog/rss.xml")
  , ("Arun Raghavan", "https://arunraghavan.net", Just "https://arunraghavan.net/feed/")
  , ("Attila Domokos", "http://www.adomokos.com", Just "http://www.adomokos.com/feeds/posts/default")
  , ("Bartosz Milewski", "https://bartoszmilewski.com", Just "https://bartoszmilewski.com/feed/")
  , ("Bassel Mabsout", "https://ipfs.io/ipfs/QmfN5DojVnEsf1Une3DFwfUiFpfWnQf31f61qgybiXVeQE/blog/", Just "https://ipfs.io/ipfs/QmfN5DojVnEsf1Une3DFwfUiFpfWnQf31f61qgybiXVeQE/index.xml")
  , ("BEKK Open", "https://open.bekk.no", Just "https://open.bekk.no/feed/Technology")
  , ("Ben Gamari", "https://bgamari.github.io", Just "https://bgamari.github.io/rss.xml")
  , ("Benjamin Kovach", "https://www.kovach.me", Just "https://www.kovach.me/atom.xml")
  , ("Blue Dinosaur", "https://blue-dinosaur.github.io", Just "https://blue-dinosaur.github.io/feed.xml")
  , ("Brent Yorgey", "https://byorgey.wordpress.com", Just "https://byorgey.wordpress.com/feed/")
  , ("Brian McKenna", "https://brianmckenna.org/blog/", Just "https://brianmckenna.org/blog/feed")
  , ("Capital Match Tech Blog", "https://tech-blog.capital-match.com", Just "https://tech-blog.capital-match.com/feed.rss")
  , ("Carl Baatz", "http://baatz.io", Just "http://baatz.io/atom.xml")
  , ("Carlos Morera", "https://carlosmchica.github.io", Just "https://carlosmchica.github.io/feed.xml")
  , ("Chad Austin", "https://chadaustin.me", Just "http://feeds2.feedburner.com/chadaustin")
  , ("Channable", "https://tech.channable.com", Just "https://tech.channable.com/atom.xml")
  , ("Chris Allen", "http://bitemyapp.com", Just "http://bitemyapp.com/atom.xml")
  , ("Chris Done", "https://chrisdone.com", Just "https://chrisdone.com/rss.xml")
  , ("Chris Ford", "https://literateprogrammer.blogspot.com", Just "https://literateprogrammer.blogspot.com/feeds/posts/default")
  , ("Chris Martin", "https://chris-martin.org", Just "https://chris-martin.org/rss.xml")
  , ("Chris Penner", "https://chrispenner.ca", Just "https://chrispenner.ca/atom.xml")
  , ("Chris Smith", "https://cdsmith.wordpress.com", Just "https://cdsmith.wordpress.com/feed/")
  , ("Chris Warburton", "http://chriswarbo.net", Just "http://chriswarbo.net/blog.atom")
  , ("Chris", "https://two-wrongs.com", Just "https://two-wrongs.com/feed.xml")
  , ("Christian Charukiewicz", "https://charukiewi.cz", Just "https://charukiewi.cz/atom.xml")
  , ("Code Podcast", "https://codepodcast.com", Just "https://feeds.soundcloud.com/users/soundcloud:users:201515747/sounds.rss")
  , ("Codurance", "https://codurance.com", Just "https://codurance.com/atom.xml")
  , ("Cody Goodman", "https://codygman.github.io", Just "https://codygman.github.io/atom.xml")
  , ("Colin Woodbury", "http://fosskers.ca", Just "http://fosskers.ca/rss-en")
  , ("Concert", "https://medium.com/@concertdaw", Just "https://medium.com/feed/@concertdaw")
  , ("Csongor Kiss", "http://kcsongor.github.io", Just "http://kcsongor.github.io/feed.xml")
  , ("Dan Burton", "https://unknownparallel.wordpress.com", Just "https://unknownparallel.wordpress.com/feed/")
  , ("Dan Oprescu", "https://trandi.wordpress.com", Just "https://trandi.wordpress.com/feed/")
  , ("Daniel Bolivar", "https://www.dabolivar.com", Just "https://www.dabolivar.com/index.xml")
  , ("Daniel Patterson", "https://dbp.io", Just "https://dbp.io/rss.xml")
  , ("Daniel Wright", "https://dpwright.com", Just "https://dpwright.com/atom.xml")
  , ("Danny Gratzer", "https://jozefg.bitbucket.io", Just "https://jozefg.bitbucket.io/rss.xml")
  , ("David Joyner", "https://medium.com/@djoyner", Just "https://medium.com/feed/@djoyner")
  , ("David Lettier", "https://lettier.github.io", Just "https://lettier.github.io/rss.xml")
  , ("Deni Bertovic", "https://denibertovic.com", Just "https://denibertovic.com/rss.xml")
  , ("Dennis Felsing", "https://hookrace.net", Just "https://hookrace.net/blog/feed/")
  , ("Dennis Gosnell", "https://functor.tokyo", Just "https://functor.tokyo/blog/feed")
  , ("Echo Nolan", "http://www.echonolan.net", Just "http://www.echonolan.net/atom.xml")
  , ("Edvard H\xfc\&binette", "https://m0ar.github.io/safe-streaming/", Just "https://m0ar.github.io/safe-streaming/feed.xml")
  , ("Edward Kmett", "http://comonad.com/reader/", Just "http://comonad.com/reader/feed/")
  , ("Edward Z. Yang", "http://blog.ezyang.com", Just "https://feeds.feedburner.com/ezyang")
  , ("Eli Bendersky", "https://eli.thegreenplace.net", Just "https://eli.thegreenplace.net/feeds/all.atom.xml")
  , ("Eta Programming Language", "https://blog.eta-lang.org", Just "https://blog.eta-lang.org/feed")
  , ("Ezequiel Alvarez", "http://clrnd.com.ar", Just "http://clrnd.com.ar/atom.xml")
  , ("F\xe9lix Baylac-Jacqu\xe9", "https://alternativebit.fr", Just "https://alternativebit.fr/posts/index.xml")
  , ("Fintan Halpenny", "https://medium.com/@fintan.halpenny", Just "https://medium.com/feed/@fintan.halpenny")
  , ("FP Complete", "https://www.fpcomplete.com", Just "https://www.fpcomplete.com/blog/atom.xml")
  , ("Francesco Gazzetta", "https://fgaz.me", Just "https://fgaz.me/atom.xml")
  , ("Francesco Mazzoli", "https://mazzo.li", Just "https://mazzo.li/atom.xml")
  , ("Fredrik Harrysson", "https://medium.com/@folsen", Just "https://medium.com/feed/@folsen")
  , ("Front Row", "http://tech.frontrowed.com", Just "http://tech.frontrowed.com/feed.xml")
  , ("Functional Geekery", "https://www.functionalgeekery.com", Just "https://www.functionalgeekery.com/feed/")
  , ("Gabriel Gonzalez", "http://www.haskellforall.com", Just "http://www.haskellforall.com/feeds/posts/default")
  , ("Getty Ritter", "https://blog.infinitenegativeutility.com", Just "https://blog.infinitenegativeutility.com/rss")
  , ("Gil Mizrahi", "https://gilmi.me", Just "https://gilmi.me/rss")
  , ("Giovanni Parra", "http://entulho.fiatjaf.alhur.es", Just "http://entulho.fiatjaf.alhur.es/feed.xml")
  , ("Google Summer of Code", "https://summerofcode.withgoogle.com", Just "http://feeds.feedburner.com/GoogleOpenSourceBlog")
  , ("GRAKN.AI", "https://blog.grakn.ai", Just "https://blog.grakn.ai/feed")
  , ("Harold Carr", "http://haroldcarr.com", Just "http://haroldcarr.com/atom.xml")
  , ("Haskell at Work", "https://haskell-at-work.com", Just "https://haskell-at-work.com/atom.xml")
  , ("Haskell for Mac", "http://blog.haskellformac.com", Just "http://blog.haskellformac.com/feed")
  , ("Haskell Tools", "https://haskelltools.blogspot.com", Just "https://haskelltools.blogspot.com/feeds/posts/default")
  , ("Haskell Weekly", "https://haskellweekly.news", Just "https://haskellweekly.news/haskell-weekly.atom")
  , ("Henri Verroken", "https://deliquus.com", Just "https://deliquus.com/atom.xml")
  , ("HolidayCheck", "http://techblog.holidaycheck.com", Just "http://techblog.holidaycheck.com/feed.xml")
  , ("Hypothesis", "https://hypothesis.works", Just "https://hypothesis.works/articles/feed/")
  , ("Igal Tabachnik", "https://hmemcpy.com", Just "https://hmemcpy.com/atom.xml")
  , ("IRIS Connect", "https://engineers.irisconnect.net", Just "https://engineers.irisconnect.net/rss.xml")
  , ("Isaac Elliott", "http://blog.ielliott.io", Just "http://blog.ielliott.io/feed.xml")
  , ("Isaac Shapira", "http://mutanatum.com", Just "http://mutanatum.com/atom.xml")
  , ("Ismail S", "https://blog.ismail-s.com", Just "https://blog.ismail-s.com/index.xml")
  , ("J Haigh", "https://debugsteven.github.io", Just "https://debugsteven.github.io/feed.xml")
  , ("Jacob Errington", "https://jerrington.me", Just "https://jerrington.me/atom.xml")
  , ("Jan Mas Rovira", "https://janmasrovira.gitlab.io/ascetic-slug/", Just "https://janmasrovira.gitlab.io/ascetic-slug/index.xml")
  , ("Jared Weakly", "https://jaredweakly.com", Just "https://jaredweakly.com/feed/")
  , ("Jaro Reinders", "https://noughtmare.gitlab.io", Just "https://noughtmare.gitlab.io/atom.xml")
  , ("Jaseem Abid", "https://jaseemabid.github.io", Just "https://jaseemabid.github.io/feed.xml")
  , ("Jason Shipman", "https://jship.github.io", Just "https://jship.github.io/atom.xml")
  , ("Jasper Van der Jeugt", "https://jaspervdj.be", Just "https://jaspervdj.be/rss.xml")
  , ("Javascript to Elm", "http://jstoelm.com", Just "https://jstoelm.libsyn.com/rss")
  , ("Jean-Louis Giordano", "https://jawaninja.com", Just "https://jawaninja.com/atom.xml")
  , ("Jens Petersen", "https://juhp.blogspot.com", Just "https://juhp.blogspot.com/feeds/posts/default")
  , ("Jeroen Keiren", "https://www.jeroenkeiren.nl", Just "https://www.jeroenkeiren.nl/feed/")
  , ("Jezen Thomas", "https://jezenthomas.com", Just "https://jezenthomas.com/feed.xml")
  , ("Joachim Breitner", "https://www.joachim-breitner.de", Just "https://www.joachim-breitner.de/blog_feed.rss")
  , ("Joe Nelson", "https://begriffs.com", Just "https://begriffs.com/atom.xml")
  , ("Joe Vargas", "http://jxv.io", Just "http://jxv.io/blog/rss.xml")
  , ("John A De Goes", "http://degoes.net", Just "http://degoes.net/feed.xml")
  , ("John Mendon\xe7\&a", "https://mendo.zone", Just "https://mendo.zone/feed.xml")
  , ("Jonathan Fischoff", "https://medium.com/@jonathangfischoff", Just "https://medium.com/feed/@jonathangfischoff")
  , ("Jonathan Lange", "https://jml.io", Just "https://jml.io/feeds/all.atom.xml")
  , ("Juan Pedro Villa Isaza", "https://jpvillaisaza.github.io", Just "https://jpvillaisaza.github.io/feed")
  , ("Julie Moronuki", "https://argumatronic.com", Just "https://argumatronic.com/rss.xml")
  , ("Justin Le", "https://blog.jle.im", Just "http://feeds.feedburner.com/incodeblog")
  , ("Jyri-Matti L\xe4hteenm\xe4ki", "https://blog.lahteenmaki.net", Just "https://blog.lahteenmaki.net/rss.xml")
  , ("Keera Studios", "http://keera.co.uk/blog/", Just "http://keera.co.uk/blog/feed/")
  , ("Kris Jenkins", "http://blog.jenkster.com", Just "https://feeds.feedburner.com/KrisJenkinsBlog")
  , ("Kwang Yul Seo", "https://kseo.github.io", Just "https://kseo.github.io/atom.xml")
  , ("Kyle Kingsbury", "https://aphyr.com", Just "https://aphyr.com/posts.atom")
  , ("Lambda the Ultimate", "http://lambda-the-ultimate.org", Just "http://lambda-the-ultimate.org/rss.xml")
  , ("Laurence Emms", "https://whatthefunctional.wordpress.com", Just "https://whatthefunctional.wordpress.com/feed/")
  , ("Li-yao Xia", "https://blog.poisson.chat", Just "https://blog.poisson.chat/rss.xml")
  , ("Libby Horacek", "https://medium.com/@horrorcheck", Just "https://medium.com/feed/@horrorcheck")
  , ("LiquidHaskell", "https://ucsd-progsys.github.io/liquidhaskell-blog/", Just "https://ucsd-progsys.github.io/liquidhaskell-blog/atom.xml")
  , ("Luis Pedro Coelho", "https://metarabbit.wordpress.com", Just "https://metarabbit.wordpress.com/feed/")
  , ("Luke Picciau", "https://itscode.red", Just "https://itscode.red/index.xml")
  , ("Marcelo Zabani", "https://mzabani.wordpress.com", Just "https://mzabani.wordpress.com/feed")
  , ("Marco Sampellegrini", "https://alpacaaa.net/blog/", Just "https://alpacaaa.net/blog/index.xml")
  , ("Mark Karpov", "https://markkarpov.com", Just "https://markkarpov.com/feed.atom")
  , ("Mark Seemann", "http://blog.ploeh.dk", Just "http://blog.ploeh.dk/atom.xml")
  , ("Mark Wotton", "https://www.shimweasel.com", Just "https://www.shimweasel.com/atom.xml")
  , ("Mateusz Kowalczyk", "http://fuuzetsu.co.uk/blog/", Just "http://fuuzetsu.co.uk/blog/atom.xml")
  , ("Matt Noonan", "http://storm-country.com", Just "http://storm-country.com/rss")
  , ("Matt Parsons", "https://www.parsonsmatt.org", Just "https://www.parsonsmatt.org/feed.xml")
  , ("Matthew Mongeau", "https://halogenandtoast.com", Just "https://halogenandtoast.com/rss/")
  , ("Matthew Pickering", "https://mpickering.github.io", Just "https://mpickering.github.io/atom.xml")
  , ("Michael Burge", "http://www.michaelburge.us", Just "http://www.michaelburge.us/feed.xml")
  , ("Michael Gattozzi", "https://mgattozzi.com", Just "https://mgattozzi.com/feed")
  , ("Michael Snoyman", "https://www.snoyman.com", Just "https://www.snoyman.com/feed")
  , ("Michael Xavier", "https://michaelxavier.net", Just "https://michaelxavier.net/rss.xml")
  , ("Microsoft Research Podcast", "https://www.microsoft.com/en-us/research/blog/category/podcast/", Just "https://www.microsoft.com/en-us/research/blog/category/podcast/feed/")
  , ("Mike Ledger", "https://quasimal.com", Just "https://quasimal.com/feed.xml")
  , ("Mikhail Glushenkov", "http://coldwa.st/e/", Just "http://feeds.feedburner.com/ChurningAndChurning")
  , ("Mistral Contrastin", "https://dodisturb.me", Just "https://dodisturb.me/atom.xml")
  , ("Monday Morning Haskell", "https://mmhaskell.com", Just "https://mmhaskell.com/blog?format=rss")
  , ("Moritz Kiefer", "https://purelyfunctional.org", Just "https://purelyfunctional.org/atom.xml")
  , ("Morphism", "https://www.morphism.tech", Just "https://www.morphism.tech/feed/")
  , ("Nam C.", "https://namc.in", Just "https://namc.in/feed.xml")
  , ("Nathan Maxson", "https://joyfulmantis.github.io", Just "https://joyfulmantis.github.io/atom.xml")
  , ("Neil Mitchell", "https://neilmitchell.blogspot.com", Just "https://neilmitchell.blogspot.com/feeds/posts/default")
  , ("Nicolas Mattia", "http://nmattia.com", Just "http://nmattia.com/atom.xml?type=blog")
  , ("Nikita Volkov", "https://nikita-volkov.github.io", Just "https://nikita-volkov.github.io/feed.xml")
  , ("Noon van der Silk", "https://silky.github.io", Just "https://silky.github.io/atom.xml")
  , ("Nuno Alexandre", "https://nunoalexandre.com", Just "https://nunoalexandre.com/feed.xml")
  , ("Oleg Grenrus", "http://oleg.fi", Just "http://oleg.fi/gists/atom.xml")
  , ("Oliver Charles", "https://ocharles.org.uk", Just "https://ocharles.org.uk/blog/posts.rss")
  , ("Osanai Kazuyoshi", "http://syocy.hatenablog.com", Just "http://syocy.hatenablog.com/feed")
  , ("Oskar Wickstr\xf6m", "https://wickstrom.tech", Just "https://wickstrom.tech/feed.xml")
  , ("Parnell Springmeyer", "https://ixmatus.net", Just "https://ixmatus.net/atom.xml")
  , ("Patrick Thompson", "http://blog.sumtypeofway.com", Just "http://blog.sumtypeofway.com/rss/")
  , ("Paul Bone", "https://paul.bone.id.au", Just "http://paul.bone.id.au/blog.xml")
  , ("Paul Chiusano", "https://pchiusano.github.io", Just "https://pchiusano.github.io/feed.xml")
  , ("Paul Johnson", "https://paulspontifications.blogspot.com", Just "https://paulspontifications.blogspot.com/feeds/posts/default")
  , ("Payton Turnage", "https://paytonturnage.com", Just "https://paytonturnage.com/feed.xml")
  , ("Phil Freeman", "https://medium.com/@paf31", Just "https://medium.com/feed/@paf31")
  , ("Philip Wadler", "https://wadler.blogspot.com", Just "https://wadler.blogspot.com/feeds/posts/default")
  , ("Philipp Maier", "https://blog.akii.de", Just "https://blog.akii.de/feed.atom")
  , ("Philipp Schuster", "https://haskellexists.blogspot.com", Just "https://haskellexists.blogspot.com/feeds/posts/default")
  , ("Piyush P. Kurur", "https://cse.iitk.ac.in/users/ppk/", Just "https://cse.iitk.ac.in/users/ppk/posts/feeds/atom.xml")
  , ("Programming Research Laboratory", "https://prl.ccs.neu.edu/blog/", Just "https://prl.ccs.neu.edu/blog/feeds/all.atom.xml")
  , ("Pusher", "https://making.pusher.com", Just "https://making.pusher.com/feed.xml")
  , ("Queensland Functional Programming Lab", "https://qfpl.io", Just "https://qfpl.io/atom.xml")
  , ("Quentin Duval", "https://deque.blog", Just "https://deque.blog/feed/")
  , ("Ramakrishnan Muthukrishnan", "https://rkrishnan.org", Just "https://rkrishnan.org/atom.xml")
  , ("Richard Cook", "https://blog.rcook.org", Just "https://blog.rcook.org/atom.xml")
  , ("Richard Eisenberg", "https://typesandkinds.wordpress.com", Just "https://typesandkinds.wordpress.com/feed/")
  , ("Richard Feldman", "https://dev.to/rtfeldman", Just "https://dev.to/feed/rtfeldman")
  , ("Roman Cheplyaka", "https://ro-che.info", Just "https://ro-che.info/articles/rss.xml")
  , ("Russell O'Connor", "http://r6.ca", Just "http://r6.ca/blog/feed.atom")
  , ("Ryan Scott", "https://ryanglscott.github.io", Just "https://ryanglscott.github.io/feed.xml")
  , ("Sam Tay", "https://samtay.github.io", Just "https://samtay.github.io/atom.xml")
  , ("Samuel G\xe9lineau", "https://gelisam.blogspot.com", Just "https://gelisam.blogspot.com/feeds/posts/default")
  , ("Sandy Maguire", "http://reasonablypolymorphic.com", Just "http://reasonablypolymorphic.com/atom.xml")
  , ("Sascha Wilde", "http://blogs.intevation.de/wilde/", Just "http://blogs.intevation.de/wilde/index.xml")
  , ("Saurabh Nanda", "https://medium.com/@saurabhnanda", Just "https://medium.com/feed/@saurabhnanda")
  , ("School of Haskell", "https://www.schoolofhaskell.com", Just "https://www.schoolofhaskell.com/recent-content/feed")
  , ("Scott Nonnenberg", "https://blog.scottnonnenberg.com", Just "https://blog.scottnonnenberg.com/atom.xml")
  , ("SeatGeek", "https://chairnerd.seatgeek.com", Just "https://chairnerd.seatgeek.com/atom.xml")
  , ("Sebastian Graf", "http://fixpt.de", Just "http://fixpt.de/atom.xml")
  , ("Sergei Trofimovich", "https://trofi.github.io", Just "https://trofi.github.io/feed/atom.xml")
  , ("Simon Marlow", "https://simonmar.github.io", Just "https://simonmar.github.io/atom.xml")
  , ("Simon Thompson", "https://profsjt.blogspot.com", Just "https://profsjt.blogspot.com/feeds/posts/default")
  , ("Small Improvements", "https://tech.small-improvements.com", Just "https://tech.small-improvements.com/feed/")
  , ("Snap Framework", "http://snapframework.com/blog", Just "http://snapframework.com/blog/feed.xml")
  , ("Spock", "https://www.spock.li/blog/", Just "https://www.spock.li/feed.xml")
  , ("Stack Builders", "https://www.stackbuilders.com", Just "https://www.stackbuilders.com/tutorials/atom.xml")
  , ("Stackage", "https://www.stackage.org", Just "https://www.stackage.org/blog/feed")
  , ("Stefano Dacchille", "https://futtetennismo.me", Just "https://futtetennismo.me/feed.xml")
  , ("Stephan Boyer", "https://www.stephanboyer.com", Just "https://www.stephanboyer.com/atom")
  , ("Stephen Diehl", "http://www.stephendiehl.com", Just "http://www.stephendiehl.com/feed.atom")
  , ("Steve Shogren", "http://deliberate-software.com", Just "http://deliberate-software.com/index.xml")
  , ("Steven Syrek", "https://medium.com/@sjsyrek", Just "https://medium.com/feed/@sjsyrek")
  , ("Summer of Haskell", "https://summer.haskell.org/news.html", Just "http://summer.haskell.org/news.xml")
  , ("Sylvain Henry", "http://www.sylvain-henry.info/home/index.html", Just "http://www.sylvain-henry.info/home/atom.xml")
  , ("Takt", "https://code.takt.com", Just "https://code.takt.com/feed")
  , ("Taylor Fausak", "http://taylor.fausak.me", Just "http://taylor.fausak.me/sitemap.atom")
  , ("Tebello M. Thejane", "https://medium.com/@zyxoas", Just "https://medium.com/feed/@zyxoas")
  , ("The GHC Blog", "https://ghc.haskell.org/trac/ghc/blog", Just "https://ghc.haskell.org/trac/ghc/blog?format=rss")
  , ("The Haskell Cast", "https://www.haskellcast.com", Just "https://www.haskellcast.com/feed.xml")
  , ("The Initial Commit", "https://theinitialcommit.com", Just "https://theinitialcommit.com/feed.xml")
  , ("The Joy of Haskell", "https://joyofhaskell.com/blog.html", Just "https://joyofhaskell.com/rss.xml")
  , ("The regex blog", "http://regex.uk/blog/", Just "http://regex.uk/blog/rss.xml")
  , ("Thoughtbot", "https://robots.thoughtbot.com", Just "https://feeds.feedburner.com/GiantRobotsSmashingIntoOtherGiantRobots")
  , ("Tim Cotten", "https://blog.cotten.io", Just "https://blog.cotten.io/feed")
  , ("Tim Humphries", "https://teh.id.au", Just "https://teh.id.au/posts/atom.xml")
  , ("Tobias Dammers", "https://programming.tobiasdammers.nl", Just "https://programming.tobiasdammers.nl/blog/rss")
  , ("Tom Prior", "http://www.prigrammer.com", Just "http://www.prigrammer.com/?feed=rss2")
  , ("Tomas Petricek", "http://tomasp.net", Just "http://tomasp.net/rss.xml")
  , ("Tommaso Piazza", "http://allocinit.io", Just "http://allocinit.io/feed.xml")
  , ("Tweag I/O", "https://www.tweag.io", Just "https://www.tweag.io/rss.xml")
  , ("Twilio", "https://www.twilio.com/blog/", Just "https://www.twilio.com/blog/feed")
  , ("Uber Engineering", "https://eng.uber.com", Just "https://eng.uber.com/feed/")
  , ("Vados", "https://vadosware.io", Just "https://vadosware.io/index.xml")
  , ("Vaibhav Sagar", "http://vaibhavsagar.com", Just "http://vaibhavsagar.com/atom.xml")
  , ("Vincent Hanquez", "http://tab.snarc.org", Just "http://tab.snarc.org/rss.xml")
  , ("Will Fancher", "https://elvishjerricco.github.io", Just "https://elvishjerricco.github.io/feed.xml")
  , ("Will Yager", "https://yager.io", Just "https://yager.io/feed/")
  , ("Windows Command Line Tools For Developers", "https://blogs.msdn.microsoft.com/commandline/", Just "https://blogs.msdn.microsoft.com/commandline/feed/")
  , ("Wit.ai", "https://wit.ai/blog", Just "https://wit.ai/blog/feed.xml")
  , ("Yesod", "https://www.yesodweb.com", Just "https://www.yesodweb.com/feed")
  , ("Zach Kessin", "https://get-finch.com", Just "http://get-finch.com/feed.xml")
  , ("Zw3rk Tech", "https://medium.com/@zw3rk", Just "https://medium.com/feed/@zw3rk")

  , ("Adam Wespiser", "https://wespiser.com", Nothing) -- https://github.com/adamwespiser/wespiser.com/issues/1
  , ("Divam", "https://dfordivam.github.io", Nothing) -- https://github.com/dfordivam/dfordivam.github.io/issues/1
  , ("Edsko de Vries", "http://edsko.net", Nothing) -- https://twitter.com/taylorfausak/status/973198287418417152
  , ("Elm", "http://elm-lang.org/blog", Nothing) -- https://github.com/elm-lang/elm-lang.org/issues/630
  , ("Functional Works", "https://functional.works-hub.com", Nothing) -- https://twitter.com/taylorfausak/status/971367969510092800
  , ("Gustav Behm", "https://rootmos.github.io", Nothing) -- email
  , ("Haskell Serbia", "https://haskell-serbia.com", Nothing) -- https://github.com/haskell-serbia/haskell-serbia/issues/27
  , ("Ibnu D. Aji", "https://ibnuda.gitlab.io", Nothing) -- https://gitlab.com/ibnuda/ibnuda.gitlab.io/issues/2
  , ("James Haver II", "http://www.mchaver.com", Nothing) -- https://github.com/mchaver/mchaver.com/issues/2
  , ("Jeremy Mikkola", "http://jeremymikkola.com", Nothing) -- https://twitter.com/taylorfausak/status/972870791808405504
  , ("Jonathan Merritt", "https://lancelet.github.io", Nothing) -- https://github.com/lancelet/blog-source/issues/1
  , ("JosephCieslik", "https://torchhound.github.io", Nothing) -- https://github.com/torchhound/torchhound.github.io/issues/2
  , ("Lorepub", "https://lorepub.com", Nothing) -- Slack
  , ("Peter Seibel", "http://www.gigamonkeys.com", Nothing) -- https://twitter.com/taylorfausak/status/973176558797185024
  , ("Philip Cunningham", "https://filib.io", Nothing) -- email
  , ("Philip Nilsson", "https://philipnilsson.github.io/Badness10k/", Nothing) -- https://github.com/philipnilsson/Badness10k/issues/4
  , ("Raphael Baron", "http://rbaron.net", Nothing) -- email
  , ("Renzo Carbonara", "https://ren.zone", Nothing) -- https://github.com/k0001/k0001.github.com/issues/1
  , ("Siddharth Bhat", "https://pixel-druid.com", Nothing) -- https://github.com/bollu/pixeldruid/issues/1
  , ("Vasantha Ganesh Kanniappan", "https://blog.hustlr.in", Nothing) -- https://gitlab.com/vasanthaganeshk/hustlr-in/issues/4
  , ("Well-Typed", "https://www.well-typed.com/", Nothing) -- https://twitter.com/taylorfausak/status/972087304067207168
  , ("Winter", "http://winterland.me", Nothing) -- https://github.com/winterland1989/winterland1989.github.io/issues/2
  , ("XT", "https://xtendo.org", Nothing) -- https://twitter.com/taylorfausak/status/973175749728505856
  ]
