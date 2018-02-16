module Reveille.Authors
  ( authors
  ) where

import Reveille.Author (Author, toAuthor)

import qualified Data.Either as Either
import qualified Data.Set as Set

authors :: Set.Set Author
authors = Set.fromList (Either.rights
  [ toAuthor
    "Alex Beal"
    "http://www.usrsb.in"
    (Just "http://www.usrsb.in/rss.xml")
  , toAuthor
    "Alexis King"
    "https://lexi-lambda.github.io"
    (Just "https://lexi-lambda.github.io/feeds/all.atom.xml")
  , toAuthor
    "Andre Van Der Merwe"
    "http://www.andrevdm.com/"
    (Just "http://www.andrevdm.com/atom.xml")
  , toAuthor
    "Attila Domokos"
    "http://www.adomokos.com"
    (Just "http://www.adomokos.com/feeds/posts/default")
  , toAuthor
    "Ben Gamari"
    "https://tomsmalley.github.io"
    Nothing
  , toAuthor
    "Concert"
    "https://medium.com/@concertdaw"
    (Just "https://medium.com/feed/@concertdaw")
  , toAuthor
    "Daniel Bolivar"
    "https://www.dabolivar.com"
    (Just "https://www.dabolivar.com/index.xml")
  , toAuthor
    "FP Complete"
    "https://www.fpcomplete.com"
    (Just "https://www.fpcomplete.com/blog/atom.xml")
  , toAuthor
    "Gabriel Gonzalez"
    "http://www.haskellforall.com"
    (Just "http://www.haskellforall.com/feeds/posts/default")
  , toAuthor
    "Google Summer of Code"
    "https://summerofcode.withgoogle.com"
    (Just "http://feeds.feedburner.com/GoogleOpenSourceBlog")
  , toAuthor
    "Haskell at Work"
    "https://haskell-at-work.com"
    (Just "https://www.youtube.com/feeds/videos.xml?channel_id=UCUgxpaK7ySR-z6AXA5-uDuw")
  , toAuthor
    "Haskell Weekly"
    "https://haskellweekly.news"
    (Just "https://haskellweekly.news/haskell-weekly.atom")
  , toAuthor
    "Humble Bundle"
    "https://www.humblebundle.com"
    (Just "http://blog.humblebundle.com/rss")
  , toAuthor
    "Ibnu D. Aji"
    "https://ibnuda.gitlab.io"
    Nothing -- https://gitlab.com/ibnuda/ibnuda.gitlab.io/issues/2
  , toAuthor
    "Joachim Breitner"
    "https://www.joachim-breitner.de"
    (Just "https://www.joachim-breitner.de/blog_feed.rss")
  , toAuthor
    "Mark Karpov"
    "https://markkarpov.com"
    (Just "https://markkarpov.com/feed.atom")
  , toAuthor
    "Matt Noonan"
    "http://storm-country.com"
    (Just "http://storm-country.com/rss")
  , toAuthor
    "Michael Snoyman"
    "https://www.snoyman.com"
    (Just "https://www.snoyman.com/feed")
  , toAuthor
    "Mistral Contrastin"
    "https://dodisturb.me"
    (Just "https://dodisturb.me/atom.xml")
  , toAuthor
    "Monday Morning Haskell"
    "https://mmhaskell.com"
    (Just "https://mmhaskell.com/blog?format=rss")
  , toAuthor
    "Nuno Alexandre"
    "https://nunoalexandre.com"
    (Just "https://nunoalexandre.com/feed.xml")
  , toAuthor
    "Oskar Wickstr\xf6m"
    "https://wickstrom.tech"
    (Just "https://wickstrom.tech/feed.xml")
  , toAuthor
    "Ryan Scott"
    "https://ryanglscott.github.io"
    (Just "https://ryanglscott.github.io/feed.xml")
  , toAuthor
    "Sandy Maguire"
    "http://reasonablypolymorphic.com"
    (Just "http://reasonablypolymorphic.com/atom.xml")
  , toAuthor
    "Siddharth Bhat"
    "https://pixel-druid.com"
    Nothing -- https://github.com/bollu/pixeldruid/issues/1
  , toAuthor
    "Stackage"
    "https://www.stackage.org"
    (Just "https://www.stackage.org/blog/feed")
  , toAuthor
    "Taylor Fausak"
    "http://taylor.fausak.me"
    (Just "http://taylor.fausak.me/sitemap.atom")
  , toAuthor
    "Tom Smalley"
    "https://tomsmalley.github.io"
    Nothing
  , toAuthor
    "Tweag I/O"
    "https://www.tweag.io"
    (Just "https://www.tweag.io/rss.xml")
  , toAuthor
    "Vaibhav Sagar"
    "http://vaibhavsagar.com"
    (Just "http://vaibhavsagar.com/atom.xml")
  ])
