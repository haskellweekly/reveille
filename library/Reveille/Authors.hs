module Reveille.Authors
  ( authors
  ) where

import Data.Function ((&))
import Reveille.Author (Author, toAuthor)

import qualified Data.Set as Set

authors :: Set.Set Author
authors = Set.empty
  & addAuthor "Alex Beal" "http://www.usrsb.in" (Just "http://www.usrsb.in/rss.xml")
  & addAuthor "Alexis King" "https://lexi-lambda.github.io" (Just "https://lexi-lambda.github.io/feeds/all.atom.xml")
  & addAuthor "Andre Van Der Merwe" "http://www.andrevdm.com/" (Just "http://www.andrevdm.com/atom.xml")
  & addAuthor "Attila Domokos" "http://www.adomokos.com" (Just "http://www.adomokos.com/feeds/posts/default")
  & addAuthor "Ben Gamari" "https://tomsmalley.github.io" Nothing
  & addAuthor "Brent Yorgey" "https://byorgey.wordpress.com" (Just "https://byorgey.wordpress.com/feed/")
  & addAuthor "Concert" "https://medium.com/@concertdaw" (Just "https://medium.com/feed/@concertdaw")
  & addAuthor "Daniel Bolivar" "https://www.dabolivar.com" (Just "https://www.dabolivar.com/index.xml")
  & addAuthor "Daniel Patterson" "https://dbp.io" (Just "https://dbp.io/rss.xml")
  & addAuthor "Eli Bendersky" "https://eli.thegreenplace.net" (Just "https://eli.thegreenplace.net/feeds/all.atom.xml")
  & addAuthor "FP Complete" "https://www.fpcomplete.com" (Just "https://www.fpcomplete.com/blog/atom.xml")
  & addAuthor "Gabriel Gonzalez" "http://www.haskellforall.com" (Just "http://www.haskellforall.com/feeds/posts/default")
  & addAuthor "Google Summer of Code" "https://summerofcode.withgoogle.com" (Just "http://feeds.feedburner.com/GoogleOpenSourceBlog")
  & addAuthor "Haskell at Work" "https://haskell-at-work.com" (Just "https://www.youtube.com/feeds/videos.xml?channel_id=UCUgxpaK7ySR-z6AXA5-uDuw")
  & addAuthor "Haskell Weekly" "https://haskellweekly.news" (Just "https://haskellweekly.news/haskell-weekly.atom")
  & addAuthor "Humble Bundle" "https://www.humblebundle.com" (Just "http://blog.humblebundle.com/rss")
  & addAuthor "Ibnu D. Aji" "https://ibnuda.gitlab.io" Nothing -- https://gitlab.com/ibnuda/ibnuda.gitlab.io/issues/2
  & addAuthor "Joachim Breitner" "https://www.joachim-breitner.de" (Just "https://www.joachim-breitner.de/blog_feed.rss")
  & addAuthor "Julie Moronuki" "https://argumatronic.com" (Just "https://argumatronic.com/rss.xml")
  & addAuthor "Luke Picciau" "https://itscode.red" (Just "https://itscode.red/index.xml")
  & addAuthor "Mark Karpov" "https://markkarpov.com" (Just "https://markkarpov.com/feed.atom")
  & addAuthor "Matt Noonan" "http://storm-country.com" (Just "http://storm-country.com/rss")
  & addAuthor "Michael Snoyman" "https://www.snoyman.com" (Just "https://www.snoyman.com/feed")
  & addAuthor "Mistral Contrastin" "https://dodisturb.me" (Just "https://dodisturb.me/atom.xml")
  & addAuthor "Monday Morning Haskell" "https://mmhaskell.com" (Just "https://mmhaskell.com/blog?format=rss")
  & addAuthor "Nuno Alexandre" "https://nunoalexandre.com" (Just "https://nunoalexandre.com/feed.xml")
  & addAuthor "Oskar Wickstr\xf6m" "https://wickstrom.tech" (Just "https://wickstrom.tech/feed.xml")
  & addAuthor "Patrick Thompson" "http://blog.sumtypeofway.com" (Just "http://blog.sumtypeofway.com/rss/")
  & addAuthor "Ryan Scott" "https://ryanglscott.github.io" (Just "https://ryanglscott.github.io/feed.xml")
  & addAuthor "Sandy Maguire" "http://reasonablypolymorphic.com" (Just "http://reasonablypolymorphic.com/atom.xml")
  & addAuthor "Siddharth Bhat" "https://pixel-druid.com" Nothing -- https://github.com/bollu/pixeldruid/issues/1
  & addAuthor "Stackage" "https://www.stackage.org" (Just "https://www.stackage.org/blog/feed")
  & addAuthor "Taylor Fausak" "http://taylor.fausak.me" (Just "http://taylor.fausak.me/sitemap.atom")
  & addAuthor "Tom Smalley" "https://tomsmalley.github.io" Nothing
  & addAuthor "Tweag I/O" "https://www.tweag.io" (Just "https://www.tweag.io/rss.xml")
  & addAuthor "Vaibhav Sagar" "http://vaibhavsagar.com" (Just "http://vaibhavsagar.com/atom.xml")

addAuthor :: String -> String -> Maybe String -> Set.Set Author -> Set.Set Author
addAuthor name url feed set = case toAuthor name url feed of
  Left problem -> error (show problem)
  Right author -> Set.insert author set
