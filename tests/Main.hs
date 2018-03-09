import Reveille
import Test.Hspec

import qualified Data.ByteString as Bytes
import qualified Data.Either as Either
import qualified Data.Set as Set
import qualified Data.Text as Text
import qualified Data.Time as Time
import qualified Text.Atom.Feed as Atom
import qualified Text.Feed.Types as Feed

main :: IO ()
main = hspec . parallel . describe "Reveille" $ do

  describe "Aggregator" $ do

    it "needs tests" $ pending

  describe "Author" $ do

    describe "toAuthor" $ do

      it "fails with an empty name" $ do
        toAuthor "" "" Nothing
          `shouldBe` Left (AuthorErrorBadName NameErrorEmpty)

      it "fails with an empty URL" $ do
        toAuthor "Name" "" Nothing
          `shouldBe` Left (AuthorErrorBadUrl UrlErrorEmpty)

      it "fails with an invalid URL" $ do
        toAuthor "Name" "not a url" Nothing
          `shouldBe` Left (AuthorErrorBadUrl (UrlErrorInvalid "not a url"))

      it "succeeds without a feed" $ do
        toAuthor "Name" "http://example.com" Nothing
          `shouldSatisfy` Either.isRight

      it "fails with an empty feed" $ do
        toAuthor "Name" "http://example.com" (Just "")
          `shouldBe` Left (AuthorErrorBadFeed UrlErrorEmpty)

      it "fails with an invalid feed" $ do
        toAuthor "Name" "http://example.com" (Just "not a url")
          `shouldBe` Left (AuthorErrorBadFeed (UrlErrorInvalid "not a url"))

      it "succeeds with a feed" $ do
        toAuthor "Name" "http://example.com" (Just "http://example.com")
          `shouldSatisfy` Either.isRight

  describe "Database" $ do

    let
      name = Either.fromRight undefined (toName "Name")
      url = Either.fromRight undefined (toUrl "http://example.com")
      time = Time.UTCTime (Time.fromGregorian 2001 2 3) 0
      author = Author name url Nothing
      item = Item name url time
      entry = Entry author item

    it "is initially empty" $ do
      getDatabaseAuthors initialDatabase `shouldBe` Set.empty
      getDatabaseEntries initialDatabase `shouldBe` Set.empty

    it "can add an author" $ do
      let db = addDatabaseItems author Set.empty initialDatabase
      getDatabaseAuthors db `shouldBe` Set.singleton author
      getDatabaseEntries db `shouldBe` Set.empty

    it "can add an item" $ do
      let db = addDatabaseItems author (Set.singleton item) initialDatabase
      getDatabaseAuthors db `shouldBe` Set.singleton author
      getDatabaseEntries db `shouldBe` Set.singleton entry

  describe "Item" $ do

    describe "toItem" $ do

      feedUrl <- case toUrl "http://example.com" of
        Left urlError -> fail (show urlError)
        Right url -> pure url
      let
        entry = Atom.Entry
          { Atom.entryId = Text.empty
          , Atom.entryTitle = Atom.TextString Text.empty
          , Atom.entryUpdated = Text.empty
          , Atom.entryAuthors = []
          , Atom.entryCategories = []
          , Atom.entryContent = Nothing
          , Atom.entryContributor = []
          , Atom.entryLinks = []
          , Atom.entryPublished = Nothing
          , Atom.entryRights = Nothing
          , Atom.entrySource = Nothing
          , Atom.entrySummary = Nothing
          , Atom.entryInReplyTo = Nothing
          , Atom.entryInReplyTotal = Nothing
          , Atom.entryAttrs = []
          , Atom.entryOther = []
          }
        title = Atom.TextString (Text.pack "Example")
        link = Atom.Link
          { Atom.linkHref = Text.empty
          , Atom.linkRel = Nothing
          , Atom.linkType = Nothing
          , Atom.linkHrefLang = Nothing
          , Atom.linkTitle = Nothing
          , Atom.linkLength = Nothing
          , Atom.linkAttrs = []
          , Atom.linkOther = []
          }
        url = Text.pack "http://example.com"
        time = Text.pack "Wed, 21 Feb 18 08:59:52 EST"

      it "fails with an empty name" $ do
        toItem feedUrl (Feed.AtomItem entry)
          `shouldBe` Left (ItemErrorBadName NameErrorEmpty)

      it "fails with an invalid time" $ do
        toItem
            feedUrl
            (Feed.AtomItem entry
              { Atom.entryTitle = title
              , Atom.entryLinks = [link { Atom.linkHref = url }]
              }
            )
          `shouldBe` Left (ItemErrorInvalidTime (Text.empty))

      it "succeeds" $ do
        toItem
            feedUrl
            (Feed.AtomItem entry
              { Atom.entryTitle = title
              , Atom.entryLinks = [link { Atom.linkHref = url }]
              , Atom.entryUpdated = time
              }
            )
          `shouldSatisfy` Either.isRight

  describe "Main" $ do

    describe "initialAuthors" $ do

      it "has no errors" $ do
        initialAuthors `shouldSatisfy` all Either.isRight

  describe "Name" $ do

    describe "fromName" $ do

      it "returns the original name" $ do
        fmap fromName (toName "Haskell") `shouldBe` Right "Haskell"

    describe "toName" $ do

      it "fails with an empty name" $ do
        toName "" `shouldBe` Left NameErrorEmpty

      it "succeeds" $ do
        toName "Haskell" `shouldSatisfy` Either.isRight

  describe "Server" $ do

    it "needs tests" $ pending

  describe "Unicode" $ do

    describe "fromUtf8" $ do

      it "decodes a one byte character" $ do
        fromUtf8 (Bytes.pack [0x24]) `shouldBe` Right "\x24"

      it "decodes a two byte character" $ do
        fromUtf8 (Bytes.pack [0xc2, 0xa2]) `shouldBe` Right "\xa2"

      it "decodes a three byte character" $ do
        fromUtf8 (Bytes.pack [0xe2, 0x82, 0xac]) `shouldBe` Right "\x20ac"

      it "decodes a four byte character" $ do
        fromUtf8 (Bytes.pack [0xf0, 0x90, 0x8d, 0x88])
          `shouldBe` Right "\x10348"

      it "fails with invalid Unicode" $ do
        fromUtf8 (Bytes.pack [0xc0]) `shouldSatisfy` Either.isLeft

    describe "toUtf8" $ do

      it "encodes a one byte character" $ do
        toUtf8 "\x24" `shouldBe` Bytes.pack [0x24]

      it "encodes a two byte character" $ do
        toUtf8 "\xa2" `shouldBe` Bytes.pack [0xc2, 0xa2]

      it "encodes a three byte character" $ do
        toUtf8 "\x20ac" `shouldBe` Bytes.pack [0xe2, 0x82, 0xac]

      it "encodes a four byte character" $ do
        toUtf8 "\x10348" `shouldBe` Bytes.pack [0xf0, 0x90, 0x8d, 0x88]

  describe "Url" $ do

    describe "fromUrl" $ do

      it "returns the original URL" $ do
        fmap fromUrl (toUrl "http://example.com")
          `shouldBe` Right "http://example.com"

    describe "toUrl" $ do

      it "fails with an empty URL" $ do
        toUrl "" `shouldBe` Left UrlErrorEmpty

      it "fails with an invalid URL" $ do
        toUrl "not a url" `shouldBe` Left (UrlErrorInvalid "not a url")

      it "succeeds" $ do
        toUrl "http://example.com" `shouldSatisfy` Either.isRight
