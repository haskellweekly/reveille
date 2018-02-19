module Reveille.Unicode
  ( fromUtf8
  , toUtf8
  ) where

import qualified Data.ByteString as Bytes
import qualified Data.Text as Text
import qualified Data.Text.Encoding as Text
import qualified Data.Text.Encoding.Error as Text

fromUtf8 :: Bytes.ByteString -> Either Text.UnicodeException String
fromUtf8 bytes = fmap Text.unpack (Text.decodeUtf8' bytes)

toUtf8 :: String -> Bytes.ByteString
toUtf8 string = Text.encodeUtf8 (Text.pack string)
