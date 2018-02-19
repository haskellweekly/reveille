module Reveille.Unicode
  ( fromUtf8
  , toUtf8
  ) where

import qualified Data.ByteString as Bytes
import qualified Data.Text as Text
import qualified Data.Text.Encoding as Text

fromUtf8 :: Bytes.ByteString -> String
fromUtf8 bytes = Text.unpack (Text.decodeUtf8 bytes)

toUtf8 :: String -> Bytes.ByteString
toUtf8 string = Text.encodeUtf8 (Text.pack string)
