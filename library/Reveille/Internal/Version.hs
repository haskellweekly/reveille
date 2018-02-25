module Reveille.Internal.Version where

import qualified Data.Version as Version
import qualified Paths_reveille as This

version :: Version.Version
version = This.version

versionString :: String
versionString = Version.showVersion version
