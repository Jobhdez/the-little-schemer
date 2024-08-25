{-# LANGUAGE NoRebindableSyntax #-}
{-# OPTIONS_GHC -fno-warn-missing-import-lists #-}
{-# OPTIONS_GHC -w #-}
module PackageInfo_red (
    name,
    version,
    synopsis,
    copyright,
    homepage,
  ) where

import Data.Version (Version(..))
import Prelude

name :: String
name = "red"
version :: Version
version = Version [0,1,0,0] []

synopsis :: String
synopsis = "A Redis-based recommender system in Haskell"
copyright :: String
copyright = ""
homepage :: String
homepage = ""
