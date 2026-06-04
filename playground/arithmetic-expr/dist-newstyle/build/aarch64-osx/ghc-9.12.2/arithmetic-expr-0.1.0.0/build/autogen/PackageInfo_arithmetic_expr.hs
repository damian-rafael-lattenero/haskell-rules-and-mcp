{-# LANGUAGE NoRebindableSyntax #-}
{-# OPTIONS_GHC -fno-warn-missing-import-lists #-}
{-# OPTIONS_GHC -w #-}
module PackageInfo_arithmetic_expr (
    name,
    version,
    synopsis,
    copyright,
    homepage,
  ) where

import Data.Version (Version(..))
import Prelude

name :: String
name = "arithmetic_expr"
version :: Version
version = Version [0,1,0,0] []

synopsis :: String
synopsis = "(describe here)"
copyright :: String
copyright = ""
homepage :: String
homepage = ""
