-- | Issue #88 — permissive parsers for primitive tool arguments.
--
-- Some MCP host clients stringify primitive tool-call arguments
-- before forwarding them to the server: a number becomes
-- @\"42\"@, a boolean becomes @\"true\"@. The default Aeson
-- 'FromJSON' instance for 'Int' / 'Bool' rejects those wires
-- with an opaque @\"expected Number, but encountered String\"@,
-- making whole tools (notably @ghc_refactor@'s line-range params
-- and @ghc_fix_warning@'s line) effectively unusable from those
-- clients.
--
-- The remedy that the array-param fix already settled on (commit
-- @0475ac2@) is to widen what the server *accepts* without changing
-- what it *advertises*: the JSON Schema in @tdInputSchema@ still
-- says @\"type\": \"integer\"@, but the parser also accepts a
-- numeric string. Well-behaved clients keep working byte-for-byte
-- unchanged; broken clients no longer fail at the boundary.
--
-- This module exposes the two corresponding newtype shells so each
-- affected tool can opt in with a single @fmap unIntField@ or
-- @fmap unBoolField@ at its parser site.
module HaskellFlows.Mcp.PermissiveJSON
  ( IntField (..)
  , BoolField (..)
  ) where

import Data.Aeson (FromJSON (..), Value (..))
import Data.Aeson.Types (typeMismatch)
import Data.Char (toLower)
import qualified Data.Scientific as Sci
import qualified Data.Text as T
import qualified Data.Text.Read as TR

-- | An 'Int' that may be wired as either a JSON number or a numeric
-- string. The 'String' branch tolerates a leading @+@/@-@ sign
-- ('TR.signed') and trailing whitespace ('T.strip') — anything
-- after the integer (e.g. @\"42 lines\"@) is rejected so a typo
-- doesn't silently truncate.
newtype IntField = IntField { unIntField :: Int }
  deriving (Eq, Show)

instance FromJSON IntField where
  parseJSON v = case v of
    Number n
      | Just i <- Sci.toBoundedInteger n -> pure (IntField i)
      | otherwise                        ->
          typeMismatch "Int (number out of Int range or fractional)" v
    String s ->
      let stripped = T.strip s
      in case TR.signed TR.decimal stripped of
           Right (i, rest)
             | T.null rest -> pure (IntField i)
             | otherwise   -> typeMismatch
                                "numeric String (trailing non-digits)" v
           Left  _         -> typeMismatch "numeric String" v
    _ -> typeMismatch "Int or numeric String" v

-- | A 'Bool' that may be wired as either a JSON boolean or one of
-- the conventional string forms: @\"true\" \/ \"false\"\/ \"1\" \/
-- \"0\"@ (case-insensitive, surrounding whitespace tolerated).
-- Anything else is rejected — we do NOT treat the empty string as
-- @False@ or non-empty as @True@: that's the JavaScript truthiness
-- foot-gun, not a tool-args policy.
newtype BoolField = BoolField { unBoolField :: Bool }
  deriving (Eq, Show)

instance FromJSON BoolField where
  parseJSON (Bool b)   = pure (BoolField b)
  parseJSON (String s) =
    case map toLower (T.unpack (T.strip s)) of
      "true"  -> pure (BoolField True)
      "false" -> pure (BoolField False)
      "1"     -> pure (BoolField True)
      "0"     -> pure (BoolField False)
      _       -> fail
        ("unrecognised boolean string: " <> show s
          <> " (expected one of: true / false / 1 / 0)")
  parseJSON v          = typeMismatch "Bool or boolean String" v
