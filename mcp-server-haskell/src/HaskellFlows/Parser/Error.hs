-- | Parse GHC diagnostics from GHCi's raw output.
--
-- Phase 1 uses a single regex on @regex-tdfa@ (linear-time, ReDoS-free)
-- that mirrors the primary header pattern in the TS parser
-- (@mcp-server/src/parsers/error-parser.ts:35@). The multi-line body and
-- its "Expected/Actual" / "Suggested fix" sub-shape are kept as the whole
-- raw body string for now — later phases will split that out and,
-- eventually, replace the regex entirely with @ghc-lib-parser@ walking the
-- real AST.
--
-- Why not stay with regex forever: the TS parser has about a half dozen
-- known false-negatives around Unicode quotes, multi-line type mismatch
-- reports, and forall-quantified signatures. AST-based parsing removes
-- that entire class of fragility.
module HaskellFlows.Parser.Error
  ( GhcError (..)
  , Severity (..)
  , parseGhcErrors
  ) where

import Data.Aeson (ToJSON (..), object, (.=))
import Data.Maybe (isJust)
import Data.Text (Text)
import qualified Data.Text as T
import Text.Read (readMaybe)
import Text.Regex.TDFA ((=~))

data Severity = SevError | SevWarning
  deriving stock (Eq, Show)

instance ToJSON Severity where
  toJSON SevError   = "error"
  toJSON SevWarning = "warning"

data GhcError = GhcError
  { geFile     :: !Text
  , geLine     :: !Int
  , geColumn   :: !Int
  , geSeverity :: !Severity
  , geCode     :: !(Maybe Text)
  , geMessage  :: !Text
  }
  deriving stock (Eq, Show)

instance ToJSON GhcError where
  toJSON e =
    object
      [ "file"     .= geFile e
      , "line"     .= geLine e
      , "column"   .= geColumn e
      , "severity" .= geSeverity e
      , "code"     .= geCode e
      , "message"  .= geMessage e
      ]

-- | Regex for the header line of a GHC diagnostic.
--
-- Captures (in order):
--
-- 1. file path
-- 2. line
-- 3. column
-- 4. severity ("error" | "warning")
-- 5. optional GHC-XXXXX code
--
-- Deliberately simple — we trade a little coverage for linear-time
-- guarantees. The TS regex tries to also capture range ends; we do not
-- need that yet for @ghci_load@.
headerRegex :: String
headerRegex =
  "^(.+):([0-9]+):([0-9]+): (error|warning)(:?)[[:space:]]*(\\[GHC-([0-9]+)\\])?"

-- | Parse all diagnostic headers from raw GHCi output.
--
-- Body text that follows a header (the multi-line explanation GHC emits
-- after the header) is attached to the preceding header verbatim, until
-- the next header or blank line.
parseGhcErrors :: Text -> [GhcError]
parseGhcErrors raw =
  let ls = T.lines raw
  in collect [] ls
  where
    collect acc [] = reverse acc
    collect acc (l : rest)
      | Just e <- parseHeader l =
          let (body, remaining) = break isHeaderOrBlank rest
              fullMsg = T.strip (T.unlines (l : body))
          in collect (e { geMessage = fullMsg } : acc) remaining
      | otherwise = collect acc rest

    isHeaderOrBlank l
      | T.null (T.strip l) = True
      | otherwise          = isJust (parseHeader l)

    parseHeader :: Text -> Maybe GhcError
    parseHeader line =
      case (T.unpack line =~ headerRegex) :: (String, String, String, [String]) of
        (_, _, _, [file, ln, col, sev, _, _, codeGroup]) -> do
          ln'  <- readMaybe ln
          col' <- readMaybe col
          sev' <- case sev of
                    "error"   -> Just SevError
                    "warning" -> Just SevWarning
                    _         -> Nothing
          let code = case codeGroup of
                       "" -> Nothing
                       n  -> Just (T.pack ("GHC-" <> n))
          pure
            GhcError
              { geFile     = T.pack file
              , geLine     = ln'
              , geColumn   = col'
              , geSeverity = sev'
              , geCode     = code
              , geMessage  = line
              }
        _ -> Nothing

