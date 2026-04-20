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
  , WarningCategory (..)
  , parseGhcErrors
  , categorizeWarning
  , bucketize
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
      , "category" .= categorizeWarning e
      ]

-- | Phase 11i: coarse-grained warning buckets so the agent can
-- prioritise the "what to fix first" decision without parsing the
-- message text per-call. Categories intentionally coarser than GHC's
-- individual -W flags: 5 groups cover >90% of real-world warnings.
data WarningCategory
  = WcUnused          -- ^ unused imports, bindings, matches, type vars
  | WcNonExhaustive   -- ^ incomplete pattern matches / missing fields
  | WcShadowing       -- ^ name-shadowing
  | WcMissingSig      -- ^ top-level function without a signature
  | WcDeferredError   -- ^ GHC-8xxxx deferred type errors / typed holes
  | WcOther           -- ^ did not match any bucket; leave the agent to
                      --   inspect the raw message
  deriving stock (Eq, Show, Ord)

instance ToJSON WarningCategory where
  toJSON WcUnused        = "unused"
  toJSON WcNonExhaustive = "non-exhaustive"
  toJSON WcShadowing     = "shadowing"
  toJSON WcMissingSig    = "missing-signature"
  toJSON WcDeferredError = "deferred-error"
  toJSON WcOther         = "other"

-- | Bucketize a single GhcError. Reads the message text + the GHC
-- code (when present). Pure; no reliance on external state.
categorizeWarning :: GhcError -> WarningCategory
categorizeWarning e =
  let msg = T.toLower (geMessage e)
      mentions = any (`T.isInfixOf` msg)
  in case geCode e of
       Just code | code `elem` deferredCodes -> WcDeferredError
       _
         | mentions
             [ "unused", "defined but not used"
             , "not used", "redundant" ]
           -> WcUnused
         | mentions
             [ "non-exhaustive", "pattern match(es) are non-exhaustive"
             , "incomplete record construction"
             , "missing constructor", "missing field" ]
           -> WcNonExhaustive
         | mentions
             [ "shadow", "shadowing" ]
           -> WcShadowing
         | mentions
             [ "top-level binding with no type signature"
             , "missing signature", "missing-signature" ]
           -> WcMissingSig
         | otherwise -> WcOther
  where
    -- GHC codes for deferred type errors / typed holes.
    deferredCodes =
      [ "GHC-88464"  -- variable not in scope (deferred)
      , "GHC-83865"  -- couldn't match expected type (deferred)
      , "GHC-66111"  -- unused-imports warning family
      ]

-- | Group a list of errors by category + count. Returned ordered by
-- bucket count descending — agents lean on the head for prioritised
-- triage. Shape is intentionally simple so any client can consume
-- it without knowing the GhcError envelope.
bucketize :: [GhcError] -> [(WarningCategory, Int)]
bucketize es =
  let cats = map categorizeWarning es
      unique = foldr addOne [] cats
      addOne c [] = [(c, 1)]
      addOne c ((k, n) : rest)
        | c == k    = (k, n + 1) : rest
        | otherwise = (k, n) : addOne c rest
  in reverse (sortByCount unique)

sortByCount :: [(WarningCategory, Int)] -> [(WarningCategory, Int)]
sortByCount = sortOn' snd
  where
    sortOn' f = foldr insert []
      where
        insert x []       = [x]
        insert x (y : ys)
          | f x <= f y    = x : y : ys
          | otherwise     = y : insert x ys

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

