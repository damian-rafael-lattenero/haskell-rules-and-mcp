-- | Parser for @hpc report@ output.
--
-- After @cabal test --enable-coverage@ runs, Cabal emits a summary
-- block like:
--
-- > 100% expressions used (12/12)
-- > 100% boolean coverage (0/0)
-- >      100% guards (0/0)
-- >      100% 'if' conditions (0/0)
-- >      100% qualifiers (0/0)
-- >  66% alternatives used (2/3)
-- >  75% local declarations used (3/4)
-- > 100% top-level declarations used (5/5)
--
-- Note that @hpc@ reports @100%@ for categories with zero applicable
-- program points (e.g. a project with no guards reports
-- @100% guards (0/0)@). Issue #89 — that quirk is mathematically
-- meaningless and inflates any naive average across categories. We
-- normalise it at the parse boundary: @total == 0@ rows surface with
-- @mPercent = Nothing@ and @mStatus = \"not_applicable\"@, mirroring
-- the @status: empty@ discriminator pattern that @ghc_check_project@
-- already uses for its zero-property gate. Consumers that want a
-- pure HPC dump still have @mCovered@ and @mTotal@; the
-- @\"not_applicable\"@ tag tells them to skip the row when computing
-- summary numbers like averages.
--
-- The result is ReDoS-safe — no regex, just a small line-based state
-- machine.
module HaskellFlows.Parser.Coverage
  ( CoverageReport (..)
  , Metric (..)
  , parseCoverage
  ) where

import Data.Char (isDigit)
import Data.Maybe (mapMaybe)
import Data.Text (Text)
import qualified Data.Text as T
import Text.Read (readMaybe)

-- | Aggregate of every metric line we recognised.
newtype CoverageReport = CoverageReport
  { crMetrics :: [Metric]
  }
  deriving stock (Eq, Show)

-- | One coverage dimension.
--
-- 'mCovered' / 'mTotal' come from the @(a/b)@ suffix on the same
-- line. 'mPercent' is 'Nothing' when @mTotal == 0@ (\"no applicable
-- program points\") and @'Just' p@ otherwise — never trust HPC's
-- leading-column percentage on a @0/0@ row, it always lies as
-- @100%@. 'mStatus' is the categorical discriminator:
--
--   * @\"covered\"@        when @covered == total > 0@
--   * @\"uncovered\"@      when @covered <  total > 0@
--   * @\"not_applicable\"@ when @total == 0@
--
-- See Issue #89 for the bug this shape fixes (the old @!Int@ percent
-- claimed @100%@ for @0/0@ rows and dragged the across-metrics
-- average upward).
--
-- 'mAlwaysTrue' / 'mAlwaysFalse' capture the HPC annotation emitted
-- for @\'if\'@ conditions that were reached but never exercised in
-- both directions, e.g. @\", 2 always True\"@. Both default to 0.
-- See Issue #177.
data Metric = Metric
  { mLabel       :: !Text
  , mPercent     :: !(Maybe Int)
  , mCovered     :: !Int
  , mTotal       :: !Int
  , mStatus      :: !Text
  , mAlwaysTrue  :: !Int
  , mAlwaysFalse :: !Int
  }
  deriving stock (Eq, Show)

-- | Parse the raw @hpc report@ output into a 'CoverageReport'.
--
-- Any lines that don't match the @NN% label (a/b)@ shape are ignored —
-- this keeps us resilient against cabal prefixing banners or footers
-- that change between versions.
parseCoverage :: Text -> CoverageReport
parseCoverage raw =
  CoverageReport { crMetrics = mapMaybe parseLine (T.lines raw) }

parseLine :: Text -> Maybe Metric
parseLine ln = do
  let stripped = T.strip ln
  (pct, rest1) <- takePercent stripped
  (covered, total, label, suffix) <- takeFractionLabelAndSuffix rest1
  let (at, af) = parseAnnotation suffix
  pure (mkMetric (T.strip label) pct covered total at af)

-- | Smart constructor — collapses the @total == 0@ row to the
-- @\"not_applicable\"@ shape regardless of what HPC's leading-column
-- percent claimed.
mkMetric :: Text -> Int -> Int -> Int -> Int -> Int -> Metric
mkMetric label pct covered total alwaysTrue alwaysFalse
  | total <= 0 = Metric
      { mLabel       = label
      , mPercent     = Nothing
      , mCovered     = covered
      , mTotal       = total
      , mStatus      = "not_applicable"
      , mAlwaysTrue  = alwaysTrue
      , mAlwaysFalse = alwaysFalse
      }
  | otherwise  = Metric
      { mLabel       = label
      , mPercent     = Just pct
      , mCovered     = covered
      , mTotal       = total
      , mStatus      = if covered >= total then "covered" else "uncovered"
      , mAlwaysTrue  = alwaysTrue
      , mAlwaysFalse = alwaysFalse
      }

-- | Consume a leading @NN%@ (with optional leading whitespace) and
-- return the number plus the remainder after the @%@ sign.
takePercent :: Text -> Maybe (Int, Text)
takePercent t =
  let (digits, afterDigits) = T.span isDigit t
  in case (T.null digits, T.uncons afterDigits) of
       (False, Just ('%', rest)) -> do
         n <- readMaybe (T.unpack digits)
         pure (n, T.stripStart rest)
       _ -> Nothing

-- | Given text like @expressions used (12\/12)@ return
-- @(covered, total, label, suffix)@ where @suffix@ is whatever HPC
-- appended after the closing @)@, e.g. @\", 2 always True\"@.
-- Requires both a parenthesised fraction and a non-empty label before it.
-- Issue #177: suffix is passed back so the caller can parse annotations.
takeFractionLabelAndSuffix :: Text -> Maybe (Int, Int, Text, Text)
takeFractionLabelAndSuffix t =
  case T.breakOn "(" t of
    (_, parenRest) | T.null parenRest -> Nothing
    (label, parenRest) ->
      let inner  = T.drop 1 parenRest               -- strip leading "("
          frac   = T.takeWhile (/= ')') inner
          after  = T.dropWhile (/= ')') inner
          suffix = if T.null after then "" else T.drop 1 after  -- strip ")"
      in case T.breakOn "/" frac of
           (_, afterSlash) | T.null afterSlash -> Nothing
           (leftTxt, rightTxt) -> do
             l <- readMaybe (T.unpack (T.strip leftTxt))
             r <- readMaybe (T.unpack (T.strip (T.drop 1 rightTxt)))
             pure (l, r, label, suffix)

-- | Parse @\", N always True\"@ and/or @\", N always False\"@ from
-- the HPC annotation suffix that follows the @(a\/b)@ fraction on
-- @\'if\'@ condition lines. Returns @(alwaysTrue, alwaysFalse)@.
-- Both default to 0 when the annotation is absent or unparseable.
-- Issue #177.
parseAnnotation :: Text -> (Int, Int)
parseAnnotation t = (extractCount "always True" t, extractCount "always False" t)

extractCount :: Text -> Text -> Int
extractCount marker haystack =
  case T.breakOn marker haystack of
    (_, rest) | T.null rest -> 0
    (prefix, _) ->
      -- The number immediately precedes the marker, e.g. ", 2 always True"
      -- Take the last whitespace-separated word of the prefix text.
      case reverse (T.words prefix) of
        (w:_) -> maybe 0 id (readMaybe (T.unpack w))
        []    -> 0
