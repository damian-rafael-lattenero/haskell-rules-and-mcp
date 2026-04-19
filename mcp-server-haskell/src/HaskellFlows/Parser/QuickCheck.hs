-- | Parser for QuickCheck's stdout.
--
-- Mirrors @mcp-server/src/parsers/quickcheck-parser.ts@. Maps the four
-- observable QuickCheck outcomes to a single 'QuickCheckResult' sum:
--
-- * @+++ OK, passed N tests.@             → 'QcPassed'
-- * @*** Failed!@ Exception: …             → 'QcException'
-- * @*** Failed! Falsifiable (after …): …@ → 'QcFailed'
-- * @*** Gave up! Passed only …@           → 'QcGaveUp'
--
-- Everything else falls through to 'QcUnparsed' with the raw output, so
-- the tool layer can still hand the text to the agent rather than
-- silently swallowing an unfamiliar shape.
module HaskellFlows.Parser.QuickCheck
  ( QuickCheckResult (..)
  , parseQuickCheckOutput
  ) where

import Data.Text (Text)
import qualified Data.Text as T
import Text.Read (readMaybe)

-- | A parsed QuickCheck run outcome. The property string is propagated
-- from the caller since GHCi's output doesn't echo it reliably.
--
-- Positional (not record) to sidestep GHC's @-Wpartial-fields@ on sum
-- types. Consumers pattern-match on the constructor — that's both safer
-- and how the tool layer already uses it.
data QuickCheckResult
  = QcPassed    !Text !Int                  -- ^ property, #passed
  | QcFailed    !Text !Int !Int !Text       -- ^ property, #passed, #shrinks, counterexample
  | QcException !Text !Text                 -- ^ property, exception msg
  | QcGaveUp    !Text !Int !Int             -- ^ property, #passed, #discarded
  | QcUnparsed  !Text !Text                 -- ^ property, raw output
  deriving stock (Eq, Show)

-- | Parse QuickCheck output. 'property' is the textual property the
-- caller asked to check — we thread it through the result so every
-- branch carries the identity of what was tested.
parseQuickCheckOutput :: Text -> Text -> QuickCheckResult
parseQuickCheckOutput property raw
  | Just n <- matchPassed raw =
      QcPassed property n
  | Just (exn, _) <- matchException raw =
      QcException property exn
  | Just (n, shr, cex) <- matchFailed raw =
      QcFailed property (max 0 (n - 1)) shr cex
  | Just (n, disc) <- matchGaveUp raw =
      QcGaveUp property n disc
  | otherwise =
      QcUnparsed property (T.strip raw)

--------------------------------------------------------------------------------
-- line-by-line detectors (regex-free, keeps us linear-time and ReDoS-safe)
--------------------------------------------------------------------------------

matchPassed :: Text -> Maybe Int
matchPassed = firstJust tryLine . T.lines
  where
    tryLine ln =
      let stripped = T.strip ln
      in if "+++ OK, passed " `T.isPrefixOf` stripped
           then readNumberAfter "passed " stripped
           else Nothing

-- | Extract the @Exception: '…' (after N tests…)@ payload. Returns the
-- exception text and the count for symmetry with 'matchFailed'.
matchException :: Text -> Maybe (Text, Int)
matchException = firstJust tryBlock . T.lines
  where
    tryBlock ln
      | "*** Failed!" `T.isInfixOf` ln && "Exception" `T.isInfixOf` ln =
          let afterExn = T.drop 1 (T.dropWhile (/= ':') (T.dropWhile (/= 'E') ln))
              -- "Exception: 'foo' (after 1 test)" → strip punctuation noise
              exnTxt   = stripExceptionDecorations (T.takeWhile (/= '(') afterExn)
          in Just (T.strip exnTxt, 0)
      | otherwise = Nothing

-- | Detect @*** Failed! … (after N tests and M shrinks):@ and capture the
-- counterexample that follows on subsequent lines up to a blank line.
matchFailed :: Text -> Maybe (Int, Int, Text)
matchFailed raw =
  let ls = T.lines raw
  in go ls
  where
    go []       = Nothing
    go (l:rest)
      | "*** Failed!" `T.isInfixOf` l && "(after " `T.isInfixOf` l =
          let (nTests, shrinks) = extractCounts l
              cex               = T.strip (T.unlines (takeWhile (not . T.null . T.strip) rest))
          in Just (nTests, shrinks, cex)
      | otherwise = go rest

    extractCounts ln =
      let afterAfter = T.drop (T.length "(after ") (snd (T.breakOn "(after " ln))
          n          = fromMaybe 0 (readNumberAtStart afterAfter)
          shr        = case T.breakOn "and " afterAfter of
            (_, rest) | not (T.null rest) ->
              fromMaybe 0 (readNumberAtStart (T.drop (T.length "and ") rest))
            _ -> 0
      in (n, shr)

matchGaveUp :: Text -> Maybe (Int, Int)
matchGaveUp = firstJust tryLine . T.lines
  where
    tryLine ln =
      let stripped = T.strip ln
      in if "*** Gave up!" `T.isPrefixOf` stripped
           then
             let passed    = fromMaybe 0 (readNumberAfter "Passed only " stripped)
                 discarded = fromMaybe 0 (readNumberAfter "; "           stripped)
             in Just (passed, discarded)
           else Nothing

--------------------------------------------------------------------------------
-- small parsing helpers
--------------------------------------------------------------------------------

firstJust :: (a -> Maybe b) -> [a] -> Maybe b
firstJust _ []     = Nothing
firstJust f (x:xs) = case f x of
  Just y  -> Just y
  Nothing -> firstJust f xs

-- | Find @needle@ in @haystack@ and read a decimal integer starting at
-- the first digit that follows.
readNumberAfter :: Text -> Text -> Maybe Int
readNumberAfter needle haystack =
  case T.breakOn needle haystack of
    (_, rest) | T.null rest -> Nothing
    (_, rest) -> readNumberAtStart (T.drop (T.length needle) rest)

readNumberAtStart :: Text -> Maybe Int
readNumberAtStart t =
  let digits = T.takeWhile (`elem` ("0123456789" :: String)) (T.dropWhile (== ' ') t)
  in if T.null digits then Nothing else readMaybe (T.unpack digits)

fromMaybe :: a -> Maybe a -> a
fromMaybe d = maybe d id

-- | Trim the common decorations GHC/QuickCheck wrap around exception
-- strings: leading colon, wrapping quotes (ASCII and Unicode), trailing
-- whitespace.
stripExceptionDecorations :: Text -> Text
stripExceptionDecorations =
    T.dropAround (`elem` (" :'\"\x2018\x2019" :: String))
  . T.strip
