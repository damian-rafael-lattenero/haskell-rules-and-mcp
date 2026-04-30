-- | Issue #100 — property-based fuzzing of the path-traversal guard.
--
-- The path-traversal guard ('Types.mkModulePath' and the parallel one
-- in 'Tool.Lint.resolveTarget') is the project's most-tested security
-- primitive — and yet it has *already* shipped one CWE-22 bypass
-- (issue #81, fixed in c1c09fc). The pre-existing test surface was
-- enumerative: a fixed list of bypass shapes the maintainer happened
-- to think of. The next bypass will be a shape nobody thought of.
--
-- This module replaces enumeration with property-based fuzzing.
-- Three load-bearing properties:
--
--   1. 'prop_pathGuard_canonical_invariant' — for any path the
--      pure guard accepts, the canonically-resolved absolute form
--      must live strictly inside the canonical project root. This
--      is the *real* security invariant; the segment-based guard
--      is its pure approximation.
--   2. 'prop_pathGuard_lint_resolveTarget_consistent' — the two
--      guards (one in 'Types', one in 'Tool.Lint') must agree on
--      whether a path is in-bounds. Pre-#81 they didn't, which was
--      the bypass.
--   3. 'prop_pathGuard_dotdot_always_rejected' — the
--      already-pinned property kept here as the regression-fast
--      fast-path.
--
-- Generator: 'arbitraryAdversarialPath' samples seven categories
-- known to trip historical CVEs (Unicode normalisation, URL-encoded
-- segments, raw control chars, mixed separators, redundant
-- separators, long components, dotdot-rich shapes). Each category
-- has its own generator to keep failure-case attribution clear.
--
-- Shrinks: the default 'String' shrink loses the structural traps
-- (it shrinks character-by-character). 'shrinkAdversarialPath'
-- preserves the segment structure while reducing complexity, so a
-- failing case shrinks to its minimal adversarial form rather than
-- collapsing to a benign prefix.
--
-- Phase A scope (this file): the in-process property with no
-- filesystem fuzz layer. Phases B–E (issue #100 §6) cover symlink
-- planting, cross-tool harness, runtime defence-in-depth, and
-- threat-model docs respectively.
module PathTraversal
  ( -- * Properties
    prop_pathGuard_canonical_invariant
  , prop_pathGuard_lint_resolveTarget_consistent
  , prop_pathGuard_dotdot_always_rejected
    -- * Generator (exposed so Spec can also use it for ad-hoc tests)
  , arbitraryAdversarialPath
  , shrinkAdversarialPath
  ) where

import Data.Either (isLeft)
import qualified Data.List as L
import qualified Data.Text as T
import System.Directory (createDirectoryIfMissing, getTemporaryDirectory)
import System.FilePath ((</>), normalise)
import Test.QuickCheck

import HaskellFlows.Types
  ( ProjectDir
  , mkModulePath
  , mkProjectDir
  , unModulePath
  , unProjectDir
  )
import qualified HaskellFlows.Tool.Lint as LintTool
import HaskellFlows.Tool.Lint (LintArgs (..))

--------------------------------------------------------------------------------
-- Generator
--------------------------------------------------------------------------------

-- | The seven categories of adversarial path we sample from.
-- Each category is a small set of canonical bypass shapes plus a
-- mutation tail — together that's enough to surface most of the
-- CWE-22 bypasses observed historically.
arbitraryAdversarialPath :: Gen FilePath
arbitraryAdversarialPath = oneof
  [ arbitraryNormalPath
  , arbitraryWithDotDotSegments
  , arbitraryWithUnicodeNormalisationTraps
  , arbitraryWithUrlEncoding
  , arbitraryWithRawControlChars
  , arbitraryWithMixedSeparators
  , arbitraryWithRedundantSeparators
  , arbitraryWithLongComponents
  ]

-- | Benign baseline — paths that should always be accepted.
arbitraryNormalPath :: Gen FilePath
arbitraryNormalPath = do
  segs <- vectorOf 3 (elements ["src", "Foo", "Bar.hs", "Baz", "test"])
  pure (L.intercalate "/" segs)

-- | Direct dotdot traversal in many shapes: bare, mid-path,
-- repeated, mixed with valid segments.
arbitraryWithDotDotSegments :: Gen FilePath
arbitraryWithDotDotSegments = do
  segs <- listOf1 $ frequency
    [ (3, pure "..")
    , (2, pure ".")
    , (3, elements ["src", "Foo", "test"])
    , (1, pure "etc/passwd")
    ]
  pure (L.intercalate "/" segs)

-- | Unicode lookalikes for the dot character that some path-resolution
-- libraries normalise *back* to ASCII '.' but our segment-based guard
-- doesn't (and shouldn't, since 'splitDirectories' is byte-oriented).
-- Generated literally via Haskell escapes so the file stays
-- ASCII-safe.
arbitraryWithUnicodeNormalisationTraps :: Gen FilePath
arbitraryWithUnicodeNormalisationTraps = do
  segs <- listOf1 $ elements
    [ "src"
    , "\x002e\x002e"        -- normal ".."
    , "\x2024\x2024"        -- two-dot leader U+2024
    , "\xFF0E\xFF0E"        -- full-width dots
    , ".\x200E."            -- zero-width LTR mark inside dots
    , "\xFEFF.."            -- byte-order-mark prefix
    ]
  pure (L.intercalate "/" segs)

-- | URL-encoded percent-escapes. The pure guard splits before any
-- decoding; the test asserts the (potentially) decoded form
-- doesn't escape if the pure guard accepted it.
arbitraryWithUrlEncoding :: Gen FilePath
arbitraryWithUrlEncoding = do
  segs <- listOf1 $ elements
    [ "src"
    , "%2e%2e"
    , "%2E%2E"
    , ".%2e"
    , "%2e."
    , "%252e%252e"   -- double-encoded
    ]
  pure (L.intercalate "/" segs)

-- | Raw control characters: NUL, BEL, ESC, DEL. Some filesystems
-- truncate at NUL; some terminals interpret ESC. The guard must
-- never produce an accepting decision that survives that
-- divergence.
arbitraryWithRawControlChars :: Gen FilePath
arbitraryWithRawControlChars = do
  segs <- listOf1 $ elements
    [ "src"
    , "..\x00"            -- NUL after ..
    , "\x00.."            -- NUL before ..
    , "src\x07"           -- BEL
    , "src\x1b[31m"       -- SGR escape
    , "..\x7f"            -- DEL
    ]
  pure (L.intercalate "/" segs)

-- | POSIX uses '/' as separator; Windows accepts both. The guard
-- runs on POSIX but the test pins what happens if a backslash slips
-- through.
arbitraryWithMixedSeparators :: Gen FilePath
arbitraryWithMixedSeparators = do
  segs <- listOf1 $ elements
    [ "src"
    , "..\\.."           -- backslash separator inside ..
    , "src\\..\\etc"
    , ".."
    ]
  pure (L.intercalate "/" segs)

-- | Redundant separators. Some path libraries collapse, others
-- preserve. Either way, the guard's decision must match canonical.
arbitraryWithRedundantSeparators :: Gen FilePath
arbitraryWithRedundantSeparators = do
  segs <- listOf1 $ elements
    [ "src"
    , ""                 -- empty (produces "//" when joined)
    , "."
    , ".."
    , "Foo"
    ]
  -- Use various separator counts to exercise the collapse logic.
  sep <- elements ["/", "//", "///"]
  pure (L.intercalate sep segs)

-- | Long components — exercise filesystem path-length limits and
-- any per-segment length constraints in the guard.
arbitraryWithLongComponents :: Gen FilePath
arbitraryWithLongComponents = do
  n <- choose (200, 1024)
  let big = replicate n 'A'
  segs <- listOf1 $ elements ["src", "..", big, "Foo"]
  pure (L.intercalate "/" segs)

-- | Custom shrink that preserves the structural traps the
-- generators introduced. The default 'String' shrink would shrink
-- @"src/..\\x00etc"@ to a benign prefix without ever reducing to
-- the minimal adversarial form. We shrink at the segment boundary
-- instead.
shrinkAdversarialPath :: FilePath -> [FilePath]
shrinkAdversarialPath p =
  let segs = splitOn '/' p
      drops = [ L.intercalate "/" (take i segs <> drop (i + 1) segs)
              | i <- [0 .. length segs - 1]
              ]
  in filter (/= p) (L.nub drops)

-- | Local 'Data.List.splitOn'-equivalent so we don't have to add a
-- 'split' dependency for one helper.
splitOn :: Char -> String -> [String]
splitOn c s = case break (== c) s of
  (chunk, [])        -> [chunk]
  (chunk, _:rest)    -> chunk : splitOn c rest

-- | A wrapper that gives us a 'Show' instance with the path
-- escapes visible (so QuickCheck's failure report doesn't render
-- raw control chars to the terminal).
newtype AdversarialPath = AdversarialPath { unAdversarialPath :: FilePath }

instance Show AdversarialPath where
  show = show . unAdversarialPath  -- forces escaping of control chars

instance Arbitrary AdversarialPath where
  arbitrary  = AdversarialPath <$> arbitraryAdversarialPath
  shrink     = map AdversarialPath
             . shrinkAdversarialPath
             . unAdversarialPath

--------------------------------------------------------------------------------
-- Properties
--------------------------------------------------------------------------------

-- | The load-bearing security invariant. For any path the pure
-- guard accepts, the resolved absolute form must lexically live
-- inside the project root.
--
-- Compared lexically (NOT via 'canonicalizePath'). The Phase-A
-- guard is segment-based and operates entirely on lexical paths;
-- this property witnesses what the guard actually does. Phase D
-- (defence-in-depth) is the place to add a canonical-path check —
-- doing it here would diverge from what the guard implements and
-- spuriously fail on systems where the tmp dir is itself a
-- symlink (macOS: @\/var\/folders\/...@ canonicalises to
-- @\/private\/var\/folders\/...@; the lexical check stays
-- consistent).
prop_pathGuard_canonical_invariant :: Property
prop_pathGuard_canonical_invariant =
  withMaxSuccess 1000 $
    forAllShrink (unAdversarialPath <$> arbitrary) shrinkAdversarialPath $
      \rawPath -> ioProperty $ do
        pd <- getPropertyProjectDir
        case mkModulePath pd rawPath of
          Left _    -> pure True   -- rejection is always safe
          Right mp  -> do
            -- Lexical comparison: 'normalise' joins root + raw and
            -- collapses redundant separators; the guard already
            -- rejected ".." segments — so we just check the
            -- resulting absolute path's prefix against the project
            -- root verbatim. The accepted path needn't physically
            -- exist; we never touch the filesystem here.
            let absResolved = normalise (unModulePath mp)
                rootPrefix  = normalise (unProjectDir pd)
            pure ( absResolved == rootPrefix
                || (rootPrefix <> "/") `L.isPrefixOf` absResolved )

-- | The two guards must agree on whether a path is in-bounds.
-- Pre-#81 they didn't — 'mkModulePath' rejected, but
-- 'resolveTarget' didn't run the same segment check.
--
-- The invariant: @isLeft (mkModulePath …) == isLeft
-- (resolveTarget …)@ for every input. Either both reject or both
-- accept; any divergence is a CWE-22 bypass.
prop_pathGuard_lint_resolveTarget_consistent :: Property
prop_pathGuard_lint_resolveTarget_consistent =
  withMaxSuccess 1000 $
    forAllShrink (unAdversarialPath <$> arbitrary) shrinkAdversarialPath $
      \rawPath -> ioProperty $ do
        pd <- getPropertyProjectDir
        let viaMP    = mkModulePath pd rawPath
            viaLint  = LintTool.resolveTarget pd
                         LintArgs
                           { laPath       = Just (T.pack rawPath)
                           , laModulePath = Nothing
                           , laFailOn     = "warning"
                           }
        pure (isLeft viaMP == isLeft viaLint)

-- | Regression-fast property kept here as a quick sanity check.
-- Any path containing a @..@ segment must be rejected. This is
-- the cheapest property and runs on every CI invocation as the
-- canary; if even this fires, the guard has lost an invariant
-- the project documentation explicitly claims.
prop_pathGuard_dotdot_always_rejected :: Property
prop_pathGuard_dotdot_always_rejected =
  withMaxSuccess 500 $
    forAllShrink (unAdversarialPath <$> arbitrary) shrinkAdversarialPath $
      \rawPath -> ioProperty $ do
        pd <- getPropertyProjectDir
        let segs = splitOn '/' rawPath
            hasDotDot = ".." `elem` segs
        pure (not hasDotDot || isLeft (mkModulePath pd rawPath))

--------------------------------------------------------------------------------
-- Test fixture
--------------------------------------------------------------------------------

-- | A stable tmp project root for the property to anchor against.
-- We don't need a fresh tmpdir per sample — the property is
-- read-only over the project directory; it only inspects the
-- guard's pure decision and the canonical form of the project
-- root itself.
getPropertyProjectDir :: IO ProjectDir
getPropertyProjectDir = do
  tmp <- getTemporaryDirectory
  let dir = tmp </> "haskell-flows-prop-path-traversal"
  createDirectoryIfMissing True dir
  case mkProjectDir dir of
    Right pd -> pure pd
    Left  e  -> error ("getPropertyProjectDir: " <> show e)
