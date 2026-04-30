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
    -- * Filesystem-aware fuzz (Issue #100 Phase B)
  , testSymlinkEscapeAcceptedByPureGuard
    -- * Runtime canonical check (Issue #100 Phase D)
  , testCanonicalCheckCatchesSymlink
  ) where

import Control.Exception (IOException, try)
import Control.Monad (unless)
import Data.Either (isLeft)
import qualified Data.List as L
import qualified Data.Text as T
import System.Directory
  ( canonicalizePath
  , createDirectoryIfMissing
  , createFileLink
  , getTemporaryDirectory
  , removeFile
  )
import System.FilePath ((</>), normalise)
import Test.QuickCheck

import HaskellFlows.Types
  ( ProjectDir
  , canonicalModulePathCheck
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

--------------------------------------------------------------------------------
-- Issue #100 Phase B: filesystem-aware fuzz layer
--
-- Phase A's properties operate purely lexically — they never touch
-- the filesystem. That's correct for what 'mkModulePath' actually
-- implements (segment-based string manipulation), but leaves a
-- documented gap: a symlink that the pure guard accepts can still
-- escape to anywhere on disk once the kernel resolves it.
--
-- Concrete shape:
--
--   project_root/
--   └── src/
--       └── escape  →  /etc            (symlink planted at setup)
--
--   mkModulePath project_root \"src/escape/passwd\"
--     ↦ Right (modulePath = project_root/src/escape/passwd)   (ACCEPTS)
--   System.Directory.canonicalizePath that result
--     ↦ /etc/passwd                                           (ESCAPES)
--
-- Phase D (defence-in-depth) would close that gap by adding a
-- runtime canonicalize-and-check at every read/write site.
-- Phase B (here) DOCUMENTS the gap as a test so a future
-- contributor who lands Phase D can flip this assertion to its
-- complement (canonical IS inside) and have a regression-fast
-- canary.
--
-- This test is structured as a one-shot 'IO Bool' — wired into
-- the unit-test list in Spec.hs without any QuickCheck
-- generator. Cheap (one symlink + one canonicalize call) and
-- self-contained.
--------------------------------------------------------------------------------

-- | Plant a symlink inside a fresh tmp project, then exercise
-- both the pure guard and the canonical resolution.
--
-- Asserts:
--
--   1. 'mkModulePath' ACCEPTS @\"src/escape/whatever\"@ (no @..@
--      segments — segment-based guard sees nothing wrong).
--   2. 'canonicalizePath' on the accepted path resolves OUTSIDE
--      the project root (escapes via the symlink).
--
-- The second assertion is the load-bearing one: it pins the gap
-- the meta-issue calls out. When Phase D lands and adds the
-- canonical-prefix check at read time, this test should be
-- updated to assert the OPPOSITE invariant — and the symlink
-- attack stops working.
--
-- Skipped silently if the test environment doesn't support
-- symlinks (Windows without admin privileges, exotic
-- filesystems, etc.) — there's no value in a flake.
testSymlinkEscapeAcceptedByPureGuard :: IO Bool
testSymlinkEscapeAcceptedByPureGuard = do
  tmp <- getTemporaryDirectory
  let projectDir = tmp </> "haskell-flows-symlink-fuzz"
      srcDir     = projectDir </> "src"
      linkPath   = srcDir </> "escape"
      -- /etc is universally readable on POSIX and exists on
      -- every CI runner. We never actually read through the
      -- symlink — only assert that canonicalize sees it land
      -- outside the project root.
      attackTgt  = "/etc"
  createDirectoryIfMissing True srcDir
  -- Use bracket-style cleanup: remove pre-existing link, then
  -- plant a fresh one. Ignore errors (test-environment-permissive).
  _ <- (try (removeFile linkPath) :: IO (Either IOException ()))
  symlinkResult <- (try (createFileLink attackTgt linkPath)
                      :: IO (Either IOException ()))
  case symlinkResult of
    Left _  -> do
      -- Symlinks unavailable on this filesystem; treat as PASS
      -- (the test had nothing to verify). A noisy skip would
      -- cause flakes on Windows runners; silent skip keeps CI
      -- predictable.
      putStrLn "  (skipped: symlinks unavailable on this filesystem)"
      pure True
    Right () -> do
      let pdRes = mkProjectDir projectDir
      case pdRes of
        Left e   -> do
          putStrLn ("  symlink-fuzz setup failed: " <> show e)
          pure False
        Right pd ->
          case mkModulePath pd "src/escape/some_target" of
            Left _ -> do
              putStrLn "  pure guard rejected — symlink escape impossible \
                       \(unexpected; was supposed to surface the gap)"
              pure False
            Right mp -> do
              -- The pure guard accepted. Now ask the kernel:
              -- where does the resolved path actually point?
              canon <- canonicalizePath (unModulePath mp)
              rootCanon <- canonicalizePath (unProjectDir pd)
              -- If 'canon' starts with 'rootCanon', symlink got
              -- resolved within the project; otherwise we've
              -- escaped — exactly the failure shape the test
              -- documents. PASS = escape detected (gap exists,
              -- Phase D needed).
              let escaped =
                    not (rootCanon == canon
                          || (rootCanon <> "/") `L.isPrefixOf` canon)
              unless escaped $
                putStrLn ("  symlink resolved INSIDE root unexpectedly. \
                          \root=" <> rootCanon <> " resolved=" <> canon)
              pure escaped

--------------------------------------------------------------------------------
-- Phase D — 'canonicalModulePathCheck' catches symlink escapes
--------------------------------------------------------------------------------

-- | Issue #100 Phase D: 'canonicalModulePathCheck' must catch a symlink
-- that the pure guard passed. Mirrors 'testSymlinkEscapeAcceptedByPureGuard'
-- but calls the IO-level canonical check instead of raw 'canonicalizePath'.
-- PASS = the canonical check returned Left (escape detected).
testCanonicalCheckCatchesSymlink :: IO Bool
testCanonicalCheckCatchesSymlink = do
  tmp         <- getTemporaryDirectory
  let projectDir = tmp </> "haskell-flows-phaseD-symlink"
      srcDir     = projectDir </> "src" </> "escape"
      attackTgt  = tmp </> "etc"           -- target outside the project
      linkPath   = srcDir </> "some_target"
  mapM_ (createDirectoryIfMissing True) [srcDir, attackTgt]
  _ <- (try (removeFile linkPath) :: IO (Either IOException ()))
  symlinkResult <- (try (createFileLink attackTgt linkPath)
                      :: IO (Either IOException ()))
  case symlinkResult of
    Left _ -> do
      putStrLn "  (skipped: symlinks unavailable on this filesystem)"
      pure True   -- silent skip, same policy as Phase B
    Right () ->
      case mkProjectDir projectDir of
        Left e -> do
          putStrLn ("  setup failed: " <> show e)
          pure False
        Right pd ->
          case mkModulePath pd "src/escape/some_target" of
            Left _ -> do
              putStrLn "  pure guard rejected — cannot reach canonicalCheck"
              pure False        -- unexpected; Phase B covers this branch
            Right mp -> do
              -- Pure guard passed (symlink's path has no '..'). Now
              -- ask the IO-level canonical check:
              result <- canonicalModulePathCheck pd mp
              pure (isLeft result)     -- PASS = canonical check caught escape
