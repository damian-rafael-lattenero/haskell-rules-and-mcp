-- | @ghc_quickcheck@ — pure helpers + cabal introspection.
--
-- W6.8 removed this tool's execution engine entirely: routeIde
-- serves @ghc_property(action=check)@ via
-- 'IdeBacked.handlePropertyCheck' (the QuickCheck run is rendered
-- into the evaluated expression itself), and W6.8.2 rewired the
-- property-store audit probes onto 'IdeBacked.ideQcProbe' — the
-- same anchor-chain evaluation, mapped onto 'QuickCheckResult'.
-- The subprocess cabal-repl harnesses and the GhcSession-bound
-- in-process harness died with the ApiSession backend.
--
-- What remains is what the live routes and 'Tool.QuickCheckExport'
-- still consume:
--
--   * 'qcMaxSuccess' — the #283 case-count contract
--   * 'chooseStoreModule' / 'isSimpleIdent' — pure store helpers
--   * 'libraryExposedModules' / 'scanLibraryExposedModules' —
--     cabal introspection (QuickCheckExport, and the audit's
--     legacy widening hints survive only in the docs)
module HaskellFlows.Tool.QuickCheck
  ( -- * Pure helpers exposed for unit tests
    chooseStoreModule
  , isSimpleIdent
    -- * #283 — the QuickCheck case count a single check runs at
  , qcMaxSuccess
    -- * Cabal library introspection (re-used by 'Tool.QuickCheckExport')
  , libraryExposedModules
  , scanLibraryExposedModules
  ) where

import Control.Exception (SomeException, try)
import Data.Char (isAlpha, isAlphaNum)
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.IO as TIO
import System.Directory (listDirectory)
import System.FilePath (takeExtension, (</>))

import qualified HaskellFlows.Tool.Deps as Deps

import HaskellFlows.Config (defaultLimits, quickCheckMaxSuccess)
import HaskellFlows.Types (ProjectDir, unProjectDir)

-- | #283: QuickCheck cases per single check. Raised from the stdArgs default
-- of 100 to 300 so a single ghc_quickcheck is markedly more likely to surface a
-- false law before it is auto-persisted (the dogfood counterexample was missed
-- at 100 by seed luck). The value is recorded in the property store as the
-- confidence ('spCases') behind each persisted law.
qcMaxSuccess :: Int
qcMaxSuccess = quickCheckMaxSuccess defaultLimits

--------------------------------------------------------------------------------
-- store-module resolution
--------------------------------------------------------------------------------

-- | Pure selector: given the property text, the caller's hint, and
-- (optionally) the @:info@ output, pick which path to persist.
--
-- The caller hint wins verbatim — the regression store only uses
-- the module to reload the right compile scope.
chooseStoreModule :: Text -> Maybe Text -> Maybe Text -> Maybe Text
chooseStoreModule _prop callerHint _mInfo = callerHint

-- | True iff @t@ parses as a single Haskell identifier (possibly
-- qualified with dots, e.g. @Spec.prop_x@).
isSimpleIdent :: Text -> Bool
isSimpleIdent t = case T.uncons t of
  Nothing      -> False
  Just (c, cs) ->
    (isAlpha c || c == '_')
    && T.all validRest cs
  where
    validRest c = isAlphaNum c || c == '_' || c == '\'' || c == '.'

--------------------------------------------------------------------------------
-- cabal library introspection
--------------------------------------------------------------------------------

-- | Read the project's @.cabal@ file and return every module name
-- listed under the library's @exposed-modules@. Returns @[]@ on any
-- parse or I/O failure — the caller falls back to whatever scope it
-- already provided.
libraryExposedModules :: ProjectDir -> IO [Text]
libraryExposedModules pd = do
  let root = unProjectDir pd
  ents <- try (listDirectory root) :: IO (Either SomeException [FilePath])
  case ents of
    Left _ -> pure []
    Right es ->
      case [root </> e | e <- es, takeExtension e == ".cabal"] of
        []    -> pure []
        (f:_) -> do
          eBody <- try (TIO.readFile f) :: IO (Either SomeException Text)
          case eBody of
            Left _     -> pure []
            Right body -> pure (scanLibraryExposedModules body)

-- | Pure parser: given a full @.cabal@ body, return library
-- exposed-module names. Scoped to the @library@ stanza via
-- 'Deps.sliceStanza'; returns @[]@ when the project has no
-- library (executable-only projects / benchmark-only projects).
--
-- A line-oriented parser lives here in-line — using the richer
-- 'HaskellFlows.Tool.CheckProject.parseExposedModules' would
-- introduce a module-graph cycle (CheckProject → CheckModule →
-- Regression → QuickCheck).
scanLibraryExposedModules :: Text -> [Text]
scanLibraryExposedModules body =
  case Deps.sliceStanza ("library", Nothing) (T.lines body) of
    Nothing             -> []
    Just (_, libLns, _) -> extractExposedModules libLns

-- | Given the lines of a SINGLE @library@ stanza, return every
-- module listed under @exposed-modules:@ — both inline on the
-- header and on continuation lines. Stops at the next cabal
-- field or stanza header.
extractExposedModules :: [Text] -> [Text]
extractExposedModules = go False
  where
    go _ [] = []
    go inside (ln : rest)
      | isExposedHeader ln =
          let inlineTail = T.strip (T.dropWhile (/= ':') ln)
              inlineNow  = T.strip (T.drop 1 inlineTail)
              nameHere   = [ inlineNow | not (T.null inlineNow) ]
          in nameHere <> go True rest
      | inside && isContinuation ln =
          let nm = T.strip ln
              newField = ':' `T.elem` nm
          in if newField
               then go False rest
               else [ nm | not (T.null nm) ] <> go True rest
      | otherwise = go False rest

    isExposedHeader ln =
      "exposed-modules:" `T.isPrefixOf` T.toLower (T.stripStart ln)

    -- A continuation is an indented line; blank lines also end the block.
    isContinuation ln =
      not (T.null (T.takeWhile (== ' ') ln)) && not (T.null (T.strip ln))
