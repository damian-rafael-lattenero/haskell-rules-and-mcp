-- | Unit tests for 'Tool.AddModules' and 'Tool.RemoveModules' handle-level
-- module validation, atomic refusal, and the scan-imports helper.
--
-- Extracted from the Spec.hs monolith (#271) via the function-export shape.
module Spec.AddModulesHandle
  ( testHandleAddModulesRefusesLowercaseModule
  , testHandleAddModulesAtomicRefusal
  , testHandleAddModulesAllOffendersListed
  , testHandleAddModulesHappyPathStillWorks
  , testHandleRemoveModulesRefuses
  , testRMScanImportPlain
  , testRMScanRespectsHierarchy
  , testRMScanQuietOnNoMatch
  ) where

import qualified Data.Aeson as A
import qualified Data.Aeson.Key as AKey
import qualified Data.Aeson.KeyMap as AKM
import qualified Data.ByteString.Lazy as BL
import qualified Data.Text as T
import qualified Data.Text.IO as TIO
import qualified Data.Text.Lazy as TL
import qualified Data.Text.Lazy.Encoding as TLE
import qualified Data.Vector as Vector
import Data.Text (Text)
import System.Directory (createDirectoryIfMissing, doesFileExist, getTemporaryDirectory, removePathForcibly)
import System.FilePath ((</>))

import qualified HaskellFlows.Mcp.Envelope as Env
import HaskellFlows.Types (mkProjectDir)
import qualified HaskellFlows.Types
import qualified HaskellFlows.Tool.AddModules as AddModules
import qualified HaskellFlows.Tool.RemoveModules as RM
import HaskellFlows.Mcp.Protocol (ToolContent (..), ToolResult (..))

import Spec.Helpers (getTestTimestamp)

--------------------------------------------------------------------------------
-- ISSUE-47 — End-to-end @ghc_add_modules@ refusal at the handler boundary
--
-- These tests drive the FULL handler against a tempdir-backed
-- @.cabal@. They prove:
--
--   (a) the validator is wired into the handler — bad input is
--       refused before any IO.
--   (b) the @.cabal@ is byte-identical pre/post call when at least
--       one name is invalid (atomic refusal — no partial writes).
--   (c) the rejection payload is structured exactly as the issue
--       specifies: success=false, error, rejected[{name,reason}].
--   (d) symmetric behaviour for @ghc_remove_modules@.
--   (e) @ghc_apply_exports@ rejects reserved keywords.
--   (f) regression: the happy path still succeeds and writes to
--       both the @.cabal@ AND the filesystem.
--------------------------------------------------------------------------------

-- | Minimal scaffolded .cabal a tempdir flow can write/read.
fixtureCabal :: Text
fixtureCabal = T.unlines
  [ "cabal-version: 3.0"
  , "name:          fixture"
  , "version:       0.0.0"
  , "library"
  , "    default-language: GHC2024"
  , "    hs-source-dirs:   src"
  , "    exposed-modules:  Foo"
  , "    build-depends:    base"
  ]

-- | Drive 'AddModules.handle' against a tempdir @ProjectDir@ with
-- a freshly-written fixture .cabal. Returns (cabal-after, payload).
withFixture :: (HaskellFlows.Types.ProjectDir -> FilePath -> IO a) -> IO a
withFixture k = do
  tmp <- getTemporaryDirectory
  ts  <- show <$> getTestTimestamp
  let dir       = tmp </> ("haskell-flows-mn-" <> ts)
      cabalFile = dir </> "fixture.cabal"
  createDirectoryIfMissing True dir
  TIO.writeFile cabalFile fixtureCabal
  res <- case mkProjectDir dir of
    Left _   -> error "fixture: mkProjectDir failed"
    Right pd -> k pd cabalFile
  removePathForcibly dir
  pure res

-- | Decode a 'ToolResult' content payload back into a JSON 'Value'
-- so the tests can pattern-match on field shape (success, error,
-- rejected[]). Mirrors what an MCP client would do.
extractPayload :: ToolResult -> A.Value
extractPayload tr = case trContent tr of
  (TextContent t : _) -> case A.eitherDecodeStrict (encodeUtf8Strict t) of
    Right v -> v
    Left _  -> A.Null
  _ -> A.Null
  where
    encodeUtf8Strict = BL.toStrict . TLE.encodeUtf8 . TL.fromStrict

-- | Issue #90: drill through the envelope to the inner @result@
-- payload. Most pre-envelope tests inspected fields at the top
-- level — those fields now live under @result@. Returns the
-- top-level Value when there's no @result@ field (graceful
-- back-compat for tools still emitting the pre-envelope shape).
resultPayload :: ToolResult -> A.Value
resultPayload tr = case extractPayload tr of
  A.Object o -> case AKM.lookup (AKey.fromText "result") o of
    Just inner -> inner
    Nothing    -> A.Object o
  v          -> v

hasField :: Text -> A.Value -> Bool
hasField k (A.Object o) = AKM.member (AKey.fromText k) o
hasField _ _            = False

lookupField :: Text -> A.Value -> Maybe A.Value
lookupField k (A.Object o) = AKM.lookup (AKey.fromText k) o
lookupField _ _            = Nothing

fieldEquals :: Text -> A.Value -> A.Value -> Bool
fieldEquals k expected v = lookupField k v == Just expected

-- | The exact bug from issue #47 — driven through the handler.
-- AFTER the fix the handler MUST refuse the call AND leave the
-- .cabal unmodified.
testHandleAddModulesRefusesLowercaseModule :: IO Bool
testHandleAddModulesRefusesLowercaseModule = withFixture $ \pd cabalFile -> do
  let args = A.object [ "modules" A..= (["lowercase.module"] :: [Text]) ]
  before <- TIO.readFile cabalFile
  result <- AddModules.handle pd args
  after  <- TIO.readFile cabalFile
  let isErr        = trIsError result
      envelope     = extractPayload result
      innerResult  = resultPayload result
  pure
    (  isErr
    && before == after
    -- Issue #90 Phase D step 2: 'rejected' moved under 'result';
    -- the legacy 'success' top-level field is dropped, callers
    -- branch on 'status' (here, 'failed' since the rejection is
    -- a domain validation failure, not a sanitize-layer refusal).
    && hasField "rejected" innerResult
    && fieldEquals "status" (A.String "failed") envelope
    )

-- | Atomic refusal: ANY bad name in the batch MUST refuse the
-- entire call. The good name is NOT registered. (Without atomic
-- refusal the agent's worldview drifts from disk reality.)
testHandleAddModulesAtomicRefusal :: IO Bool
testHandleAddModulesAtomicRefusal = withFixture $ \pd cabalFile -> do
  let args = A.object
        [ "modules" A..= (["GoodOne", "lowercase.module", "GoodTwo"] :: [Text]) ]
  before <- TIO.readFile cabalFile
  _      <- AddModules.handle pd args
  after  <- TIO.readFile cabalFile
  pure
    (  before == after
    && not ("GoodOne" `T.isInfixOf` after)
    && not ("GoodTwo" `T.isInfixOf` after)
    )

-- | The rejection payload MUST list every offender so the LLM can
-- fix all bad names in one round-trip (not N round-trips, one per
-- bad name).
testHandleAddModulesAllOffendersListed :: IO Bool
testHandleAddModulesAllOffendersListed = withFixture $ \pd _ -> do
  let args = A.object
        [ "modules" A..= (["1Foo", "lowercase", "Foo.module"] :: [Text]) ]
  result <- AddModules.handle pd args
  let payload   = resultPayload result
      rejected  = lookupField "rejected" payload
      names     = case rejected of
        Just (A.Array xs) -> map (lookupField "name") (Vector.toList xs)
        _                 -> []
  pure $ Just (A.String "1Foo")        `elem` names
      && Just (A.String "lowercase")   `elem` names
      && Just (A.String "Foo.module")  `elem` names

-- | Regression: the happy path still works post-fix. We write a
-- valid module and verify both the .cabal and a stub source file
-- get created.
testHandleAddModulesHappyPathStillWorks :: IO Bool
testHandleAddModulesHappyPathStillWorks = withFixture $ \pd cabalFile -> do
  let args = A.object [ "modules" A..= (["NewMod"] :: [Text]) ]
  result <- AddModules.handle pd args
  after  <- TIO.readFile cabalFile
  -- Stub file exists at the conventional location.
  stubExists <- doesFileExist
                  (HaskellFlows.Types.unProjectDir pd </> "src" </> "NewMod.hs")
  pure
    (  not (trIsError result)
    && "NewMod" `T.isInfixOf` after
    && stubExists
    )

-- | Symmetric: 'ghc_remove_modules' refuses the same shape. Even
-- though removal is "destructive" (the bad name was never legal in
-- the first place), the handler refuses on principle so a typo
-- can't propagate.
testHandleRemoveModulesRefuses :: IO Bool
testHandleRemoveModulesRefuses = withFixture $ \pd cabalFile -> do
  let args = A.object [ "modules" A..= (["lowercase.module"] :: [Text]) ]
  before <- TIO.readFile cabalFile
  result <- RM.handle pd args
  after  <- TIO.readFile cabalFile
  pure
    (  trIsError result
    && before == after
    && hasField "rejected" (resultPayload result)
    )

-- | Issue #41: 'parseImportLine' / 'scanImportersInBody' must
-- recognise the canonical Haskell import shapes and ignore
-- everything else.

testRMScanImportPlain :: IO Bool
testRMScanImportPlain =
  let body = T.unlines
        [ "module Other where"
        , ""
        , "import Foo"
        , "import Bar.Baz (x, y)"
        , "import qualified Foo as F"
        , "import qualified Mtl"
        ]
      hits = RM.scanImportersInBody "test/Other.hs" ["Foo"] body
  in pure (length hits == 2
        && all ((== "Foo") . RM.iModule) hits
        && all ((== "test/Other.hs") . RM.iFile) hits)

-- | Issue #41 — module names match as whole tokens, NOT
-- substrings. Removing 'Foo' must NOT flag 'import Foo.Bar'.
testRMScanRespectsHierarchy :: IO Bool
testRMScanRespectsHierarchy =
  let body = T.unlines [ "import Foo.Bar", "import Foo.Baz" ]
  in pure (null (RM.scanImportersInBody "x.hs" ["Foo"] body))

-- | Issue #41 — empty body / no targets / unrelated body all
-- yield no hits. (Defensive trio so regressions don't slip in
-- via accidental sentinel matches.)
testRMScanQuietOnNoMatch :: IO Bool
testRMScanQuietOnNoMatch = pure $
     null (RM.scanImportersInBody "f.hs" ["Foo"] "")
  && null (RM.scanImportersInBody "f.hs" []      "import Foo\n")
  && null (RM.scanImportersInBody "f.hs" ["Foo"] "module Other where\n")
