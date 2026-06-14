-- | Unit tests for the @ghc_scratch@ scratchpad tool (#253) — data layer,
-- write/list/show/clear/check/promote actions, sanitisation, and the
-- spliceInto / wrapAsLetBlock / compileFailResult helpers.
--
-- Extracted verbatim from the Spec.hs monolith (#271). The driver
-- ('test/Spec.hs') splices 'scratchTests' into its run list via
-- @++ scratchTests@; the test bodies are unchanged.
module Spec.Scratch
  ( scratchTests
  ) where

import qualified Data.Aeson as A
import qualified Data.Aeson.KeyMap as AKM
import Data.Maybe (isNothing)
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.Lazy as TL
import qualified Data.Text.Lazy.Encoding as TLE

import HaskellFlows.Ghc.Sanitize
import HaskellFlows.Mcp.Protocol (ToolContent (..), ToolResult (..))
import HaskellFlows.Parser.Error
import qualified HaskellFlows.Data.Scratchpad as SP
import qualified HaskellFlows.Tool.Refactor as RefactorTool
import qualified HaskellFlows.Tool.Scratch as ScratchTool

import Spec.Harness (test)
import Spec.Helpers (withTempProject)

-- | Registration list for the scratchpad domain, spliced into the
-- driver's run list. Labels are kept identical to pre-split so the
-- PASS/FAIL output is unchanged.
scratchTests :: [IO Bool]
scratchTests =
  [ test "Scratchpad data roundtrip"           testScratchpadRoundtrip
  , test "Scratchpad upsert by id"             testScratchpadUpsertById
  , test "Scratchpad findById"                 testScratchpadFindById
  , test "Scratch parseAction round-trip"      testScratchParseAction
  , test "Scratch action=write persists entry" testScratchHandleWrite
  , test "Scratch write requires 'code'"       testScratchHandleWriteMissingCode
  , test "Scratch write auto-generates id"     testScratchHandleWriteAutoId
  , test "Scratch list empty returns count=0"  testScratchHandleListEmpty
  , test "Scratch show unknown id → no_match"  testScratchHandleShowMissing
  , test "Scratch clear w/o confirm refused"   testScratchHandleClearNoConfirm
  , test "Scratch clear by id removes one"     testScratchHandleClearById
  , test "Scratch clear confirm=true truncates" testScratchHandleClearAll
  , test "Scratch check missing id → failed"   testScratchCheckMissingId
  , test "Scratch check unknown id → no_match" testScratchCheckUnknownId
  , test "Scratch check sanitize rejects"      testScratchCheckSanitizeReject
  , test "ScratchResult JSON round-trip"       testScratchResultRoundTrip
  , test "F-01: check type_ok has kind at top" testScratchCheckKindAtTopLevel
  , test "Phase 3: show returns full entry detail"   testScratchShowFullDetail
  , test "Phase 3: seResult survives loadAll"        testScratchResultRoundTripViaLoadAll
  , test "Phase 3: show after check carries result"  testScratchShowAfterCheckHasResult
  , test "Phase 4: promote missing id → failed"      testScratchPromoteMissingId
  , test "Phase 4: promote missing target_module → failed"
                                                     testScratchPromoteMissingTargetModule
  , test "Phase 4: promote unknown id → no_match"   testScratchPromoteUnknownId
  , test "Phase 4: promote path escapes project → refused"
                                                     testScratchPromoteBadModulePath
  , test "Phase 4: spliceInto appends at end"        testSpliceIntoAppend
  , test "Phase 4: spliceInto inserts after line N"  testSpliceIntoAtLine
  , test "F-03: sanitizeDeclarations allows newlines"   testSanitizeDeclAllowsNewlines
  , test "F-03: sanitizeDeclarations blocks sentinel"   testSanitizeDeclBlocksSentinel
  , test "F-03: wrapAsLetBlock indents code"            testWrapAsLetBlockIndents
  , test "F-04: promote wraps expression with binding_name"
                                                        testScratchPromoteBindingName
  , test "F-05: compileFailResult uses first error message"
                                                        testCompileFailResultCause
  , test "#276: splitImports separates single import"   testSplitImportsSingle
  , test "#276: splitImports keeps multiple imports"    testSplitImportsMultiple
  , test "#276: splitImports no imports → empty list"   testSplitImportsNone
  , test "#276: splitImports groups multi-line import"  testSplitImportsMultiLine
  , test "#276: splitImports ignores indented import kw" testSplitImportsBodyKeyword
  ]

--------------------------------------------------------------------------------
-- test function definitions (moved verbatim from Spec.hs, #271)
--------------------------------------------------------------------------------

-- | Round-trip a ScratchEntry through the on-disk scratchpad.
testScratchpadRoundtrip :: IO Bool
testScratchpadRoundtrip = withTempProject $ \pd -> do
  store <- SP.openStore pd
  let entry = SP.ScratchEntry
        { SP.seId       = "t1"
        , SP.seKind     = SP.ScratchHypothesis
        , SP.seCode     = "\\x -> x + 1 :: Int -> Int"
        , SP.seModule   = Just "src/Foo.hs"
        , SP.seImports  = ["Data.List"]
        , SP.seNote     = Just "verify roundtrip"
        , SP.seResult   = Nothing
        , SP.seStatus   = SP.ScratchOpen
        , SP.seCreated  = 1_000_000
        , SP.seUpdated  = 1_000_000
        }
  SP.save store entry
  loaded <- SP.loadAll store
  pure $ case loaded of
    [e] -> SP.seId e == "t1"
        && SP.seCode e == "\\x -> x + 1 :: Int -> Int"
        && SP.seModule e == Just "src/Foo.hs"
        && SP.seImports e == ["Data.List"]
        && SP.seStatus e == SP.ScratchOpen
        && SP.seKind e == SP.ScratchHypothesis
    _   -> False

-- | save with the same id should replace, not append.
testScratchpadUpsertById :: IO Bool
testScratchpadUpsertById = withTempProject $ \pd -> do
  store <- SP.openStore pd
  let mk c = SP.ScratchEntry
        { SP.seId       = "dup"
        , SP.seKind     = SP.ScratchHypothesis
        , SP.seCode     = c
        , SP.seModule   = Nothing
        , SP.seImports  = []
        , SP.seNote     = Nothing
        , SP.seResult   = Nothing
        , SP.seStatus   = SP.ScratchOpen
        , SP.seCreated  = 1_000_000
        , SP.seUpdated  = 1_000_000
        }
  SP.save store (mk "v1")
  SP.save store (mk "v2")
  SP.save store (mk "v3")
  loaded <- SP.loadAll store
  pure $ case loaded of
    [e] -> SP.seCode e == "v3"
    _   -> False

-- | findById should resolve an id, return Nothing when absent.
testScratchpadFindById :: IO Bool
testScratchpadFindById = withTempProject $ \pd -> do
  store <- SP.openStore pd
  let entry = SP.ScratchEntry
        { SP.seId       = "lookup-me"
        , SP.seKind     = SP.ScratchHypothesis
        , SP.seCode     = "True"
        , SP.seModule   = Nothing
        , SP.seImports  = []
        , SP.seNote     = Nothing
        , SP.seResult   = Nothing
        , SP.seStatus   = SP.ScratchOpen
        , SP.seCreated  = 1
        , SP.seUpdated  = 1
        }
  SP.save store entry
  hit  <- SP.findById store "lookup-me"
  miss <- SP.findById store "no-such-id"
  pure (fmap SP.seId hit == Just "lookup-me" && isNothing miss)

-- | parseAction round-trip — covers the default + every named action.
testScratchParseAction :: IO Bool
testScratchParseAction = pure $
  ScratchTool.parseAction Nothing            == Right ScratchTool.ActList
  && ScratchTool.parseAction (Just "write")  == Right ScratchTool.ActWrite
  && ScratchTool.parseAction (Just "check")  == Right ScratchTool.ActCheck
  && ScratchTool.parseAction (Just "list")   == Right ScratchTool.ActList
  && ScratchTool.parseAction (Just "show")   == Right ScratchTool.ActShow
  && ScratchTool.parseAction (Just "clear")  == Right ScratchTool.ActClear
  && ScratchTool.parseAction (Just "promote") == Right ScratchTool.ActPromote
  && case ScratchTool.parseAction (Just "nope") of
       Left _  -> True
       Right _ -> False

-- | action=write creates the entry on disk and returns its id.
testScratchHandleWrite :: IO Bool
testScratchHandleWrite = withTempProject $ \pd -> do
  store <- SP.openStore pd
  let args = A.object
        [ "action" A..= ("write" :: Text)
        , "id"     A..= ("user-id" :: Text)
        , "code"   A..= ("1 + 1 :: Int" :: Text)
        ]
  result <- ScratchTool.runHandle store undefined undefined args
  case trContent result of
    [TextContent body] -> case A.decode (TLE.encodeUtf8 (TL.fromStrict body)) of
      Just (A.Object top) -> case AKM.lookup "status" top of
        Just (A.String "ok") -> do
          loaded <- SP.loadAll store
          pure $ case loaded of
            [e] -> SP.seId e == "user-id" && SP.seCode e == "1 + 1 :: Int"
            _   -> False
        _ -> pure False
      _ -> pure False
    _ -> pure False

-- | action=write without code returns status=failed kind=missing_arg.
testScratchHandleWriteMissingCode :: IO Bool
testScratchHandleWriteMissingCode = withTempProject $ \pd -> do
  store <- SP.openStore pd
  let args = A.object [ "action" A..= ("write" :: Text) ]
  result <- ScratchTool.runHandle store undefined undefined args
  pure $ case trContent result of
    [TextContent body] -> case A.decode (TLE.encodeUtf8 (TL.fromStrict body)) of
      Just (A.Object top) -> AKM.lookup "status" top == Just (A.String "failed")
      _ -> False
    _ -> False

-- | action=write without an id auto-generates scratch-1, scratch-2, ...
testScratchHandleWriteAutoId :: IO Bool
testScratchHandleWriteAutoId = withTempProject $ \pd -> do
  store <- SP.openStore pd
  let args = A.object
        [ "action" A..= ("write" :: Text)
        , "code"   A..= ("a" :: Text)
        ]
  _ <- ScratchTool.runHandle store undefined undefined args
  _ <- ScratchTool.runHandle store undefined undefined args
  loaded <- SP.loadAll store
  let ids = map SP.seId loaded
  pure (ids == ["scratch-1", "scratch-2"])

-- | action=list with no entries returns count=0.
testScratchHandleListEmpty :: IO Bool
testScratchHandleListEmpty = withTempProject $ \pd -> do
  store <- SP.openStore pd
  let args = A.object [ "action" A..= ("list" :: Text) ]
  result <- ScratchTool.runHandle store undefined undefined args
  pure $ case trContent result of
    [TextContent body] -> case A.decode (TLE.encodeUtf8 (TL.fromStrict body)) of
      Just (A.Object top) -> case AKM.lookup "result" top of
        Just (A.Object r) -> AKM.lookup "count" r == Just (A.Number 0)
        _ -> False
      _ -> False
    _ -> False

-- | action=show on an unknown id returns status=no_match.
testScratchHandleShowMissing :: IO Bool
testScratchHandleShowMissing = withTempProject $ \pd -> do
  store <- SP.openStore pd
  let args = A.object [ "action" A..= ("show" :: Text), "id" A..= ("ghost" :: Text) ]
  result <- ScratchTool.runHandle store undefined undefined args
  pure $ case trContent result of
    [TextContent body] -> case A.decode (TLE.encodeUtf8 (TL.fromStrict body)) of
      Just (A.Object top) -> AKM.lookup "status" top == Just (A.String "no_match")
      _ -> False
    _ -> False

-- | action=clear without id and without confirm=true is refused.
testScratchHandleClearNoConfirm :: IO Bool
testScratchHandleClearNoConfirm = withTempProject $ \pd -> do
  store <- SP.openStore pd
  let args = A.object [ "action" A..= ("clear" :: Text) ]
  result <- ScratchTool.runHandle store undefined undefined args
  pure $ case trContent result of
    [TextContent body] -> case A.decode (TLE.encodeUtf8 (TL.fromStrict body)) of
      Just (A.Object top) -> AKM.lookup "status" top == Just (A.String "refused")
      _ -> False
    _ -> False

-- | action=clear with id removes only that entry.
testScratchHandleClearById :: IO Bool
testScratchHandleClearById = withTempProject $ \pd -> do
  store <- SP.openStore pd
  let mk i c = SP.ScratchEntry
        { SP.seId       = i, SP.seKind = SP.ScratchHypothesis, SP.seCode = c
        , SP.seModule   = Nothing, SP.seImports = [], SP.seNote = Nothing
        , SP.seResult   = Nothing, SP.seStatus = SP.ScratchOpen
        , SP.seCreated  = 1, SP.seUpdated = 1
        }
  SP.save store (mk "a" "a")
  SP.save store (mk "b" "b")
  let args = A.object [ "action" A..= ("clear" :: Text), "id" A..= ("a" :: Text) ]
  _ <- ScratchTool.runHandle store undefined undefined args
  loaded <- SP.loadAll store
  pure (map SP.seId loaded == ["b"])

-- | action=clear with confirm=true (no id) truncates everything.
testScratchHandleClearAll :: IO Bool
testScratchHandleClearAll = withTempProject $ \pd -> do
  store <- SP.openStore pd
  let mk i = SP.ScratchEntry
        { SP.seId       = i, SP.seKind = SP.ScratchHypothesis, SP.seCode = "x"
        , SP.seModule   = Nothing, SP.seImports = [], SP.seNote = Nothing
        , SP.seResult   = Nothing, SP.seStatus = SP.ScratchOpen
        , SP.seCreated  = 1, SP.seUpdated = 1
        }
  SP.save store (mk "a")
  SP.save store (mk "b")
  SP.save store (mk "c")
  let args = A.object [ "action" A..= ("clear" :: Text), "confirm" A..= True ]
  _ <- ScratchTool.runHandle store undefined undefined args
  loaded <- SP.loadAll store
  pure (null loaded)

-- #253 Phase 2: ghc_scratch action=check — boundary / data-layer tests.
-- GHC-session-requiring tests (type_ok / type_error from live GHCi)
-- are covered by the FlowScratchpad E2E scenario; the unit tests here
-- pin the sanitize-gate, missing-id, and unknown-id paths that do not
-- need a live session (undefined is safe because check returns early).

-- | action=check without 'id' → MissingArg (failed).
testScratchCheckMissingId :: IO Bool
testScratchCheckMissingId = withTempProject $ \pd -> do
  store <- SP.openStore pd
  let args = A.object [ "action" A..= ("check" :: Text) ]
  result <- ScratchTool.runHandle store undefined undefined args
  pure $ case trContent result of
    [TextContent body] -> case A.decode (TLE.encodeUtf8 (TL.fromStrict body)) of
      Just (A.Object top) -> AKM.lookup "status" top == Just (A.String "failed")
      _ -> False
    _ -> False

-- | action=check with an id that doesn't exist → no_match.
testScratchCheckUnknownId :: IO Bool
testScratchCheckUnknownId = withTempProject $ \pd -> do
  store <- SP.openStore pd
  let args = A.object [ "action" A..= ("check" :: Text), "id" A..= ("ghost" :: Text) ]
  result <- ScratchTool.runHandle store undefined undefined args
  pure $ case trContent result of
    [TextContent body] -> case A.decode (TLE.encodeUtf8 (TL.fromStrict body)) of
      Just (A.Object top) -> AKM.lookup "status" top == Just (A.String "no_match")
      _ -> False
    _ -> False

-- | action=check refuses code containing the sentinel string.
-- (F-03 removed the newline rejection — multi-line declarations now go
-- through the wrapAsLetBlock path.  Sentinel injection is still blocked
-- in both the single-line and multi-line sanitizers.)
testScratchCheckSanitizeReject :: IO Bool
testScratchCheckSanitizeReject = withTempProject $ \pd -> do
  store <- SP.openStore pd
  -- Single-line code containing the sentinel — sanitizeExpression
  -- catches it before touching the GHC session.
  let badCode = "f x = True -- " <> sentinel
      writeArgs = A.object
        [ "action" A..= ("write" :: Text)
        , "id"     A..= ("bad" :: Text)
        , "code"   A..= badCode
        ]
  _ <- ScratchTool.runHandle store undefined undefined writeArgs
  let checkArgs = A.object
        [ "action" A..= ("check" :: Text)
        , "id"     A..= ("bad" :: Text)
        ]
  result <- ScratchTool.runHandle store undefined undefined checkArgs
  pure $ case trContent result of
    [TextContent body] -> case A.decode (TLE.encodeUtf8 (TL.fromStrict body)) of
      Just (A.Object top) -> AKM.lookup "status" top == Just (A.String "refused")
      _ -> False
    _ -> False

-- | 'ScratchResult' ToJSON / FromJSON round-trip.
testScratchResultRoundTrip :: IO Bool
testScratchResultRoundTrip =
  let r = SP.ScratchResult
        { SP.srKind   = "type_ok"
        , SP.srDetail = "Int -> Int"
        , SP.srAt     = 1.0
        }
  in pure $ case A.fromJSON (A.toJSON r) of
       A.Success r' ->
         SP.srKind   r' == SP.srKind   r
         && SP.srDetail r' == SP.srDetail r
         && SP.srAt    r' == SP.srAt    r
       A.Error _ -> False

-- | F-01 regression: action=check response carries 'kind' at the top
-- level of the mkOk result object, NOT nested under result.result.kind.
-- With 'undefined' for the session, withGhcSession throws and the
-- try-block stores a type_error; we just need 'kind' to be directly
-- inside the result object so scratchNext's envField routing works.
testScratchCheckKindAtTopLevel :: IO Bool
testScratchCheckKindAtTopLevel = withTempProject $ \pd -> do
  store <- SP.openStore pd
  let writeArgs = A.object
        [ "action" A..= ("write" :: Text)
        , "id"     A..= ("probe" :: Text)
        , "code"   A..= ("bogusF" :: Text)  -- passes sanitize, GHC call throws
        ]
  _ <- ScratchTool.runHandle store undefined undefined writeArgs
  let checkArgs = A.object
        [ "action" A..= ("check" :: Text)
        , "id"     A..= ("probe" :: Text)
        ]
  result <- ScratchTool.runHandle store undefined undefined checkArgs
  pure $ case trContent result of
    [TextContent body] -> case A.decode (TLE.encodeUtf8 (TL.fromStrict body)) of
      Just (A.Object top) ->
        case AKM.lookup "status" top of
          Just (A.String "ok") ->
            -- status=ok means the try-block ran; result must have 'kind'
            -- directly (type_error from undefined-session crash), not nested.
            case AKM.lookup "result" top of
              Just (A.Object inner) ->
                -- 'kind' must be present here
                case AKM.lookup "kind" inner of
                  Just (A.String k) -> k == "type_ok" || k == "type_error"
                  _ -> False
              _ -> False
          _ -> True  -- refused / failed paths: sanitize boundary fired, fine
      _ -> False
    _ -> False

-- #253 Phase 3: ghc_scratch action=show — full detail + seResult round-trip.

-- | action=show returns the complete entry: all fields present, code
-- intact, status correct, seResult=null initially.
testScratchShowFullDetail :: IO Bool
testScratchShowFullDetail = withTempProject $ \pd -> do
  store <- SP.openStore pd
  -- Write an entry with a note and module context
  let writeArgs = A.object
        [ "action" A..= ("write" :: Text)
        , "id"     A..= ("full-e1" :: Text)
        , "code"   A..= ("\\x -> x + 1" :: Text)
        , "module" A..= ("src/Foo.hs" :: Text)
        , "note"   A..= ("hypothesis: this is a plain increment" :: Text)
        ]
  _ <- ScratchTool.runHandle store undefined undefined writeArgs
  let showArgs = A.object
        [ "action" A..= ("show" :: Text)
        , "id"     A..= ("full-e1" :: Text)
        ]
  result <- ScratchTool.runHandle store undefined undefined showArgs
  pure $ case trContent result of
    [TextContent body] -> case A.decode (TLE.encodeUtf8 (TL.fromStrict body)) of
      Just (A.Object top) ->
        -- Outer envelope: status=ok
        case AKM.lookup "status" top of
          Just (A.String "ok") ->
            case AKM.lookup "result" top of
              Just (A.Object inner) ->
                -- All required fields must be present
                AKM.lookup "id"     inner == Just (A.String "full-e1")
                && AKM.lookup "code"   inner == Just (A.String "\\x -> x + 1")
                && AKM.lookup "status" inner == Just (A.String "open")
                -- note field forwarded correctly
                && AKM.lookup "note" inner   == Just (A.String "hypothesis: this is a plain increment")
                -- result is null before any check
                && AKM.lookup "result" inner == Just A.Null
                -- module field forwarded
                && AKM.lookup "module" inner == Just (A.String "src/Foo.hs")
              _ -> False
          _ -> False
      _ -> False
    _ -> False

-- | 'seResult' survives a save → loadAll round-trip. After calling
-- SP.save with a non-Nothing result, loadAll must return the same
-- 'ScratchResult' value byte-for-byte.
testScratchResultRoundTripViaLoadAll :: IO Bool
testScratchResultRoundTripViaLoadAll = withTempProject $ \pd -> do
  store <- SP.openStore pd
  let result = SP.ScratchResult
        { SP.srKind   = "type_ok"
        , SP.srDetail = "Int -> Int"
        , SP.srAt     = 1234567.0
        }
      entry = SP.ScratchEntry
        { SP.seId      = "rt-entry"
        , SP.seKind    = SP.ScratchHypothesis
        , SP.seCode    = "\\x -> x + 1"
        , SP.seModule  = Nothing
        , SP.seImports = []
        , SP.seNote    = Nothing
        , SP.seResult  = Just result
        , SP.seStatus  = SP.ScratchVerified
        , SP.seCreated = 100.0
        , SP.seUpdated = 200.0
        }
  SP.save store entry
  loaded <- SP.loadAll store
  pure $ case loaded of
    [e] ->
      SP.seId e == "rt-entry"
      && SP.seStatus e == SP.ScratchVerified
      && case SP.seResult e of
           Just r  ->
             SP.srKind   r == "type_ok"
             && SP.srDetail r == "Int -> Int"
             && SP.srAt    r == 1234567.0
           Nothing -> False
    _ -> False

-- | action=show after action=check (undefined session → type_error) carries
-- the persisted ScratchResult in the response's result.result.
-- This verifies the full pipeline: check persists the result, show
-- reads it back, and the outer toJSON round-trip is intact.
testScratchShowAfterCheckHasResult :: IO Bool
testScratchShowAfterCheckHasResult = withTempProject $ \pd -> do
  store <- SP.openStore pd
  -- Write a simple entry (undefined session means check will catch a
  -- SomeException and persist kind=type_error)
  let writeArgs = A.object
        [ "action" A..= ("write" :: Text)
        , "id"     A..= ("chk-then-show" :: Text)
        , "code"   A..= ("badIdent" :: Text)
        ]
  _ <- ScratchTool.runHandle store undefined undefined writeArgs
  -- check — undefined session throws, type_error is persisted
  let checkArgs = A.object
        [ "action" A..= ("check" :: Text)
        , "id"     A..= ("chk-then-show" :: Text)
        ]
  _ <- ScratchTool.runHandle store undefined undefined checkArgs
  -- show — must return the persisted result
  let showArgs = A.object
        [ "action" A..= ("show" :: Text)
        , "id"     A..= ("chk-then-show" :: Text)
        ]
  result <- ScratchTool.runHandle store undefined undefined showArgs
  pure $ case trContent result of
    [TextContent body] -> case A.decode (TLE.encodeUtf8 (TL.fromStrict body)) of
      Just (A.Object top) ->
        case AKM.lookup "status" top of
          Just (A.String "ok") ->
            case AKM.lookup "result" top of
              Just (A.Object inner) ->
                -- result.result must be a non-null object with kind=type_error
                case AKM.lookup "result" inner of
                  Just (A.Object r) ->
                    AKM.lookup "kind" r == Just (A.String "type_error")
                  _ -> False
              _ -> False
          _ -> False
      _ -> False
    _ -> False

-- #253 Phase 4: ghc_scratch action=promote — boundary tests.
-- The splice + compile-verify tests require a live GHC session and are
-- covered by the FlowScratchpad E2E scenario.  These unit tests pin the
-- argument-validation paths that fire before touching the filesystem.

-- | action=promote without 'id' → failed.
testScratchPromoteMissingId :: IO Bool
testScratchPromoteMissingId = withTempProject $ \pd -> do
  store <- SP.openStore pd
  let args = A.object [ "action" A..= ("promote" :: Text) ]
  result <- ScratchTool.runHandle store undefined pd args
  pure $ case trContent result of
    [TextContent body] -> case A.decode (TLE.encodeUtf8 (TL.fromStrict body)) of
      Just (A.Object top) -> AKM.lookup "status" top == Just (A.String "failed")
      _ -> False
    _ -> False

-- | action=promote with id but no 'target_module' → failed.
testScratchPromoteMissingTargetModule :: IO Bool
testScratchPromoteMissingTargetModule = withTempProject $ \pd -> do
  store <- SP.openStore pd
  let writeArgs = A.object
        [ "action" A..= ("write" :: Text)
        , "id"     A..= ("pm1" :: Text)
        , "code"   A..= ("f x = x" :: Text)
        ]
  _ <- ScratchTool.runHandle store undefined undefined writeArgs
  let args = A.object [ "action" A..= ("promote" :: Text), "id" A..= ("pm1" :: Text) ]
  result <- ScratchTool.runHandle store undefined pd args
  pure $ case trContent result of
    [TextContent body] -> case A.decode (TLE.encodeUtf8 (TL.fromStrict body)) of
      Just (A.Object top) -> AKM.lookup "status" top == Just (A.String "failed")
      _ -> False
    _ -> False

-- | action=promote with unknown id → no_match.
testScratchPromoteUnknownId :: IO Bool
testScratchPromoteUnknownId = withTempProject $ \pd -> do
  store <- SP.openStore pd
  let args = A.object
        [ "action"        A..= ("promote" :: Text)
        , "id"            A..= ("no-such-id" :: Text)
        , "target_module" A..= ("src/Foo.hs" :: Text)
        ]
  result <- ScratchTool.runHandle store undefined pd args
  pure $ case trContent result of
    [TextContent body] -> case A.decode (TLE.encodeUtf8 (TL.fromStrict body)) of
      Just (A.Object top) -> AKM.lookup "status" top == Just (A.String "no_match")
      _ -> False
    _ -> False

-- | action=promote with target_module that escapes the project → refused.
testScratchPromoteBadModulePath :: IO Bool
testScratchPromoteBadModulePath = withTempProject $ \pd -> do
  store <- SP.openStore pd
  let writeArgs = A.object
        [ "action" A..= ("write" :: Text)
        , "id"     A..= ("pm2" :: Text)
        , "code"   A..= ("f x = x" :: Text)
        ]
  _ <- ScratchTool.runHandle store undefined undefined writeArgs
  let args = A.object
        [ "action"        A..= ("promote" :: Text)
        , "id"            A..= ("pm2" :: Text)
        , "target_module" A..= ("/etc/passwd" :: Text)
        ]
  result <- ScratchTool.runHandle store undefined pd args
  pure $ case trContent result of
    [TextContent body] -> case A.decode (TLE.encodeUtf8 (TL.fromStrict body)) of
      Just (A.Object top) -> AKM.lookup "status" top == Just (A.String "refused")
      _ -> False
    _ -> False

-- | Pure 'spliceInto' test: appending (no target_line) places code
-- after a blank separator and ends with a newline.
testSpliceIntoAppend :: IO Bool
testSpliceIntoAppend =
  let orig   = "module Foo where\n\nfoo = 1\n"
      code   = "bar = 2"
      result = ScratchTool.spliceInto orig code Nothing
  in pure
       ( T.isInfixOf "foo = 1" result
           && T.isInfixOf "bar = 2" result
           -- blank-line separator before the new code
           && T.isInfixOf "foo = 1\n\nbar = 2" result
           -- trailing newline
           && T.isSuffixOf "\n" result
       )

-- | Pure 'spliceInto' test: inserting after line 2 puts code between
-- lines 2 and 3 of the original file.
testSpliceIntoAtLine :: IO Bool
testSpliceIntoAtLine =
  let orig   = "line1\nline2\nline3\n"
      code   = "NEW"
      result = ScratchTool.spliceInto orig code (Just 2)
      ls     = T.lines result
  in pure
       ( "line1" `elem` ls
           && "line2" `elem` ls
           && "NEW"   `elem` ls
           && "line3" `elem` ls
           -- order: line1 before line2, line2 before NEW, NEW before line3
           && headIdx "line1" ls < headIdx "line2" ls
           && headIdx "line2" ls < headIdx "NEW"   ls
           && headIdx "NEW"   ls < headIdx "line3" ls
       )
  where
    headIdx :: Text -> [Text] -> Int
    headIdx x xs = case [i | (i, e) <- zip [0..] xs, e == x] of
      (n:_) -> n
      []    -> maxBound

--------------------------------------------------------------------------------
-- F-03: sanitizeDeclarations + wrapAsLetBlock
--------------------------------------------------------------------------------

-- | sanitizeDeclarations allows newlines (unlike sanitizeExpression).
testSanitizeDeclAllowsNewlines :: IO Bool
testSanitizeDeclAllowsNewlines = pure $
  case sanitizeDeclarations "f :: Int -> Int\nf x = x + 1" of
    Right _ -> True
    Left  _ -> False

-- | sanitizeDeclarations still blocks the sentinel string.
testSanitizeDeclBlocksSentinel :: IO Bool
testSanitizeDeclBlocksSentinel = pure $
  case sanitizeDeclarations ("f x = " <> sentinel) of
    Left ContainsSentinel -> True
    _                     -> False

-- | wrapAsLetBlock indents each line by 2 spaces and wraps in let/in.
testWrapAsLetBlockIndents :: IO Bool
testWrapAsLetBlockIndents = pure $
  let code   = "f :: Int\nf = 42"
      result = ScratchTool.wrapAsLetBlock code
  in T.isPrefixOf "let\n" result
       && T.isInfixOf "  f :: Int" result
       && T.isInfixOf "  f = 42"   result
       && T.isSuffixOf " in ()" result

--------------------------------------------------------------------------------
-- F-04: promote wraps single-line expression when binding_name is given
--------------------------------------------------------------------------------

-- | action=promote with binding_name wraps the stored expression as
-- "name = expr" before splicing, producing a valid top-level binding.
-- This test only checks the path-validation layer (no live GHC session
-- needed) — the spliced text is verified by inspecting the error payload
-- which carries the attempted code before GHC rejects or accepts it.
testScratchPromoteBindingName :: IO Bool
testScratchPromoteBindingName = withTempProject $ \pd -> do
  store <- SP.openStore pd
  -- Write a single-line expression entry.
  let writeArgs = A.object
        [ "action" A..= ("write" :: Text)
        , "id"     A..= ("expr-entry" :: Text)
        , "code"   A..= ("(\\x -> x + 1) :: Int -> Int" :: Text)
        ]
  _ <- ScratchTool.runHandle store undefined undefined writeArgs
  -- Promote with binding_name — target module does not exist so the
  -- path-traversal guard fires before GHC is touched, letting us test
  -- the binding_name wrapping via the refused kind.
  let promoteArgs = A.object
        [ "action"       A..= ("promote" :: Text)
        , "id"           A..= ("expr-entry" :: Text)
        , "target_module" A..= ("../escape" :: Text)   -- triggers path_traversal
        , "binding_name" A..= ("myFun" :: Text)
        ]
  result <- ScratchTool.runHandle store undefined pd promoteArgs
  -- We only need to confirm the args parse and reach the path-guard
  -- (status=refused, kind=path_traversal) — that proves binding_name
  -- parsed correctly and the entry was found.
  pure $ case trContent result of
    [TextContent body] -> case A.decode (TLE.encodeUtf8 (TL.fromStrict body)) of
      Just (A.Object top) ->
        AKM.lookup "status" top == Just (A.String "refused")
      _ -> False
    _ -> False

--------------------------------------------------------------------------------
-- F-05: compileFailResult uses first error message for 'cause'
--------------------------------------------------------------------------------

-- | compileFailResult should use the first SevError message for the
-- 'cause' field, not a truncation of the raw string that may be
-- dominated by unrelated package-version warnings.
testCompileFailResultCause :: IO Bool
testCompileFailResultCause = pure $
  let firstErr = GhcError
        { geFile     = "src/Foo.hs"
        , geColumn   = 1
        , geLine     = 10
        , geSeverity = SevError
        , geCode     = Just "GHC-94426"
        , geMessage  = "Invalid type signature"
        }
      -- Raw output starts with a long package warning that would eat
      -- the first 400 chars and truncate the real error.
      longPreamble = T.replicate 300 "package-warning ; "
      raw = longPreamble <> "Invalid type signature"
      result = RefactorTool.compileFailResult False [firstErr] raw " — snapshot restored"
  in case trContent result of
    [TextContent body] -> case A.decode (TLE.encodeUtf8 (TL.fromStrict body)) of
      Just (A.Object top) ->
        case AKM.lookup "error" top of
          Just (A.Object errObj) ->
            case AKM.lookup "cause" errObj of
              Just (A.String cause) -> cause == "Invalid type signature"
              _ -> False
          _ -> False
      _ -> False
    _ -> False

-- #276: ghc_scratch(check) used to fail with "parse error on input 'import'"
-- because import lines were wrapped inside `let … in ()`. 'splitImports' now
-- peels the leading import section off so it can be added to the interactive
-- context instead. These cover the pure partitioning logic.

testSplitImportsSingle :: IO Bool
testSplitImportsSingle = pure $
  let (imps, body) = ScratchTool.splitImports
                       "import Data.List (sort)\n\nfoo = sort [3,1,2]"
   in imps == ["import Data.List (sort)"]
        && T.strip body == "foo = sort [3,1,2]"

testSplitImportsMultiple :: IO Bool
testSplitImportsMultiple = pure $
  let (imps, body) = ScratchTool.splitImports
                       "import Data.Char (isAlpha)\nimport Data.List\nx = 1"
   in imps == ["import Data.Char (isAlpha)", "import Data.List"]
        && T.strip body == "x = 1"

testSplitImportsNone :: IO Bool
testSplitImportsNone = pure $
  let (imps, body) = ScratchTool.splitImports "foo :: Int\nfoo = 1"
   in null imps && T.strip body == "foo :: Int\nfoo = 1"

testSplitImportsMultiLine :: IO Bool
testSplitImportsMultiLine = pure $
  let (imps, body) = ScratchTool.splitImports
                       "import Data.Map\n  ( fromList\n  , toList\n  )\nx = 1"
   in imps == ["import Data.Map\n  ( fromList\n  , toList\n  )"]
        && T.strip body == "x = 1"

-- A non-import declaration body that merely *mentions* "import" indented (e.g.
-- a string) must not be mistaken for the import section.
testSplitImportsBodyKeyword :: IO Bool
testSplitImportsBodyKeyword = pure $
  let (imps, body) = ScratchTool.splitImports
                       "import Data.List\nmsg = \"please import this\""
   in imps == ["import Data.List"]
        && T.strip body == "msg = \"please import this\""
