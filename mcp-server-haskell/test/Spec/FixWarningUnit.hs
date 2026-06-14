-- | Unit tests for 'Tool.FixWarning' plan helpers (#66111, #40910, #235),
-- underscore-prefix token, tail-binding patch, sig patch, stored-property
-- null-module rendering (#238), and warning categorization.
--
-- Extracted from the Spec.hs monolith (#271) via the function-export shape.
module Spec.FixWarningUnit
  ( testFixWarningUnusedImports
  , testFixPlanFixable66111
  , testFixPlanNotFixable40910
  , testFixPlanWithNamePromotes
  , testUnderscorePrefixToken
  , testUnderscorePrefixWordBoundary
  , testUnderscorePrefixIdempotent
  , testPatchTailBindings202
  , testFixWarningOutOfBounds
  , testIsTypeSigLine235
  , testPatchPrecedingTypeSig235
  , testPatchPrecedingTypeSigSkips235
  , testPatchPrecedingTypeSigNoOp235
  , testWritePatchedAlsoFixesSig235
  , testRenderStoredNullModuleHint238
  , testRenderStoredJustModuleNoHint238
  , testListResultNullModuleCount238
  , testListResultNoNullFields238
  , testEnhanceNullModuleDetail238
  , testEnhanceNullModuleDetailNoOp238
  , testWarningCategorize
  , testWarningBucketize
  ) where

import qualified Data.Aeson as A
import qualified Data.Aeson.Key as AKey
import qualified Data.Aeson.KeyMap as AKM
import qualified Data.Text as T
import qualified Data.Text.IO as TIO
import qualified Data.Text.Lazy as TL
import qualified Data.Text.Lazy.Encoding as TLE
import Data.Text (Text)
import Data.Maybe (isNothing)
import System.Directory (createDirectoryIfMissing, getTemporaryDirectory, removePathForcibly)
import System.FilePath ((</>))

import HaskellFlows.Parser.Error
  ( GhcError (..)
  , Severity (..)
  , WarningCategory (..)
  , bucketize
  , categorizeWarning
  )
import qualified HaskellFlows.Mcp.Envelope as Env
import HaskellFlows.Mcp.Protocol (ToolContent (..), ToolResult (..))
import HaskellFlows.Data.PropertyStore (StoredProperty (..))
import HaskellFlows.Types (mkProjectDir)
import qualified HaskellFlows.Tool.FixWarning as FixWarning
import Spec.ToolEnvFixture (pdEnv)
import HaskellFlows.Tool.Regression (renderStored)
import qualified HaskellFlows.Tool.PropertyStore as PropertyStore
import qualified HaskellFlows.Tool.PropertyAudit as PropertyAuditTool
import qualified HaskellFlows.Tool.Regression as RegTool

import Spec.Helpers (withTempProject)

testFixWarningUnusedImports :: IO Bool
testFixWarningUnusedImports =
  let plan = FixWarning.planForCode "GHC-66111"
  in pure $ FixWarning.fpDrop plan
         && T.isInfixOf "unused import" (T.toLower (FixWarning.fpHint plan))

-- | Issue #55: 'fixable' is the machine-readable signal that
-- replaces \"read the prose hint\". GHC-66111 has a deterministic
-- drop-the-line patch → fixable=True.
testFixPlanFixable66111 :: IO Bool
testFixPlanFixable66111 =
  let plan = FixWarning.planForCode "GHC-66111"
  in pure $ FixWarning.fpFixable plan
         && FixWarning.fpDrop plan

-- | Issue #55: GHC-40910 with NO name → no concrete patch
-- (the tool can't guess which binding the warning meant).
-- fixable=False so the agent knows to fix by hand.
testFixPlanNotFixable40910 :: IO Bool
testFixPlanNotFixable40910 =
  let plan = FixWarning.planForCode "GHC-40910"
  in pure $ not (FixWarning.fpFixable plan)
         && not (FixWarning.fpDrop plan)
         && isNothing (FixWarning.fpPatch plan)

-- | Issue #55: GHC-40910 WITH a binding name → 'planForCodeWithName'
-- promotes the plan to fixable=True with a concrete patch line that
-- prefixes the name with an underscore.
testFixPlanWithNamePromotes :: IO Bool
testFixPlanWithNamePromotes =
  let srcLine = "combineSorted xs ys = sort (xs ++ _holeArg)"
      plan    = FixWarning.planForCodeWithName "GHC-40910" (Just "ys") srcLine
  in pure $ FixWarning.fpFixable plan
         && case FixWarning.fpPatch plan of
              Just patched ->
                patched == "combineSorted xs _ys = sort (xs ++ _holeArg)"
              Nothing -> False

-- | Issue #55 — 'underscorePrefix' core: replace a free word-
-- boundary occurrence of the binding name with @_<name>@.
testUnderscorePrefixToken :: IO Bool
testUnderscorePrefixToken =
  let line = "f x ys = x + 1"
  in pure $
    FixWarning.underscorePrefix "ys" line == Just "f x _ys = x + 1"

-- | Issue #55: must NOT match substrings — 'ysx' or 'tys' don't
-- count as the binding 'ys'.
testUnderscorePrefixWordBoundary :: IO Bool
testUnderscorePrefixWordBoundary =
  let line = "process xys = xys + 1  -- 'ys' is not a token here"
  in pure (isNothing (FixWarning.underscorePrefix "ys" line))

-- | Issue #55: a name already underscore-prefixed → no patch.
-- Prevents double-underscoring on retries.
testUnderscorePrefixIdempotent :: IO Bool
testUnderscorePrefixIdempotent =
  let line = "f x _ys = x"
  in pure (isNothing (FixWarning.underscorePrefix "ys" line))

-- | Issue #202: patchTailBindings renames binding equations that
-- immediately follow the sig line, fixing the GHC-44432 breakage
-- that occurred when only the sig was patched.
testPatchTailBindings202 :: IO Bool
testPatchTailBindings202 = pure $
  -- typical: sig + single equation
     FixWarning.patchTailBindings "unusedBinding"
       [ "unusedBinding = 42"
       , ""
       , "greet x = x"
       ]
       == [ "_unusedBinding = 42"
          , ""
          , "greet x = x"
          ]
  -- multiple equations (pattern matching)
  && FixWarning.patchTailBindings "f"
       [ "f 0 = 1"
       , "f n = n + 1"
       , ""
       ]
       == [ "_f 0 = 1"
          , "_f n = n + 1"
          , ""
          ]
  -- stops at first non-matching line
  && FixWarning.patchTailBindings "foo"
       [ "bar = 1"
       , "foo = 2"
       ]
       == [ "bar = 1"
          , "foo = 2"
          ]
  -- already prefixed: underscorePrefix returns Nothing → passthrough
  && FixWarning.patchTailBindings "unusedBinding"
       [ "_unusedBinding = 42" ]
       == [ "_unusedBinding = 42" ]

-- | Issue #221: applying a fix to a line beyond the file's end must
-- return a validation error, not a silent no-op with applied=true.
testFixWarningOutOfBounds :: IO Bool
testFixWarningOutOfBounds = do
  tmp <- getTemporaryDirectory
  let dir  = tmp </> "haskell-flows-issue-221"
      file = dir </> "Fixture.hs"
  removePathForcibly dir
  createDirectoryIfMissing True dir
  TIO.writeFile file "module Fixture where\n\nfoo = 1\n"   -- 3 lines
  result <- case mkProjectDir dir of
    Left _   -> pure False
    Right pd -> do
      let args = A.object
            [ "module_path" A..= ("Fixture.hs" :: Text)
            , "line"        A..= (999 :: Int)
            , "code"        A..= ("GHC-66111" :: Text)
            , "apply"       A..= True
            ]
      tr <- FixWarning.handle (pdEnv pd) args
      pure $ case decodeToolResult tr of
        Right env ->
             Env.reStatus env == Env.StatusFailed
          && maybe False
               (\e ->  Env.eeKind e == Env.Validation
                    && T.isInfixOf "out of bounds" (Env.eeMessage e))
               (Env.reError env)
        Left _ -> False
  removePathForcibly dir
  pure result

--------------------------------------------------------------------------------
-- Issue #235 — patchPrecedingTypeSig
--------------------------------------------------------------------------------

testIsTypeSigLine235 :: IO Bool
testIsTypeSigLine235 = pure $
     FixWarning.isTypeSigLine "listSum" "listSum :: [Int] -> Int"
  && FixWarning.isTypeSigLine "listSum" "listSum :: [Int] -> Int  -- comment"
  && not (FixWarning.isTypeSigLine "listSum" "listSum = foldr add 0")
  && not (FixWarning.isTypeSigLine "listSum" "listSumHelper :: Int")
  && not (FixWarning.isTypeSigLine "listSum" "-- | listSum :: ignored")

-- | #235: patchPrecedingTypeSig renames a type sig in the list.
testPatchPrecedingTypeSig235 :: IO Bool
testPatchPrecedingTypeSig235 = pure $
  FixWarning.patchPrecedingTypeSig "listSum"
    [ "-- | Sum of a list."
    , "listSum :: [Int] -> Int"
    ]
  == [ "-- | Sum of a list."
     , "_listSum :: [Int] -> Int"
     ]

-- | #235: patchPrecedingTypeSig skips blank lines and Haddock comments.
testPatchPrecedingTypeSigSkips235 :: IO Bool
testPatchPrecedingTypeSigSkips235 = pure $
  FixWarning.patchPrecedingTypeSig "foo"
    [ "foo :: Int -> Int"
    , ""
    , "-- inner comment"
    , ""
    ]
  == [ "_foo :: Int -> Int"
     , ""
     , "-- inner comment"
     , ""
     ]

-- | #235: no-op when no type sig precedes the binding.
testPatchPrecedingTypeSigNoOp235 :: IO Bool
testPatchPrecedingTypeSigNoOp235 = pure $
  FixWarning.patchPrecedingTypeSig "bar"
    [ "foo = 1"    -- different name — not a type sig for 'bar'
    , "bar = 2"
    ]
  == [ "foo = 1"
     , "bar = 2"
     ]

-- | #235: writePatched with GHC-40910 on a binding line must ALSO
-- rename the preceding type signature to avoid GHC-44432.
testWritePatchedAlsoFixesSig235 :: IO Bool
testWritePatchedAlsoFixesSig235 = do
  tmp <- getTemporaryDirectory
  let dir  = tmp </> "hf-test235-type-sig"
      file = dir </> "Fixture.hs"
  removePathForcibly dir
  createDirectoryIfMissing True dir
  -- A module where listSum is defined but unused (no exports/callers).
  TIO.writeFile file $ T.unlines
    [ "module Fixture where"
    , ""
    , "-- | Sum of a list."
    , "listSum :: [Int] -> Int"      -- line 4: type sig
    , "listSum = foldr (+) 0"         -- line 5: binding (GHC warns here)
    ]
  result <- case mkProjectDir dir of
    Left _   -> pure False
    Right pd -> do
      let args = A.object
            [ "module_path" A..= ("Fixture.hs" :: Text)
            , "line"        A..= (5 :: Int)   -- GHC-40910 reported on binding
            , "code"        A..= ("GHC-40910" :: Text)
            , "name"        A..= ("listSum" :: Text)
            , "apply"       A..= True
            ]
      _ <- FixWarning.handle (pdEnv pd) args
      patched <- TIO.readFile file
      let lns = T.lines patched
      -- Both type sig (line 4, ix 3) and binding (line 5, ix 4) must be renamed.
      pure $  T.isPrefixOf "_listSum ::" (lns !! 3)
           && T.isPrefixOf "_listSum ="  (lns !! 4)
  removePathForcibly dir
  pure result

-- | #238: renderStored should emit a 'module_hint' field when spModule is Nothing.
testRenderStoredNullModuleHint238 :: IO Bool
testRenderStoredNullModuleHint238 = pure $
  let sp = StoredProperty
             { spExpression = "\\x -> x == x"
             , spModule     = Nothing
             , spPassed     = 3
             , spUpdated    = 1_000_000
             , spCases     = 0
             }
      A.Object km = RegTool.renderStored sp
  in AKM.member "module_hint" km

-- | #238: renderStored should NOT emit a 'module_hint' field when spModule is Just.
testRenderStoredJustModuleNoHint238 :: IO Bool
testRenderStoredJustModuleNoHint238 = pure $
  let sp = StoredProperty
             { spExpression = "\\x -> x == x"
             , spModule     = Just "src/Foo.hs"
             , spPassed     = 3
             , spUpdated    = 1_000_000
             , spCases     = 0
             }
      A.Object km = RegTool.renderStored sp
  in not (AKM.member "module_hint" km)

-- | #238: listResult should emit null_module_count and null_module_hint fields
-- when the property store contains at least one null-module property.
testListResultNullModuleCount238 :: IO Bool
testListResultNullModuleCount238 = do
  let spNull = StoredProperty
                 { spExpression = "\\x -> x >= 0"
                 , spModule     = Nothing
                 , spPassed     = 1
                 , spUpdated    = 1_000_000
                 , spCases     = 0
                 }
      spOk   = StoredProperty
                 { spExpression = "\\x -> x == x"
                 , spModule     = Just "src/Foo.hs"
                 , spPassed     = 5
                 , spUpdated    = 1_000_000
                 , spCases     = 0
                 }
      tr     = RegTool.listResult [spNull, spOk]
  case trContent tr of
    [TextContent body] ->
      case A.eitherDecode (TLE.encodeUtf8 (TL.fromStrict body)) of
        Right env ->
          case Env.reResult env of
            Just (A.Object km) ->
              pure $ AKM.member "null_module_count" km
                  && AKM.member "null_module_hint"  km
                  && AKM.lookup "null_module_count" km == Just (A.Number 1)
            _ -> pure False
        Left _ -> pure False
    _ -> pure False

-- | #238: listResult should NOT emit null_module_count/hint when all props have modules.
testListResultNoNullFields238 :: IO Bool
testListResultNoNullFields238 = do
  let sp = StoredProperty
             { spExpression = "\\x -> x == x"
             , spModule     = Just "src/Foo.hs"
             , spPassed     = 2
             , spUpdated    = 1_000_000
             , spCases     = 0
             }
      tr = RegTool.listResult [sp]
  case trContent tr of
    [TextContent body] ->
      case A.eitherDecode (TLE.encodeUtf8 (TL.fromStrict body)) of
        Right env ->
          case Env.reResult env of
            Just (A.Object km) ->
              pure $ not (AKM.member "null_module_count" km)
                  && not (AKM.member "null_module_hint"  km)
            _ -> pure False
        Left _ -> pure False
    _ -> pure False

-- | #238: enhanceNullModuleDetail appends a hint when status=skipped,
-- detail starts with "probe load/parse failure", and p1 has no module.
testEnhanceNullModuleDetail238 :: IO Bool
testEnhanceNullModuleDetail238 = pure $
  let result = PropertyAuditTool.enhanceNullModuleDetail
                 True False
                 "skipped"
                 "probe load/parse failure: (no GHCi output)"
  in "re-run it via" `T.isInfixOf` result
  && "module: null" `T.isInfixOf` result

-- | #238: enhanceNullModuleDetail is a no-op when both properties have modules.
testEnhanceNullModuleDetailNoOp238 :: IO Bool
testEnhanceNullModuleDetailNoOp238 = pure $
  let original = "probe load/parse failure: (no GHCi output)"
      result   = PropertyAuditTool.enhanceNullModuleDetail
                   False False "skipped" original
  in result == original

-- | Phase 11i: warning categorizer buckets common messages into
-- the 5 coarse classes the agent can prioritise on.
testWarningCategorize :: IO Bool
testWarningCategorize = pure $
     cat "Defined but not used: `foo'"           == WcUnused
  && cat "Pattern match(es) are non-exhaustive"  == WcNonExhaustive
  && cat "This binding for `x' shadows"          == WcShadowing
  && cat "Top-level binding with no type signature: foo :: Int -> Int"
                                                  == WcMissingSig
  && cat "Something else entirely"                == WcOther
  where
    cat msg = categorizeWarning GhcError
      { geFile = "Foo.hs", geLine = 1, geColumn = 1
      , geSeverity = SevWarning, geCode = Nothing, geMessage = msg
      }

-- | Phase 11i: bucketize returns (category, count) pairs ordered
-- by count descending, so agents reading the head triage first.
testWarningBucketize :: IO Bool
testWarningBucketize =
  let mk msg = GhcError
        { geFile = "Foo.hs", geLine = 1, geColumn = 1
        , geSeverity = SevWarning, geCode = Nothing, geMessage = msg
        }
      errs =
        [ mk "Defined but not used: x"
        , mk "Defined but not used: y"
        , mk "Defined but not used: z"
        , mk "Pattern match(es) are non-exhaustive"
        , mk "This binding shadows"
        ]
      buckets = bucketize errs
  in pure $ case buckets of
       ((WcUnused, 3) : _) -> True
       _                   -> False

--------------------------------------------------------------------------------
-- local helper (mirrors Spec.Helpers.decodeToolResult)
--------------------------------------------------------------------------------

decodeToolResult :: ToolResult -> Either String Env.ToolResponse
decodeToolResult tr = case trContent tr of
  [TextContent body] ->
    A.eitherDecode (TLE.encodeUtf8 (TL.fromStrict body))
  _ -> Left "expected exactly one TextContent"
