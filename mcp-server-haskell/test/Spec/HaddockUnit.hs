-- | Unit tests for Haddock extraction from Info tool, doc-payload shape,
-- GhcError code helpers, GHC.Internal qualifier stripping, FixWarning
-- patch-key guard, Hoogle hit dedup/count, and no-match/error distinction.
--
-- Extracted from the Spec.hs monolith (#271) via the function-export shape.
module Spec.HaddockUnit
  ( testExtractHaddockFindsDoc
  , testExtractHaddockNoHaddock
  , testExtractHaddockNoComment
  , testExtractHaddockAboveTypeSig
  , testNoDocInScopePayloadShape
  , testHasDocFalseDirectly
  , testMkGhcErrorCode
  , testStripGhcInternalQual
  , testStripGhcInternalQualMulti
  , testStripGhcInternalQualNoop
  , testFixWarnNoPatchKey
  , testRemediationToolName
  , testHoogleHitName
  , testHoogleDedup
  , testNoMatchIsNotFailing
  , testHoogleNoMatchIsError
  , testHoogleCountAlias
  ) where

import qualified Data.Aeson as A
import Data.Aeson (Value, object, (.=))
import qualified Data.Aeson.Key as AKey
import qualified Data.Aeson.KeyMap as AKM
import qualified Data.List as List
import Data.Maybe (isNothing)
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.IO as TIO
import qualified Data.Text.Lazy as TL
import qualified Data.Text.Lazy.Encoding as TLE
import System.Directory (getTemporaryDirectory, removePathForcibly, createDirectoryIfMissing)
import System.FilePath ((</>))

import qualified HaskellFlows.Ghc.ApiSession as ApiSession
import HaskellFlows.Ghc.ApiSession (startGhcSession, killGhcSession)
import qualified HaskellFlows.Mcp.Envelope as Env
import HaskellFlows.Mcp.NextStep
import qualified HaskellFlows.Mcp.NextStep as NextStep
import HaskellFlows.Mcp.Protocol (ToolContent (..), ToolResult (..))
import HaskellFlows.Parser.Error (GhcError (..), Severity (..), parseGhcErrors)
import qualified HaskellFlows.Tool.CreateProject as CreateProject
import qualified HaskellFlows.Tool.Doc as DocTool
import qualified HaskellFlows.Tool.FixWarning as FixWarning
import HaskellFlows.Tool.Hoogle (HoogleHit (..), parseHoogleLine)
import qualified HaskellFlows.Tool.Hoogle as HoogleTool
import qualified HaskellFlows.Tool.Info as InfoTool
import HaskellFlows.Types (mkProjectDir)

import Spec.Helpers (runToolEnvelope, withTempProject)

testExtractHaddockFindsDoc :: IO Bool
testExtractHaddockFindsDoc = do
  tmp <- getTemporaryDirectory
  let path = tmp </> "haskell-flows-103-haddock1.hs"
  TIO.writeFile path "-- | Compute the identity of a value.\nidentity :: a -> a\nidentity x = x\n"
  result <- DocTool.extractHaddockAbove path 2  -- defLine=2 is the type sig line
  removePathForcibly path
  pure $ case result of
    Just txt -> T.isInfixOf "Compute the identity" txt
    Nothing  -> False

-- | A plain @--@ comment (without @|@) is NOT a Haddock block.
testExtractHaddockNoHaddock :: IO Bool
testExtractHaddockNoHaddock = do
  tmp <- getTemporaryDirectory
  let path = tmp </> "haskell-flows-103-haddock2.hs"
  TIO.writeFile path "-- Not a haddock comment\nfoo :: Int -> Int\nfoo x = x\n"
  result <- DocTool.extractHaddockAbove path 2
  removePathForcibly path
  pure (isNothing result)

-- | No comment at all above the definition → 'Nothing'.
testExtractHaddockNoComment :: IO Bool
testExtractHaddockNoComment = do
  tmp <- getTemporaryDirectory
  let path = tmp </> "haskell-flows-103-haddock3.hs"
  TIO.writeFile path "foo :: Int -> Int\nfoo x = x\n"
  result <- DocTool.extractHaddockAbove path 1
  removePathForcibly path
  pure (isNothing result)

--------------------------------------------------------------------------------
-- Issue #195 — extractHaddockAbove type-sig skip + nextStep routing
--------------------------------------------------------------------------------

-- | #195: 'extractHaddockAbove' must find a @-- |@ comment that sits
-- above a type-signature line, which in turn sits above the binding.
-- GHC reports the binding line (not the sig line) as @defLine@, so
-- the scanner must skip the sig line before collecting the comment.
testExtractHaddockAboveTypeSig :: IO Bool
testExtractHaddockAboveTypeSig = do
  tmp <- getTemporaryDirectory
  let path = tmp </> "haskell-flows-195-typesig.hs"
  TIO.writeFile path $ T.unlines
    [ "-- | Greet a person."
    , "greet :: String -> String"
    , "greet name = \"Hello, \" <> name <> \"!\""
    ]
  -- defLine = 3 (the binding line)
  result <- DocTool.extractHaddockAbove path 3
  removePathForcibly path
  pure $ case result of
    Just txt -> "Greet a person" `T.isInfixOf` txt
    Nothing  -> False

-- | #195: 'noDocInScopePayload' must have @hasDoc=false@,
-- @found_in_scope=true@, and a non-empty @reason@.
testNoDocInScopePayloadShape :: IO Bool
testNoDocInScopePayloadShape =
  case DocTool.noDocInScopePayload "greet" of
    A.Object o ->
      let hasDoc      = AKM.lookup "hasDoc"         o == Just (A.Bool False)
          foundInScope = AKM.lookup "found_in_scope" o == Just (A.Bool True)
          hasReason   = case AKM.lookup "reason" o of
                          Just (A.String r) -> not (T.null r)
                          _                 -> False
      in pure (hasDoc && foundInScope && hasReason)
    _ -> pure False

-- | #195: 'hasDocFalse' must return 'True' when fed a payload whose
-- @hasDoc@ field is @false@. This is a direct probe of the helper
-- so that if 'testDocNoDocNextStepIsInfo' fails we can distinguish
-- "helper broken" from "dispatch guard ordering wrong".
testHasDocFalseDirectly :: IO Bool
testHasDocFalseDirectly =
  let payloadFalse = DocTool.noDocInScopePayload "greet"
      payloadTrue  = DocTool.hasDocPayload "greet" "some doc"
      falsePayload = object [ "hasDoc" .= False ]
      truePayload  = object [ "hasDoc" .= True  ]
  in pure $  NextStep.hasDocFalse payloadFalse
          && not (NextStep.hasDocFalse payloadTrue)
          && NextStep.hasDocFalse falsePayload
          && not (NextStep.hasDocFalse truePayload)

-- Issue #106 sub-findings
--------------------------------------------------------------------------------

-- | F-14: 'parseGhcErrors' should populate 'geCode' when the header line
-- contains @[GHC-XXXXX]@. This verifies the regex capture group is correct.
-- The captureHook fix populates geCode for the GHC-API path; the same
-- 'geCode' field is what categorizeWarning branches on.
testMkGhcErrorCode :: IO Bool
testMkGhcErrorCode =
  let raw = T.unlines
        [ "src/Foo.hs:5:1: warning: [GHC-66111] [-Wunused-imports]"
        , "    The import of 'Data.List' is redundant"
        ]
  in pure $ case parseGhcErrors raw of
       [e] -> geCode e == Just "GHC-66111"
           && geSeverity e == SevWarning
       _   -> False

--------------------------------------------------------------------------------
-- #180 — stripGhcInternalQual
--------------------------------------------------------------------------------

-- | #180: GHC 9.12 leaks 'ghc-internal-VERSION:GHC.Internal.*' prefixes into
-- diagnostics. 'stripGhcInternalQual' must remove them, leaving the public
-- module path.
testStripGhcInternalQual :: IO Bool
testStripGhcInternalQual =
  let raw = "No instance for 'ghc-internal-9.1202.0:GHC.Internal.Data.String.IsString Int'"
      want = "No instance for 'Data.String.IsString Int'"
  in pure (ApiSession.stripGhcInternalQual raw == want)

-- | #180: multiple occurrences in one message must all be stripped.
testStripGhcInternalQualMulti :: IO Bool
testStripGhcInternalQualMulti =
  let raw  = "ghc-internal-9.1202.0:GHC.Internal.Enum and ghc-internal-9.1202.0:GHC.Internal.Show"
      want = "Enum and Show"
  in pure (ApiSession.stripGhcInternalQual raw == want)

-- | #180: text without internal package qualifications is left unchanged.
testStripGhcInternalQualNoop :: IO Bool
testStripGhcInternalQualNoop =
  let raw = "No instance for 'Data.String.IsString Int'"
  in pure (ApiSession.stripGhcInternalQual raw == raw)

-- | F-17: 'previewResult' for a dropLine plan must omit the @patch@ key
-- entirely rather than emitting @\"patch\": null@. Agents that branch on
-- key presence (not null vs. absent) were getting confused.
testFixWarnNoPatchKey :: IO Bool
testFixWarnNoPatchKey =
  let plan = FixWarning.planForCode "GHC-66111"
      args = FixWarning.FixWarningArgs
               { FixWarning.fwModulePath = "src/Foo.hs"
               , FixWarning.fwLine       = 3
               , FixWarning.fwCode       = "GHC-66111"
               , FixWarning.fwApply      = False
               , FixWarning.fwName       = Nothing
               }
      result = FixWarning.previewResult "src/Foo.hs" plan args
  in pure $ case trContent result of
       [TextContent body] ->
         case A.decode (TLE.encodeUtf8 (TL.fromStrict body)) of
           Just (A.Object topEnv) ->
             case AKM.lookup "result" topEnv of
               Just (A.Object r) ->
                    AKM.member "dropLine" r
                 && not (AKM.member "patch" r)
               _ -> False
           _ -> False
       _ -> False

-- | F-07: error remediation strings must reference the consolidated
-- @ghc_project(action=\"create\")@ surface, not the retired
-- @ghc_create_project@ tool name.
testRemediationToolName :: IO Bool
testRemediationToolName = do
  let scaffold = CreateProject.sourceFile "Foo"
  pure $ not (T.isInfixOf "ghc_create_project" scaffold)

-- | F-20: 'parseHoogleLine' must populate 'hhName' with the
-- function/type name extracted from the LHS (the token after the
-- module prefix). Previously the name was captured and discarded.
testHoogleHitName :: IO Bool
testHoogleHitName =
  let line = "Prelude filter :: (a -> Bool) -> [a] -> [a]"
  in pure $ case parseHoogleLine line of
       Just h  -> hhName h == Just "filter"
       Nothing -> False

-- | F-19: 'hitsPayload' must deduplicate hits by (module, signature)
-- so that Hoogle returning the same entry twice (e.g. once per package
-- variant) doesn't inflate the count. Test the predicate directly with
-- 'List.nubBy' on constructed hits.
testHoogleDedup :: IO Bool
testHoogleDedup =
  let mk m nm sig = HoogleHit { hhModule = m, hhName = nm, hhSignature = sig }
      h1 = mk (Just "Data.List") (Just "sort") "Ord a => [a] -> [a]"
      h2 = mk (Just "Data.List") (Just "sort") "Ord a => [a] -> [a]"  -- duplicate
      h3 = mk (Just "Data.Set")  (Just "toList") "Set a -> [a]"
      sameHit a b = hhModule a == hhModule b && hhSignature a == hhSignature b
      unique = List.nubBy sameHit [h1, h2, h3]
  in pure (length unique == 2)

--------------------------------------------------------------------------------
-- Issue #139 — StatusNoMatch must not set isError
--------------------------------------------------------------------------------

-- | #139: 'isFailingStatus StatusNoMatch' must return False so that
-- 'toolResponseToResult' sets @isError=false@ on no-result hoogle responses.
testNoMatchIsNotFailing :: IO Bool
testNoMatchIsNotFailing = do
  let result = Env.toolResponseToResult (Env.mkNoMatch (A.object []))
  pure (not (trIsError result))

-- | #139: the full hoogle renderResult path for an empty hit list must
-- produce a ToolResult with isError=false.
testHoogleNoMatchIsError :: IO Bool
testHoogleNoMatchIsError = do
  let result = HoogleTool.renderResult "NoSuchSymbolXYZ" (HoogleTool.HoSuccess [])
  pure (not (trIsError result))

-- | #158: 'FromJSON HoogleArgs' must accept "count" as a synonym for
-- "limit" — both field names are in common LLM use. The schema declares
-- additionalProperties=false but the 'FromJSON' instance must honour
-- both aliases rather than silently dropping the unknown "count" key.
testHoogleCountAlias :: IO Bool
testHoogleCountAlias =
  -- Parse args with "count" key instead of "limit"
  case A.fromJSON (A.object ["query" A..= ("map" :: Text), "count" A..= (3 :: Int)]) of
    A.Error _   -> pure False  -- must parse successfully
    A.Success (HoogleTool.HoogleArgs { HoogleTool.haQuery = q, HoogleTool.haLimit = lim }) ->
      pure (q == "map" && lim == 3)
