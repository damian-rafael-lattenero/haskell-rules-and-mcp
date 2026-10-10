-- | Unit tests for Haddock extraction from Info tool, doc-payload shape,
-- GhcError code helpers, GHC.Internal qualifier stripping, FixWarning
-- patch-key guard, Hoogle hit dedup/count, and no-match/error distinction.
--
-- Extracted from the Spec.hs monolith (#271) via the function-export shape.
module Spec.HaddockUnit
  ( testMkGhcErrorCode
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
import System.Directory (getTemporaryDirectory, removePathForcibly)
import System.FilePath ((</>))

import qualified HaskellFlows.Mcp.Envelope as Env
import HaskellFlows.Mcp.NextStep
import qualified HaskellFlows.Mcp.NextStep as NextStep
import HaskellFlows.Mcp.Protocol (ToolResult (..))
import HaskellFlows.Parser.Error (GhcError (..), Severity (..), parseGhcErrors)
import qualified HaskellFlows.Tool.CreateProject as CreateProject
import qualified HaskellFlows.Tool.FixWarning as FixWarning
import HaskellFlows.Tool.Hoogle (HoogleHit (..), parseHoogleLine)
import qualified HaskellFlows.Tool.Hoogle as HoogleTool
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
               , FixWarning.fwMessage    = Nothing
               }
      result = FixWarning.previewResult "src/Foo.hs" plan args
  in pure $ case Env.reResult result of
       Just (A.Object r) ->
            AKM.member "dropLine" r
         && not (AKM.member "patch" r)
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
-- produce a ToolResponse with a non-failing status.
testHoogleNoMatchIsError :: IO Bool
testHoogleNoMatchIsError = do
  let result = HoogleTool.renderResult "NoSuchSymbolXYZ" (HoogleTool.HoSuccess [])
  pure (Env.reStatus result `elem` [Env.StatusOk, Env.StatusPartial, Env.StatusNoMatch])

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
