-- | Unit tests for @ghc_inspect(action=goto)@ payload shaping —
-- InFile/InModule payload shapes (#117), compiled-module remediation
-- (#214), qualifiedPreloadPayload (#224).
--
-- The session-bound handler died with the ApiSession backend (W6.8);
-- the resolution behavior it used to pin is now covered end-to-end by
-- the e2e suite against the ghcide route. What remains here pins the
-- pure payload layer shared with 'HaskellFlows.Tool.IdeBacked'.
module Spec.Goto
  ( testGotoLibraryNameNoMatch
  , testGotoFileHasLocation
  , testGotoCompiledModuleRemediation
  , testGotoQualifiedPreloadPayload
  ) where

import qualified Data.Aeson as A
import qualified Data.Aeson.Key as AKey
import qualified Data.Aeson.KeyMap as AKM
import qualified Data.Text as T

import qualified HaskellFlows.Tool.Goto as GotoTool

-- ---------------------------------------------------------------------------
-- Pure locationPayload tests (#117 / #214 / #224)
-- ---------------------------------------------------------------------------

-- | #117: when 'queryLocation' resolves a name to an 'InModule' location
-- (library name with no local source file), 'locationPayload' must include
-- @has_location: false@ and a remediation hint.
testGotoLibraryNameNoMatch :: IO Bool
testGotoLibraryNameNoMatch =
  let loc = GotoTool.InModule "GHC.Base"
      payload = GotoTool.locationPayload "fmap" loc
  in case payload of
       A.Object o ->
         let hasLoc  = AKM.lookup "has_location" o == Just (A.Bool False)
             hasRem  = case AKM.lookup "remediation" o of
                         Just (A.String t) -> not (T.null t)
                         _                 -> False
             hasKind = AKM.lookup "kind" o == Just (A.String "module")
         in pure (hasLoc && hasRem && hasKind)
       _ -> pure False

-- | #117: an 'InFile' location must carry @has_location: true@.
testGotoFileHasLocation :: IO Bool
testGotoFileHasLocation =
  let loc = GotoTool.InFile "src/Foo.hs" 10 5
      payload = GotoTool.locationPayload "myFn" loc
  in case payload of
       A.Object o ->
         pure (AKM.lookup "has_location" o == Just (A.Bool True))
       _ -> pure False

-- | #214: the InModule remediation message must NOT say "no local source
-- file" because compiled project modules DO have a local source file —
-- they're simply compiled. The message must use "was compiled" instead.
testGotoCompiledModuleRemediation :: IO Bool
testGotoCompiledModuleRemediation =
  let loc = GotoTool.InModule "Scratch"
      payload = GotoTool.locationPayload "greet" loc
  in case payload of
       A.Object o ->
         case AKM.lookup "remediation" o of
           Just (A.String t) ->
             let hasCompiled   = "was compiled" `T.isInfixOf` t
                 noFalseSource = not ("no local source file" `T.isInfixOf` t)
             in pure (hasCompiled && noFalseSource)
           _ -> pure False
       _ -> pure False

-- | #224: qualifiedPreloadPayload names the unqualified form and the
-- module prefix so the agent knows how to retry without the qualifier.
testGotoQualifiedPreloadPayload :: IO Bool
testGotoQualifiedPreloadPayload =
  let loc = GotoTool.InModule "GHC.Internal.Data.OldList"
      payload = GotoTool.qualifiedPreloadPayload "Data.List.sort" "sort" loc
  in case payload of
       A.Object o ->
         case AKM.lookup "remediation" o of
           Just (A.String t) ->
             let mentionsSort = "'sort'" `T.isInfixOf` t
                 mentionsMod  = "'Data.List'" `T.isInfixOf` t
                 mentionsQual = "qualified" `T.isInfixOf` t
             in pure (mentionsSort && mentionsMod && mentionsQual)
           _ -> pure False
       _ -> pure False
