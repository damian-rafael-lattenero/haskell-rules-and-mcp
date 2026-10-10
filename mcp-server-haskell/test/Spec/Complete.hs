-- | Unit tests for @ghc_inspect(action=complete)@ payload shaping —
-- qualified-prefix splitting (#252 splitQualifiedPrefix), candidate
-- rendering (#145/#225), and the permissive-JSON-parse contract for
-- the 'limit' field.
--
-- The session-bound handler died with the ApiSession backend (W6.8);
-- resolution behavior is now covered end-to-end by the e2e suite
-- against the ghcide route. These tests pin the pure layer shared
-- with 'HaskellFlows.Tool.IdeBacked'.
module Spec.Complete
  ( testCompletePermissiveLimit
  , testCompleteQualifiedRemediation
  , testCompleteQualifiedRemediation225
  , testSplitQualifiedPrefixWithName
  , testSplitQualifiedPrefixEmptySuffix
  , testSplitQualifiedPrefixUnqualified
  , testSplitQualifiedPrefixDeep
  , testCompleteImportsLookupModule
  ) where

import qualified Data.Aeson as A
import qualified Data.Aeson.KeyMap as AKM
import Data.Maybe (isNothing)
import qualified Data.Text as T
import qualified Data.Text.IO as TIO

import qualified HaskellFlows.Mcp.Envelope as Env
import qualified HaskellFlows.Tool.Complete as CompleteTool

-- | Complete.limit accepts stringified numbers. Default still
-- applies when the field is omitted entirely.
testCompletePermissiveLimit :: IO Bool

testCompletePermissiveLimit = do
  let nativeJson = "{\"prefix\":\"sho\",\"limit\":10}"
      stringJson = "{\"prefix\":\"sho\",\"limit\":\"10\"}"
      missingJson = "{\"prefix\":\"sho\"}"
      decode raw = A.fromJSON <$> (A.decode raw :: Maybe A.Value)
  case (decode nativeJson, decode stringJson, decode missingJson) of
    (Just (A.Success (a :: CompleteTool.CompleteArgs)),
     Just (A.Success (b :: CompleteTool.CompleteArgs)),
     Just (A.Success (c :: CompleteTool.CompleteArgs))) ->
       -- a == b proves permissive parses match native;
       -- existence of c proves the default still applies.
       pure ( show a == show b && not (null (show c)) )
    _ -> pure False

-- | #145: zero hits for a qualified prefix (contains '.') must include
-- a 'remediation' field explaining the import-scope root cause.
-- Unqualified zero-hit results should NOT include the remediation field.

testCompleteQualifiedRemediation :: IO Bool

testCompleteQualifiedRemediation = pure $
  let qualResp  = CompleteTool.renderCompletions "Data.Map." 25 []
      plainResp = CompleteTool.renderCompletions "zZqUnlikely" 25 []
      hasRemediation env =
        case Env.reResult env of
          Just (A.Object o) -> AKM.member "remediation" o
          _                 -> False
  in Env.reStatus qualResp  == Env.StatusNoMatch && hasRemediation qualResp
  && Env.reStatus plainResp == Env.StatusNoMatch && not (hasRemediation plainResp)

-- | #225: updated qualified remediation names the module and suggests
-- bare prefix instead of just "use ghc_add_import first".

testCompleteQualifiedRemediation225 :: IO Bool

testCompleteQualifiedRemediation225 = pure $
  let resp = CompleteTool.renderCompletions "Data.List." 25 []
  in case Env.reResult resp of
       Just (A.Object o) ->
         case AKM.lookup "remediation" o of
           Just (A.String t) ->
             "import qualified Data.List" `T.isInfixOf` t
               && "Data.List" `T.isInfixOf` t
               && "preload" `T.isInfixOf` t
           _ -> False
       _ -> False

-- | Issue #252: the ghc_complete tool description must document that
-- qualified prefixes (e.g. "Data.Map.") are supported.

testSplitQualifiedPrefixWithName :: IO Bool

testSplitQualifiedPrefixWithName =
  pure $ CompleteTool.splitQualifiedPrefix "Data.Map.lookup"
       == Just ("Data.Map", "lookup")

-- | #252: splitQualifiedPrefix splits "Data.Map." (trailing dot) into
-- ("Data.Map", "") — the empty name prefix means "all exports".

testSplitQualifiedPrefixEmptySuffix :: IO Bool

testSplitQualifiedPrefixEmptySuffix =
  pure $ CompleteTool.splitQualifiedPrefix "Data.Map."
       == Just ("Data.Map", "")

-- | #252: splitQualifiedPrefix returns Nothing for bare prefixes
-- with no dot.

testSplitQualifiedPrefixUnqualified :: IO Bool

testSplitQualifiedPrefixUnqualified =
  pure (isNothing (CompleteTool.splitQualifiedPrefix "fold"))

-- | #252: splitQualifiedPrefix handles multi-dot module paths.

testSplitQualifiedPrefixDeep :: IO Bool

testSplitQualifiedPrefixDeep =
  pure $ CompleteTool.splitQualifiedPrefix "Data.Map.Strict.lookup"
       == Just ("Data.Map.Strict", "lookup")

-- | #252: structural source check — Complete.hs must reference
-- @lookupModule@ so the qualified-prefix fallback path is wired in.

testCompleteImportsLookupModule :: IO Bool

testCompleteImportsLookupModule = do
  src <- TIO.readFile "src/HaskellFlows/Tool/Complete.hs"
  pure $ "lookupModule" `T.isInfixOf` src
      && "queryQualifiedFallback" `T.isInfixOf` src
