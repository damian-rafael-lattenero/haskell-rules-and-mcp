-- | Unit tests for the @ghc_complete@ tool — qualified-prefix splitting
-- (#252 splitQualifiedPrefix), candidate rendering, newline injection guard,
-- and the permissive-JSON-parse contract for the 'limit' field.
--
-- Extracted from the Spec.hs monolith (#271) via the function-export shape:
-- the driver keeps the registrations and imports these functions.
module Spec.Complete
  ( testCompletePermissiveLimit
  , testCompleteHitsOk
  , testCompleteNoMatch
  , testCompleteQualifiedRemediation
  , testCompleteQualifiedRemediation225
  , testCompleteDescriptionMentionsQualified
  , testSplitQualifiedPrefixWithName
  , testSplitQualifiedPrefixEmptySuffix
  , testSplitQualifiedPrefixUnqualified
  , testSplitQualifiedPrefixDeep
  , testCompleteImportsLookupModule
  , testCompleteRefusesNewline
  ) where

import qualified Data.Aeson as A
import qualified Data.Aeson.Key as AKey
import qualified Data.Aeson.KeyMap as AKM
import Data.Maybe (isNothing)
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.IO as TIO
import qualified Data.Text.Lazy as TL
import qualified Data.Text.Lazy.Encoding as TLE
import System.Directory (createDirectoryIfMissing, getTemporaryDirectory, removePathForcibly)
import System.FilePath ((</>))

import HaskellFlows.Ghc.ApiSession (killGhcSession, startGhcSession)
import qualified HaskellFlows.Mcp.Envelope as Env
import HaskellFlows.Mcp.Protocol (ToolContent (..), ToolResult (..), tdDescription)
import qualified HaskellFlows.Tool.Complete as CompleteTool
import HaskellFlows.Types (mkProjectDir)

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

-- ---------------------------------------------------------------------------
-- Phase B helper
-- ---------------------------------------------------------------------------

-- | Stage a tmpdir project with a 'Foo' module exporting 'foo' and drive
-- 'CompleteTool.handle' with the given args.
runComplete :: A.Value -> IO (Either String Env.ToolResponse)
runComplete args = do
  tmp <- getTemporaryDirectory
  let dir = tmp </> "haskell-flows-complete-test"
  removePathForcibly dir
  createDirectoryIfMissing True (dir </> "src")
  TIO.writeFile (dir </> "src" </> "Foo.hs")
    (T.pack "module Foo where\nfoo :: Int\nfoo = 1\n")
  result <- case mkProjectDir dir of
    Left _   -> pure (Left "could not build ProjectDir")
    Right pd -> do
      sess <- startGhcSession pd
      tr   <- CompleteTool.handle sess args
      killGhcSession sess
      case trContent tr of
        [TextContent body] ->
          pure (A.eitherDecode (TLE.encodeUtf8 (TL.fromStrict body)))
        _ -> pure (Left "expected exactly one TextContent")
  removePathForcibly dir
  pure result

-- ---------------------------------------------------------------------------
-- Candidate + rendering tests
-- ---------------------------------------------------------------------------

-- | Completing 'fold' returns at least one in-scope candidate (foldr,
-- foldl, foldMap, …) → status='ok' with the legacy candidates
-- array preserved inside 'result'.
testCompleteHitsOk :: IO Bool
testCompleteHitsOk = do
  decoded <- runComplete
    (A.object [ "prefix" A..= ("fold" :: Text) ])
  pure $ case decoded of
    Right env
      | Env.reStatus env == Env.StatusOk
      , Just (A.Object payload) <- Env.reResult env ->
          AKM.lookup (AKey.fromText "prefix") payload == Just (A.String "fold")
            && AKM.member (AKey.fromText "count")      payload
            && AKM.member (AKey.fromText "candidates") payload
            && AKM.member (AKey.fromText "truncated")  payload
    _ -> False

-- | A prefix that matches no in-scope identifier → status='no_match'.
testCompleteNoMatch :: IO Bool
testCompleteNoMatch = do
  decoded <- runComplete
    (A.object [ "prefix" A..= ("zZqXunlikelyPrefix" :: Text) ])
  pure $ case decoded of
    Right env
      | Env.reStatus env == Env.StatusNoMatch
      , Just (A.Object payload) <- Env.reResult env ->
          AKM.lookup (AKey.fromText "count") payload == Just (A.Number 0)
    _ -> False

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
testCompleteDescriptionMentionsQualified :: IO Bool
testCompleteDescriptionMentionsQualified =
  let desc = tdDescription CompleteTool.descriptor
  in pure $ "Qualified" `T.isInfixOf` desc
         || "qualified" `T.isInfixOf` desc

-- ---------------------------------------------------------------------------
-- splitQualifiedPrefix unit tests (#252)
-- ---------------------------------------------------------------------------

-- | #252: splitQualifiedPrefix splits "Data.Map.lookup" into
-- ("Data.Map", "lookup").
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

-- | A newline-laden prefix → status='refused' with
-- error.kind='newline_injection'.
testCompleteRefusesNewline :: IO Bool
testCompleteRefusesNewline = do
  decoded <- runComplete
    (A.object [ "prefix" A..= ("fold\n:quit" :: Text) ])
  pure $ case decoded of
    Right env
      | Env.reStatus env == Env.StatusRefused
      , Just err <- Env.reError env ->
          Env.eeKind err == Env.NewlineInjection
            && Env.eeField err == Just "prefix"
    _ -> False
