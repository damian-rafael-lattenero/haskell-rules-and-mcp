-- | Unit tests for 'Tool.Type' and 'Tool.Eval' (#90 Phase B): valid-expr
-- ok, ill-typed failed, newline/sentinel/oversized-int guards, and
-- import-prefix redirect (#143).
--
-- Extracted from the Spec.hs monolith (#271) via the function-export shape.
module Spec.TypeEval
  ( testTypeValidExprOk
  , testTypeIllTypedFailed
  , testTypeRefusesNewline
  , testTypeNotInScope
  , testEvalPureExprOk
  , testEvalImportRedirect
  , testEvalRefusesNewline
  , testEvalRefusesSentinel
  , testEvalRefusesOversizedInteger
  , testEvalRefusesLargeLiteral
  , testClassifyEvalExceptionInSource
  ) where

import qualified Data.Aeson as A
import qualified Data.Aeson.Key as AKey
import qualified Data.Aeson.KeyMap as AKM
import qualified Data.Text as T
import qualified Data.Text.IO as TIO
import qualified Data.Text.Lazy as TL
import qualified Data.Text.Lazy.Encoding as TLE
import System.Directory (createDirectoryIfMissing, getTemporaryDirectory, removePathForcibly)
import System.FilePath ((</>))

import qualified HaskellFlows.Mcp.Envelope as Env
import HaskellFlows.Mcp.Protocol (ToolContent (..), ToolResult (..))
import HaskellFlows.Types (mkProjectDir)
import HaskellFlows.Ghc.ApiSession (startGhcSession, killGhcSession)
import qualified HaskellFlows.Tool.Type as TypeTool
import qualified HaskellFlows.Tool.Eval as EvalTool

import Spec.Helpers (decodeToolResult, runToolEnvelope)

-- | Phase B helper: stage a tmpdir project + drive 'TypeTool.handle'.
runType :: A.Value -> IO (Either String Env.ToolResponse)
runType args = do
  tmp <- getTemporaryDirectory
  let dir = tmp </> "haskell-flows-type-test"
  removePathForcibly dir
  createDirectoryIfMissing True (dir </> "src")
  TIO.writeFile (dir </> "src" </> "Foo.hs")
    (T.pack "module Foo where\nfoo :: Int\nfoo = 1\n")
  result <- case mkProjectDir dir of
    Left _   -> pure (Left "could not build ProjectDir")
    Right pd -> do
      sess <- startGhcSession pd
      tr   <- TypeTool.handle sess args
      killGhcSession sess
      case trContent tr of
        [TextContent body] ->
          pure (A.eitherDecode (TLE.encodeUtf8 (TL.fromStrict body)))
        _ -> pure (Left "expected exactly one TextContent")
  removePathForcibly dir
  pure result

-- | 'ghc_type' on a valid Prelude expression resolves cleanly →
-- status='ok' with result.{expression, type}. The exact rendering
-- of the type varies by GHC minor (forall + brackets, etc.) so we
-- only assert structure.
testTypeValidExprOk :: IO Bool
testTypeValidExprOk = do
  decoded <- runType (A.object [ "expression" A..= ("id" :: T.Text) ])
  pure $ case decoded of
    Right env
      | Env.reStatus env == Env.StatusOk
      , Just (A.Object payload) <- Env.reResult env ->
          AKM.lookup (AKey.fromText "expression") payload == Just (A.String "id")
            && AKM.member (AKey.fromText "type") payload
    _ -> False

-- | An ill-typed expression → status='failed' with kind='type_error'.
-- Pre-#90 this returned a free-form 'expression did not type-check
-- — <SDoc>' string; post-#90 the SDoc lives in error.cause and
-- the message stays short.
testTypeIllTypedFailed :: IO Bool
testTypeIllTypedFailed = do
  decoded <- runType (A.object [ "expression" A..= ("True + 1" :: T.Text) ])
  pure $ case decoded of
    Right env
      | Env.reStatus env == Env.StatusFailed
      , Just err <- Env.reError env ->
          Env.eeKind err == Env.TypeError
    _ -> False

-- | Newline in expression → status='refused' with
-- kind='newline_injection'.
testTypeRefusesNewline :: IO Bool
testTypeRefusesNewline = do
  decoded <- runType (A.object [ "expression" A..= ("id\n:quit" :: T.Text) ])
  pure $ case decoded of
    Right env
      | Env.reStatus env == Env.StatusRefused
      , Just err <- Env.reError env ->
          Env.eeKind err == Env.NewlineInjection
            && Env.eeField err == Just "expression"
    _ -> False

-- | #141: an expression whose name is not in scope → status='failed'
-- with kind='not_in_scope', not kind='type_error'.
-- 'Data.Map.fromList' is not imported by default so asking for its
-- type triggers GHC's "Not in scope" diagnostic.
testTypeNotInScope :: IO Bool
testTypeNotInScope = do
  decoded <- runType (A.object [ "expression" A..= ("Data.Map.fromList" :: T.Text) ])
  pure $ case decoded of
    Right env
      | Env.reStatus env == Env.StatusFailed
      , Just err <- Env.reError env ->
          Env.eeKind err == Env.NotInScope
    _ -> False

-- | Phase B helper: stage a tmpdir + drive 'EvalTool.handle'.
runEval :: A.Value -> IO (Either String Env.ToolResponse)
runEval args = do
  tmp <- getTemporaryDirectory
  let dir = tmp </> "haskell-flows-eval-test"
  removePathForcibly dir
  createDirectoryIfMissing True (dir </> "src")
  TIO.writeFile (dir </> "src" </> "Foo.hs")
    (T.pack "module Foo where\nfoo :: Int\nfoo = 1\n")
  result <- case mkProjectDir dir of
    Left _   -> pure (Left "could not build ProjectDir")
    Right pd -> do
      sess <- startGhcSession pd
      tr   <- EvalTool.handle sess args
      killGhcSession sess
      case trContent tr of
        [TextContent body] ->
          pure (A.eitherDecode (TLE.encodeUtf8 (TL.fromStrict body)))
        _ -> pure (Left "expected exactly one TextContent")
  removePathForcibly dir
  pure result

-- | 'ghc_eval' on a pure expression returns its show-rendered
-- output → status='ok' with result.{output, truncated}.
testEvalPureExprOk :: IO Bool
testEvalPureExprOk = do
  decoded <- runEval (A.object [ "expression" A..= ("1 + 1" :: T.Text) ])
  pure $ case decoded of
    Right env
      | Env.reStatus env == Env.StatusOk
      , Just (A.Object payload) <- Env.reResult env ->
          AKM.lookup (AKey.fromText "output") payload == Just (A.String "2")
            && AKM.lookup (AKey.fromText "truncated") payload == Just (A.Bool False)
    _ -> False

-- | #143: an expression that starts with 'import ' is not a valid
-- Haskell expression — redirect to ghc_add_import with a structured
-- error + nextStep. Checks without a live GhcSession (the redirect
-- fires before sanitization and GHC loading).
testEvalImportRedirect :: IO Bool
testEvalImportRedirect =
  let resp = EvalTool.importRedirectResult "import Data.Map"
  in pure $ Env.reStatus resp == Env.StatusFailed
          && case Env.reError resp of
               Just err -> Env.eeKind err == Env.CompileError
               Nothing  -> False
          && case Env.reNextStep resp of
               Just (A.Object ns) ->
                 AKM.lookup "tool" ns == Just (A.String "ghc_add_import")
               _ -> False

-- | Newline in expression → status='refused' with
-- kind='newline_injection'.
testEvalRefusesNewline :: IO Bool
testEvalRefusesNewline = do
  decoded <- runEval (A.object [ "expression" A..= ("1 + 1\n:quit" :: T.Text) ])
  pure $ case decoded of
    Right env
      | Env.reStatus env == Env.StatusRefused
      , Just err <- Env.reError env ->
          Env.eeKind err == Env.NewlineInjection
            && Env.eeField err == Just "expression"
    _ -> False

-- | Sentinel string in expression → status='refused' with
-- kind='sentinel_poisoning'. Anchors the security gate that
-- prevents an attacker-controlled prompt from desyncing the
-- framing protocol.
testEvalRefusesSentinel :: IO Bool
testEvalRefusesSentinel = do
  decoded <- runEval
    (A.object [ "expression" A..= ("\"<<<GHCi-DONE-7f3a2b>>>\"" :: T.Text) ])
  pure $ case decoded of
    Right env
      | Env.reStatus env == Env.StatusRefused
      , Just err <- Env.reError env ->
          Env.eeKind err == Env.SentinelPoisoning
            && Env.eeField err == Just "expression"
    _ -> False

-- | #127: power-expression with exponent >= 64 → status='refused' with
-- kind='oversized_input'. Guards against the two-limb GMP Integer that
-- crashes the in-process GHC evaluator via an RTS segfault.
testEvalRefusesOversizedInteger :: IO Bool
testEvalRefusesOversizedInteger = do
  decoded <- runEval (A.object [ "expression" A..= ("2^64" :: T.Text) ])
  pure $ case decoded of
    Right env
      | Env.reStatus env == Env.StatusRefused
      , Just err <- Env.reError env ->
          Env.eeKind err == Env.OversizedInput
            && Env.eeField err == Just "expression"
    _ -> False

-- | #127: 20-digit decimal literal → status='refused' with
-- kind='oversized_input'. 18446744073709551616 is 2^64.
testEvalRefusesLargeLiteral :: IO Bool
testEvalRefusesLargeLiteral = do
  decoded <- runEval
    (A.object [ "expression" A..= ("18446744073709551616" :: T.Text) ])
  pure $ case decoded of
    Right env
      | Env.reStatus env == Env.StatusRefused
      , Just err <- Env.reError env ->
          Env.eeKind err == Env.OversizedInput
            && Env.eeField err == Just "expression"
    _ -> False

-- | #134: the source must contain both the 'GhcException' and 'SourceError'
-- pattern guards in 'classifyEvalException', confirming compile-time errors
-- are routed to CompileError rather than RuntimeException.
testClassifyEvalExceptionInSource :: IO Bool
testClassifyEvalExceptionInSource = do
  src <- TIO.readFile "src/HaskellFlows/Tool/Eval.hs"
  pure $ T.isInfixOf "classifyEvalException" src
      && T.isInfixOf "GhcException" src
      && T.isInfixOf "SourceError"  src
      && T.isInfixOf "Env.CompileError" src
