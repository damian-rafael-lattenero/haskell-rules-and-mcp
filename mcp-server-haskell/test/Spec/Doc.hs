-- | Unit tests for the @ghc_doc@ tool — Haddock lookup (#87 no_match
-- contract), nextStep routing for in-scope no-doc (#195), LaTeX
-- stripping (F-11), and the bracket-strip helper (#144).
--
-- Extracted from the Spec.hs monolith (#271) via the function-export shape:
-- the driver keeps the registrations and imports these functions.
module Spec.Doc
  ( testDocHasDocOk
  , testDocUnknownNameNoMatch
  , testDocRefusesNewline
  , testDocNoDocNextStepIsInfo
  , testDocStripLatex
  , testDocStripBrackets
  ) where

import qualified Data.Aeson as A
import qualified Data.Aeson.Key as AKey
import qualified Data.Aeson.KeyMap as AKM
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.IO as TIO
import System.Directory (createDirectoryIfMissing, getTemporaryDirectory, removePathForcibly)
import System.FilePath ((</>))

import HaskellFlows.Ghc.ApiSession (killGhcSession, startGhcSession)
import qualified HaskellFlows.Mcp.Envelope as Env
import Spec.ToolEnvFixture (sessionEnv)
import qualified HaskellFlows.Mcp.NextStep as NextStep
import HaskellFlows.Mcp.ToolName (ToolName (..))
import qualified HaskellFlows.Tool.Doc as DocTool
import HaskellFlows.Types (mkProjectDir)

-- ---------------------------------------------------------------------------
-- Phase B helper
-- ---------------------------------------------------------------------------

runDoc :: A.Value -> IO (Either String Env.ToolResponse)
runDoc args = do
  tmp <- getTemporaryDirectory
  let dir = tmp </> "haskell-flows-doc-test"
  removePathForcibly dir
  createDirectoryIfMissing True (dir </> "src")
  TIO.writeFile (dir </> "src" </> "Foo.hs")
    (T.pack "module Foo where\nfoo :: Int\nfoo = 1\n")
  result <- case mkProjectDir dir of
    Left _   -> pure (Left "could not build ProjectDir")
    Right pd -> do
      sess <- startGhcSession pd
      tr   <- DocTool.handle (sessionEnv sess) args
      killGhcSession sess
      pure (Right tr)
  removePathForcibly dir
  pure result

-- ---------------------------------------------------------------------------
-- GHC-session doc tests
-- ---------------------------------------------------------------------------

-- | 'ghc_doc' on a Prelude name (e.g. 'map') usually has Haddock
-- on a properly-installed base. Status='ok' with result.hasDoc=true.
-- The test accepts BOTH 'ok' (Haddock available) and 'no_match'
-- (Haddock missing on this build of base) — the contract is that
-- a name-in-scope with no doc maps to no_match, not to an error.
testDocHasDocOk :: IO Bool
testDocHasDocOk = do
  decoded <- runDoc (A.object [ "name" A..= ("map" :: Text) ])
  pure $ case decoded of
    Right env
      | Env.reStatus env == Env.StatusOk
      , Just (A.Object payload) <- Env.reResult env ->
          AKM.lookup (AKey.fromText "hasDoc") payload == Just (A.Bool True)
            && AKM.member (AKey.fromText "doc") payload
      | Env.reStatus env == Env.StatusNoMatch
      , Just (A.Object payload) <- Env.reResult env ->
          AKM.lookup (AKey.fromText "hasDoc") payload == Just (A.Bool False)
            && AKM.member (AKey.fromText "reason") payload
    _ -> False

-- | 'ghc_doc' on a name that's not in scope → status='no_match'.
testDocUnknownNameNoMatch :: IO Bool
testDocUnknownNameNoMatch = do
  decoded <- runDoc
    (A.object [ "name" A..= ("definitelyNotARealName123" :: Text) ])
  pure $ case decoded of
    Right env
      | Env.reStatus env == Env.StatusNoMatch
      , Just (A.Object payload) <- Env.reResult env ->
          AKM.lookup (AKey.fromText "name") payload
            == Just (A.String "definitelyNotARealName123")
            && AKM.lookup (AKey.fromText "hasDoc") payload == Just (A.Bool False)
            && AKM.member (AKey.fromText "reason") payload
    _ -> False

-- | A newline-laden name → status='refused' with kind='newline_injection'.
testDocRefusesNewline :: IO Bool
testDocRefusesNewline = do
  decoded <- runDoc (A.object [ "name" A..= ("foo\n:quit" :: Text) ])
  pure $ case decoded of
    Right env
      | Env.reStatus env == Env.StatusRefused
      , Just err <- Env.reError env ->
          Env.eeKind err == Env.NewlineInjection
            && Env.eeField err == Just "name"
    _ -> False

-- ---------------------------------------------------------------------------
-- Pure payload tests
-- ---------------------------------------------------------------------------

-- | #195: when ghc_doc returns status=ok with @hasDoc=false@, the
-- injected nextStep must route to @ghc_info@, not @hoogle_search@.
testDocNoDocNextStepIsInfo :: IO Bool
testDocNoDocNextStepIsInfo =
  let payload = DocTool.noDocInScopePayload "greet"
      mNs = NextStep.suggestNext GhcDoc True payload
  in case mNs of
       Just ns -> pure (NextStep.nsTool ns == GhcInfo)
       Nothing -> pure False

-- | F-11: 'hasDocPayload' must strip LaTeX @\\(…\\)@ delimiters that
-- GHC's pretty-printer emits for math notation in Haddock strings.
testDocStripLatex :: IO Bool
testDocStripLatex =
  let raw     = "O(\\(n\\)) complexity"
      payload = DocTool.hasDocPayload "foo" raw
  in pure $ case payload of
       A.Object km ->
         case AKM.lookup "doc" km of
           Just (A.String d) -> not (T.isInfixOf "\\(" d) && T.isInfixOf "O(" d
           _                 -> False
       _ -> False

-- | #144: GHC's 'showPprUnsafe' wraps Haddock doc strings in literal
-- @[@ … @]@ brackets (its internal DocH list format). 'hasDocPayload'
-- must strip those brackets so agents receive clean prose.
testDocStripBrackets :: IO Bool
testDocStripBrackets = pure $
  DocTool.stripDocBrackets "[ Left-associative fold @since base-4.6.0.0]"
    == "Left-associative fold @since base-4.6.0.0"
  && DocTool.stripDocBrackets "no brackets here" == "no brackets here"
  && DocTool.stripDocBrackets "[ ]" == ""
  && DocTool.stripDocBrackets "[single]" == "single"
