-- | @ghc_explain_error@ — type-error therapist (#59).
--
-- Phase 1: 'ghc_explain_error' loads the module, picks the target
-- diagnostic, and returns @{diagnostic, context}@ where @context@
-- packages the module source, the import list, and the diagnostic's
-- enclosing line range. The agent uses its own LLM to propose fix
-- candidates.
--
-- Phase 2 (this commit): optional @verify_patch@ argument.
-- When present, the tool applies the patch to the file (same
-- snapshot-and-recompile safety as 'ghc_refactor'), checks whether
-- the original error is gone from the post-compile diagnostic set,
-- and always restores the original file regardless of outcome.
-- The caller gets a 'verify_result' field with:
--   @{patch_applied, error_resolved, diagnostics_after}@
--
-- The split keeps the MCP free of an LLM dependency (Option A in
-- the issue) — the agent already has an LLM, the MCP provides
-- the evidence package and verification harness.
module HaskellFlows.Tool.ExplainError
  ( descriptor
  , handle
  , ExplainErrorArgs (..)
    -- * Pure helpers (exported for unit tests)
  , pickDiagnostic
  , extractImports
  , enclosingLineRange
    -- * Phase 2 helpers (exported for unit tests)
  , PatchSpec (..)
  , applyLinePatch
    -- * Response shaping (exported for unit tests)
  , renderContext
  ) where

import Control.Exception (SomeException, try)
import Data.Char (isAsciiLower, isAsciiUpper, isDigit)
import Data.Aeson
import Data.Aeson.Types (parseEither)
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.IO as TIO

import HaskellFlows.Ghc.ApiSession
  ( GhcSession
  , LoadFlavour (..)
  , invalidateLoadCache
  , loadAndCaptureDiagnostics
  )
import qualified HaskellFlows.Mcp.Envelope as Env
import HaskellFlows.Mcp.ParseError (formatParseError)
import HaskellFlows.Mcp.Protocol
import HaskellFlows.Mcp.ToolName (ToolName (..), toolNameText)
import HaskellFlows.Parser.Error (GhcError (..), Severity (..))
import HaskellFlows.Types (ProjectDir, ModulePath, mkModulePath, unModulePath)

descriptor :: ToolDescriptor
descriptor =
  ToolDescriptor
    { tdName        = toolNameText GhcExplainError
    , tdDescription =
        "Build a structured explanation context for a type error. "
          <> "Phase 1: returns {diagnostic, context} where 'context' "
          <> "packages the module source + imports + the diagnostic's "
          <> "enclosing line range. The agent uses its own LLM to "
          <> "propose fix candidates. Phase 2 (planned): a separate "
          <> "verify endpoint applies each candidate to a snapshot "
          <> "and returns ranked verified candidates."
    , tdInputSchema =
        object
          [ "type"       .= ("object" :: Text)
          , "properties" .= object
              [ "module_path"      .= obj "string"
              , "diagnostic_index" .= obj "integer"
              , "verify_patch"     .= object
                  [ "type" .= ("object" :: Text)
                  , "properties" .= object
                      [ "line" .= obj "integer"
                      , "old"  .= obj "string"
                      , "new"  .= obj "string"
                      ]
                  , "required" .= (["line", "old", "new"] :: [Text])
                  ]
              ]
          , "required"             .= (["module_path"] :: [Text])
          , "additionalProperties" .= False
          ]
    }
  where
    obj :: Text -> Value
    obj t = object [ "type" .= t ]

-- | Line-level patch specification. @line@ is 1-based.
-- The tool replaces the first occurrence of @old@ on that line
-- with @new@.
data PatchSpec = PatchSpec
  { psLine :: !Int
  , psOld  :: !Text
  , psNew  :: !Text
  }
  deriving stock (Show)

instance FromJSON PatchSpec where
  parseJSON = withObject "PatchSpec" $ \o ->
    PatchSpec
      <$> o .: "line"
      <*> o .: "old"
      <*> o .: "new"

data ExplainErrorArgs = ExplainErrorArgs
  { eaModulePath      :: !Text
  , eaDiagnosticIndex :: !(Maybe Int)
  , eaVerifyPatch     :: !(Maybe PatchSpec)
    -- ^ Phase 2: when set, apply this patch, recompile, check if
    -- the original error is resolved, then restore the file.
  }
  deriving stock (Show)

instance FromJSON ExplainErrorArgs where
  parseJSON = withObject "ExplainErrorArgs" $ \o ->
    ExplainErrorArgs
      <$> o .:  "module_path"
      <*> o .:? "diagnostic_index"
      <*> o .:? "verify_patch"

handle :: GhcSession -> ProjectDir -> Value -> IO ToolResult
handle ghcSess pd rawArgs = case parseEither parseJSON rawArgs of
  Left err -> pure (formatParseError err)
  Right args -> case mkModulePath pd (T.unpack (eaModulePath args)) of
    Left e   -> pure (pathTraversalResult (T.pack (show e)))
    Right mp -> do
      let full = unModulePath mp
      eBody <- try (TIO.readFile full)
                 :: IO (Either SomeException Text)
      case eBody of
        Left e -> pure (subprocessResult
          (T.pack ("Could not read module: " <> show e)))
        Right body -> do
          (_, diags) <- loadAndCaptureDiagnostics ghcSess Strict
          -- Filter to OWN-module errors so a sibling's failure
          -- doesn't drag the agent off-target.
          let ownDiags = filter (ownsThisModule (eaModulePath args)) diags
          case pickDiagnostic (eaDiagnosticIndex args) ownDiags of
            Nothing ->
              pure (renderNoErrors (eaModulePath args) ownDiags)
            Just diag -> do
              -- Phase 2: optional patch verification.
              mVerify <- case eaVerifyPatch args of
                Nothing    -> pure Nothing
                Just patch -> Just <$> runVerifyPatch ghcSess mp body diag patch
              pure (renderContext (eaModulePath args) body diag ownDiags mVerify)

ownsThisModule :: Text -> GhcError -> Bool
ownsThisModule rel diag = rel `T.isSuffixOf` geFile diag

--------------------------------------------------------------------------------
-- diagnostic selection
--------------------------------------------------------------------------------

-- | Issue #59: pick the diagnostic the agent wants to explain.
-- Defaults to the first error (severity SevError); the optional
-- @diagnostic_index@ lets the agent target a specific slot when
-- the module has multiple errors.
pickDiagnostic :: Maybe Int -> [GhcError] -> Maybe GhcError
pickDiagnostic mIdx diags =
  let errs = filter ((== SevError) . geSeverity) diags
  in case mIdx of
       Just i
         | i >= 0, i < length errs -> Just (errs !! i)
         | otherwise               -> Nothing
       Nothing -> case errs of
         (d : _) -> Just d
         []      -> Nothing

--------------------------------------------------------------------------------
-- context builder
--------------------------------------------------------------------------------

-- | Issue #59: enumerate import lines as @{module, qualified, alias,
-- specific}@ tuples. Phase 1 keeps this textual; Phase 2 will
-- resolve to package + version via 'ghc-pkg field'.
extractImports :: Text -> [Value]
extractImports body =
  [ object
      [ "raw"       .= ln
      , "module"    .= modName
      , "qualified" .= isQual
      ]
  | ln <- T.lines body
  , let stripped = T.stripStart ln
  , Just rest <- [T.stripPrefix "import " stripped]
  , let (modName, isQual) = parseImport rest
  , not (T.null modName)
  ]
  where
    parseImport rest =
      case T.stripPrefix "qualified " (T.stripStart rest) of
        Just s  ->
          ( T.takeWhile isModChar (T.stripStart s)
          , True
          )
        Nothing ->
          ( T.takeWhile isModChar (T.stripStart rest)
          , False
          )
    isModChar c = isAsciiUpper c
               || isAsciiLower c
               || isDigit c
               || c == '.' || c == '_' || c == '\''

-- | Issue #59: return the line range surrounding a given line
-- ('lineNum') with a window of @padding@ on each side, clamped
-- to the body's actual size. Used to slice the LLM-context to
-- a manageable size when the module is huge.
enclosingLineRange :: Int -> Int -> Int -> (Int, Int)
enclosingLineRange totalLines padding lineNum =
  let lo = max 1 (min totalLines (lineNum - padding))
      hi = min totalLines (max 1 (lineNum + padding))
  in (min lo hi, max lo hi)

--------------------------------------------------------------------------------
-- Phase 2: patch verification
--------------------------------------------------------------------------------

-- | Apply @patch@ to @body@, overwriting the file, recompile, then
-- always restore the original. Returns a JSON 'Value' with the
-- verify result.
runVerifyPatch
  :: GhcSession -> ModulePath -> Text -> GhcError -> PatchSpec
  -> IO Value
runVerifyPatch ghcSess mp body origDiag patch = do
  let path = unModulePath mp
  case applyLinePatch body patch of
    Nothing ->
      pure (object
        [ "patch_applied"   .= False
        , "error_resolved"  .= False
        , "reason"          .= ("patch target not found on specified line" :: Text)
        ])
    Just patched -> do
      writeRes <- try (TIO.writeFile path patched) :: IO (Either SomeException ())
      case writeRes of
        Left e ->
          pure (object
            [ "patch_applied"  .= False
            , "error_resolved" .= False
            , "reason"         .= T.pack ("write failed: " <> show e)
            ])
        Right _ -> do
          invalidateLoadCache ghcSess
          (_, postDiags) <- loadAndCaptureDiagnostics ghcSess Strict
          -- Restore original regardless of outcome.
          _ <- try (TIO.writeFile path body) :: IO (Either SomeException ())
          invalidateLoadCache ghcSess
          let origKey     = (geFile origDiag, geLine origDiag, geColumn origDiag)
              origGone    = all (\d -> (geFile d, geLine d, geColumn d) /= origKey
                                    || geMessage d /= geMessage origDiag)
                                postDiags
              postErrDiags = filter ((== SevError) . geSeverity) postDiags
          pure (object
            [ "patch_applied"      .= True
            , "error_resolved"     .= origGone
            , "diagnostics_after"  .= map renderDiag postErrDiags
            ])

-- | Pure helper: apply a line-level text patch to @body@.
-- Returns @Nothing@ when the old text is not found on the
-- specified line (1-based). Returns @Just@ the patched content.
applyLinePatch :: Text -> PatchSpec -> Maybe Text
applyLinePatch body patch =
  let lns        = T.lines body
      idx        = psLine patch - 1   -- 0-based index
  in if idx < 0 || idx >= length lns
       then Nothing
       else
         let ln = lns !! idx
         in if psOld patch `T.isInfixOf` ln
              then
                let replaced = T.replace (psOld patch) (psNew patch) ln
                    newLns   = take idx lns <> [replaced] <> drop (idx + 1) lns
                in Just (T.unlines newLns)
              else Nothing

--------------------------------------------------------------------------------
-- response shaping
--------------------------------------------------------------------------------

-- | Phase C + Phase 2: a found-error context is informational —
-- the tool's job is to package evidence for the agent's LLM, not
-- to fail. status='ok' always; the agent reads
-- 'result.diagnostic' and decides what to do next.
renderContext :: Text -> Text -> GhcError -> [GhcError] -> Maybe Value -> ToolResult
renderContext modulePath body diag ownDiags mVerify =
  let lns      = T.lines body
      total    = length lns
      -- F-24: padding reduced from 50 to 15. With padding=50 the
      -- entire file was included for any module under 100 lines,
      -- defeating the purpose of the slice.
      (lo, hi) = enclosingLineRange total 15 (geLine diag)
      sliced   = T.unlines
        [ ln | (i, ln) <- zip [1 :: Int ..] lns, i >= lo, i <= hi ]
      verifyFields = case mVerify of
        Nothing -> []
        Just v  -> [ "verify_result" .= v ]
      payload = object $
        [ "module_path" .= modulePath
        , "diagnostic"  .= renderDiag diag
        , "context"     .= object
            [ "enclosing_slice" .= sliced
            , "enclosing_range" .= object
                [ "start" .= lo
                , "end"   .= hi
                ]
            , "total_lines"     .= total
            , "imports"         .= extractImports body
            , "all_errors"      .= map renderDiag ownDiags
            ]
        , "instructions_for_agent" .=
            ( "Propose fix candidates as JSON {explanation, patch{line, \
              \old, new}, rationale}. Pass the patch as verify_patch to \
              \let the tool apply it, recompile, and report whether the \
              \error is resolved. The original file is always restored." :: Text )
        ] <> verifyFields
  in Env.toolResponseToResult (Env.mkOk payload)

-- | Issue #90 Phase C: no error to explain → status='ok' with
-- 'diagnostic=null' and the agent-side hint under 'result'.
renderNoErrors :: Text -> [GhcError] -> ToolResult
renderNoErrors modulePath diags =
  let payload = object
        [ "module_path" .= modulePath
        , "diagnostic"  .= Null
        , "warnings"    .= map renderDiag
                              (filter ((== SevWarning) . geSeverity) diags)
        , "hint"        .=
            ( "No errors detected in this module. If the project as a \
              \whole still fails to build, run ghc_check_project to \
              \enumerate the failing modules." :: Text )
        ]
  in Env.toolResponseToResult (Env.mkOk payload)

renderDiag :: GhcError -> Value
renderDiag d = object
  [ "file"     .= geFile d
  , "line"     .= geLine d
  , "column"   .= geColumn d
  , "severity" .= sevText (geSeverity d)
  , "message"  .= geMessage d
  ]
  where
    sevText :: Severity -> Text
    sevText SevError   = "error"
    sevText SevWarning = "warning"


-- | Issue #90 Phase C: 'mkModulePath' rejection.
pathTraversalResult :: Text -> ToolResult
pathTraversalResult msg =
  Env.toolResponseToResult
    (Env.mkRefused (Env.mkErrorEnvelope Env.PathTraversal msg))

-- | Issue #90 Phase C: filesystem read failure.
subprocessResult :: Text -> ToolResult
subprocessResult msg =
  Env.toolResponseToResult
    (Env.mkFailed (Env.mkErrorEnvelope Env.SubprocessError msg))
