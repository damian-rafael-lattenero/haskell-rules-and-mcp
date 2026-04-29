-- | @ghc_explain_error@ — type-error therapist (#59).
--
-- Phase 1 scope (issue itself estimates 1-2 weeks total): build a
-- structured \"explanation context\" the agent can feed to its
-- own LLM. The two-call protocol the issue describes — context
-- collection + agent-driven candidate verification — is delivered
-- in two iterations:
--
--   * THIS commit: 'ghc_explain_error' loads the module, picks
--     the target diagnostic, and returns @{diagnostic, context}@
--     where @context@ packages the module source, the import
--     list, and the diagnostic's enclosing line range. The agent
--     uses its own LLM to propose candidates.
--   * Phase 2 (planned): 'ghc_explain_error_verify' receives an
--     array of agent-proposed @{patch}@ objects, applies each to
--     a snapshot via the same machinery 'ghc_refactor' uses, and
--     returns ranked verified candidates.
--
-- The split keeps the MCP free of an LLM dependency (Option A in
-- the issue) — the agent already has an LLM, the MCP provides
-- the evidence package and (Phase 2) the verification harness.
module HaskellFlows.Tool.ExplainError
  ( descriptor
  , handle
  , ExplainErrorArgs (..)
    -- * Pure helpers (exported for unit tests)
  , pickDiagnostic
  , extractImports
  , enclosingLineRange
  ) where

import Control.Exception (SomeException, try)
import Data.Aeson
import Data.Aeson.Types (parseEither)
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.IO as TIO
import qualified Data.Text.Lazy as TL
import qualified Data.Text.Lazy.Encoding as TLE

import HaskellFlows.Ghc.ApiSession
  ( GhcSession
  , LoadFlavour (..)
  , loadAndCaptureDiagnostics
  )
import HaskellFlows.Mcp.Protocol
import HaskellFlows.Mcp.ToolName (ToolName (..), toolNameText)
import HaskellFlows.Parser.Error (GhcError (..), Severity (..))
import HaskellFlows.Types (ProjectDir, mkModulePath, unModulePath)

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
              ]
          , "required"             .= (["module_path"] :: [Text])
          , "additionalProperties" .= False
          ]
    }
  where
    obj :: Text -> Value
    obj t = object [ "type" .= t ]

data ExplainErrorArgs = ExplainErrorArgs
  { eaModulePath      :: !Text
  , eaDiagnosticIndex :: !(Maybe Int)
  }
  deriving stock (Show)

instance FromJSON ExplainErrorArgs where
  parseJSON = withObject "ExplainErrorArgs" $ \o ->
    ExplainErrorArgs
      <$> o .:  "module_path"
      <*> o .:? "diagnostic_index"

handle :: GhcSession -> ProjectDir -> Value -> IO ToolResult
handle ghcSess pd rawArgs = case parseEither parseJSON rawArgs of
  Left err -> pure (errorResult (T.pack ("Invalid arguments: " <> err)))
  Right args -> case mkModulePath pd (T.unpack (eaModulePath args)) of
    Left e   -> pure (errorResult (T.pack (show e)))
    Right mp -> do
      let full = unModulePath mp
      eBody <- try (TIO.readFile full)
                 :: IO (Either SomeException Text)
      case eBody of
        Left e -> pure (errorResult
          (T.pack ("Could not read module: " <> show e)))
        Right body -> do
          (_, diags) <- loadAndCaptureDiagnostics ghcSess Strict
          -- Filter to OWN-module errors so a sibling's failure
          -- doesn't drag the agent off-target.
          let ownDiags = filter (ownsThisModule (eaModulePath args)) diags
          case pickDiagnostic (eaDiagnosticIndex args) ownDiags of
            Nothing ->
              pure (renderNoErrors (eaModulePath args) ownDiags)
            Just diag ->
              pure (renderContext (eaModulePath args) body diag ownDiags)

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
    isModChar c = (c >= 'A' && c <= 'Z')
               || (c >= 'a' && c <= 'z')
               || (c >= '0' && c <= '9')
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
-- response shaping
--------------------------------------------------------------------------------

renderContext :: Text -> Text -> GhcError -> [GhcError] -> ToolResult
renderContext modulePath body diag ownDiags =
  let lns      = T.lines body
      total    = length lns
      (lo, hi) = enclosingLineRange total 50 (geLine diag)
      sliced   = T.unlines
        [ ln | (i, ln) <- zip [1 :: Int ..] lns, i >= lo, i <= hi ]
      payload = object
        [ "success"     .= True
        , "module_path" .= modulePath
        , "diagnostic"  .= renderDiag diag
        , "context"     .= object
            [ "module_source"   .= body
            , "enclosing_slice" .= sliced
            , "enclosing_range" .= object
                [ "start" .= lo
                , "end"   .= hi
                ]
            , "total_lines"     .= total
            , "imports"         .= extractImports body
            , "all_errors"      .= map renderDiag ownDiags
            ]
        , "instructions_for_agent" .=
            ( "Propose 3 fix candidates as JSON {explanation, patch{line, \
              \old, new}, rationale}. Phase 1 has no verify endpoint — \
              \apply your top candidate via ghc_refactor (rename_local) \
              \or hand-edit. Phase 2 will route candidates through a \
              \verify endpoint that snapshots, applies, recompiles, and \
              \ranks." :: Text )
        ]
  in ToolResult
       { trContent = [ TextContent (encodeUtf8Text payload) ]
       , trIsError = False
       }

renderNoErrors :: Text -> [GhcError] -> ToolResult
renderNoErrors modulePath diags =
  let payload = object
        [ "success"     .= True
        , "module_path" .= modulePath
        , "diagnostic"  .= Null
        , "warnings"    .= map renderDiag
                              (filter ((== SevWarning) . geSeverity) diags)
        , "hint"        .=
            ( "No errors detected in this module. If the project as a \
              \whole still fails to build, run ghc_check_project to \
              \enumerate the failing modules." :: Text )
        ]
  in ToolResult
       { trContent = [ TextContent (encodeUtf8Text payload) ]
       , trIsError = False
       }

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

errorResult :: Text -> ToolResult
errorResult msg =
  ToolResult
    { trContent = [ TextContent (encodeUtf8Text (object
        [ "success" .= False, "error" .= msg ])) ]
    , trIsError = True
    }

encodeUtf8Text :: Value -> Text
encodeUtf8Text = TL.toStrict . TLE.decodeUtf8 . encode
