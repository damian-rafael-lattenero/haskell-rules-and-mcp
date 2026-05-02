-- | @ghc_refactor@ — small-scope refactors with snapshot-and-compile
-- semantics.
--
-- Actions:
--
-- * @rename_local@ — rewrite @old_name@ → @new_name@ inside
--   @[scope_line_start, scope_line_end]@.
-- * @extract_binding@ — replace a line range with a call to
--   @new_name@ and append a top-level binding of the original body.
--
-- Safety contract:
--
-- * We snapshot the current file contents before any edit.
-- * We write the rewrite to disk.
-- * We call @ghc_load@ (strict mode) against the rewritten file.
-- * If GHCi surfaces any @error:@, we restore the snapshot verbatim
--   and return the compile errors to the agent.
-- * If compilation succeeds (or only has non-blocking warnings), the
--   edit stays committed.
--
-- @dry_run: true@ short-circuits before the disk write — the rewrite
-- is computed, the diff returned, nothing touches the filesystem or
-- GHCi. Useful for a preview before committing.
--
-- This is textual, not AST-aware. The compile step is the correctness
-- oracle — we never have to reason about Haskell syntax ourselves.
module HaskellFlows.Tool.Refactor
  ( descriptor
  , handle
  , RefactorArgs (..)
  , Action (..)
    -- * Diagnostic-diff helpers (#50)
  , errorKey
  , errorSignatures
  ) where

import Control.Exception (SomeException, try)
import Data.Aeson
import qualified Data.Aeson.Key as Key
import qualified Data.Aeson.KeyMap as KeyMap
import Data.Aeson.Types (parseEither)
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.IO as TIO

import HaskellFlows.Ghc.ApiSession
  ( GhcSession
  , LoadFlavour (..)
  , invalidateLoadCache
  , loadForTarget
  , targetForPath
  )
import qualified HaskellFlows.Mcp.Envelope as Env
import qualified HaskellFlows.Mcp.Schema as Schema
import HaskellFlows.Mcp.PermissiveJSON
  ( IntField (unIntField)
  , BoolField (unBoolField)
  )
import HaskellFlows.Mcp.Protocol
import HaskellFlows.Mcp.ToolName (ToolName (..), toolNameText)
import qualified HaskellFlows.Tool.Move as Move
import HaskellFlows.Parser.Error
  ( GhcError (..)
  , Severity (..)
  )
import HaskellFlows.Refactor.Extract
  ( ExtractResult (..)
  , extractBinding
  )
import HaskellFlows.Refactor.Rename
  ( RenameResult (..)
  , renameInScope
  , validateIdentifier
  )
import HaskellFlows.Types
  ( ModulePath
  , PathError (..)
  , ProjectDir
  , mkModulePath
  , unModulePath
  )

descriptor :: ToolDescriptor
descriptor =
  ToolDescriptor
    { tdName        = toolNameText GhcRefactor
    , tdDescription =
        "Refactors with snapshot-and-compile safety. Actions: "
          <> "'rename_local' (scoped identifier rename), "
          <> "'extract_binding' (lift a line range to a named "
          <> "top-level binding), 'move_symbol' (atomic cross-module "
          <> "move of a top-level binding — slices signature + "
          <> "Haddock + body, rewrites consumer imports, verifies, "
          <> "rolls back on failure). If GHCi reports a compile "
          <> "error on the rewrite, the file is restored from "
          <> "snapshot — the refactor is atomic from the agent's "
          <> "perspective. (#94 Phase C: 'move_symbol' subsumes the "
          <> "retired ghc_move.)"
    , tdInputSchema = schema
    }

-- | Issue #92 Phase B: per-action discriminated schema. Each
-- branch declares its OWN required-field set so a host that
-- respects the schema sends a valid-by-spec request that the
-- runtime accepts. Pre-#92 the schema declared a flat
-- @required: [action, module_path, new_name]@ that lied about
-- @rename_local@ (which actually needs old_name + scope_line_*)
-- and @extract_binding@ (scope_line_*).
schema :: Value
schema = Schema.discriminatedSchema "action"
  [ Schema.SchemaBranch
      { Schema.sbDiscriminantValue = "rename_local"
      , Schema.sbDescription       =
          "Scoped identifier rename within an explicit line range."
      , Schema.sbProperties        =
          [ ("module_path",      Schema.stringField  "Relative module path.")
          , ("old_name",         Schema.stringField  "Identifier to replace.")
          , ("new_name",         Schema.stringField
              "Replacement identifier (must be a valid Haskell varid).")
          , ("scope_line_start", Schema.integerField
              "Inclusive 1-based start line of the rename scope.")
          , ("scope_line_end",   Schema.integerField
              "Inclusive 1-based end line of the rename scope.")
          , ("dry_run",          Schema.booleanField
              "If true, compute the rewrite and return without touching disk.")
          ]
      , Schema.sbRequired
          = [ "module_path", "old_name", "new_name"
            , "scope_line_start", "scope_line_end"
            ]
      }
  , Schema.SchemaBranch
      { Schema.sbDiscriminantValue = "extract_binding"
      , Schema.sbDescription       =
          "Extract a line range into a named top-level binding."
      , Schema.sbProperties        =
          [ ("module_path",      Schema.stringField  "Relative module path.")
          , ("new_name",         Schema.stringField
              "New top-level binding name (must be a valid Haskell varid).")
          , ("scope_line_start", Schema.integerField
              "Inclusive 1-based start line of the range to extract.")
          , ("scope_line_end",   Schema.integerField
              "Inclusive 1-based end line of the range to extract.")
          , ("dry_run",          Schema.booleanField
              "If true, compute the rewrite and return without touching disk.")
          ]
      , Schema.sbRequired
          = [ "module_path", "new_name"
            , "scope_line_start", "scope_line_end"
            ]
      }
  , Schema.SchemaBranch
      { Schema.sbDiscriminantValue = "move_symbol"
      , Schema.sbDescription       =
          "Atomic cross-module move of a top-level binding. \
          \Slices signature + Haddock + body out of 'from', \
          \appends to 'to', rewrites consumer 'import' lines, \
          \verifies the project still loads. Any failure rolls \
          \back ALL touched files. Phase 1: destination module \
          \must already exist. (#94 Phase C: subsumes the retired \
          \ghc_move.)"
      , Schema.sbProperties        =
          [ ("symbol",  Schema.stringField
              "Name of the top-level binding to move.")
          , ("from",    Schema.stringField
              "Source module path (relative).")
          , ("to",      Schema.stringField
              "Destination module path (relative). Must exist.")
          , ("dry_run", Schema.booleanField
              "If true, compute the moved layout and return without \
              \touching disk.")
          ]
      , Schema.sbRequired
          = [ "symbol", "from", "to" ]
      }
  ]

data Action = ActRename | ActExtract
  deriving stock (Eq, Show)

data RefactorArgs = RefactorArgs
  { raAction         :: !Action
  , raModulePath     :: !Text
  , raOldName        :: !(Maybe Text)
  , raNewName        :: !Text
  , raScopeLineStart :: !(Maybe Int)
  , raScopeLineEnd   :: !(Maybe Int)
  , raDryRun         :: !Bool
  }
  deriving stock (Show)

-- | Issue #92 Phase B: per-action validation at parse time.
-- The flat record stays for handler convenience, but the parser
-- now enforces the *same* required-field set the schema
-- advertises — failures surface as Aeson \"missing_arg\" /
-- \"type_mismatch\" via the friendly ParseError formatter (#85)
-- rather than as a runtime "is required for rename_local" check
-- buried in the handler. Schema and runtime stop disagreeing.
instance FromJSON RefactorArgs where
  parseJSON = withObject "RefactorArgs" $ \o -> do
    a   <- o .:  "action"
    mp  <- o .:  "module_path"
    new <- o .:  "new_name"
    -- Issue #88: accept stringified numerics / booleans from MCP
    -- host wrappers that serialise primitives as strings.
    dr  <- maybe False unBoolField <$> o .:? "dry_run"
    act <- case (a :: Text) of
      "rename_local"    -> pure ActRename
      "extract_binding" -> pure ActExtract
      other             -> fail ("unknown action: " <> T.unpack other)
    case act of
      ActRename -> do
        -- rename_local REQUIRES old_name + both scope lines.
        -- Per-action enforcement at the parser boundary is the
        -- single-source-of-truth fix for #92.
        old <- o .:  "old_name"
        ls  <- unIntField <$> o .:  "scope_line_start"
        le  <- unIntField <$> o .:  "scope_line_end"
        pure RefactorArgs
          { raAction         = act
          , raModulePath     = mp
          , raOldName        = Just old
          , raNewName        = new
          , raScopeLineStart = Just ls
          , raScopeLineEnd   = Just le
          , raDryRun         = dr
          }
      ActExtract -> do
        ls  <- unIntField <$> o .:  "scope_line_start"
        le  <- unIntField <$> o .:  "scope_line_end"
        pure RefactorArgs
          { raAction         = act
          , raModulePath     = mp
          , raOldName        = Nothing
          , raNewName        = new
          , raScopeLineStart = Just ls
          , raScopeLineEnd   = Just le
          , raDryRun         = dr
          }

handle :: GhcSession -> ProjectDir -> Value -> IO ToolResult
handle ghcSess pd rawArgs
  -- #94 Phase C: 'move_symbol' is a passthrough to Move.handle.
  -- Intercepted before the rename/extract parser because its payload
  -- shape (symbol/from/to) doesn't match RefactorArgs.
  | actionTextOf rawArgs == Just "move_symbol" = Move.handle ghcSess pd rawArgs
  | otherwise = case parseEither parseJSON rawArgs of
      Left parseError ->
        pure (errorResult (T.pack ("Invalid arguments: " <> parseError)))
      Right args -> case mkModulePath pd (T.unpack (raModulePath args)) of
        Left e   -> pure (pathTraversalResult (formatPathError e))
        Right mp -> do
          r <- handleAction ghcSess mp args
          invalidateLoadCache ghcSess
          pure r
  where
    -- Peek at the 'action' field without committing to RefactorArgs's
    -- parser; cheap helper used only for the move_symbol dispatch above.
    actionTextOf :: Value -> Maybe Text
    actionTextOf v = case v of
      Object o -> case KeyMap.lookup "action" o of
        Just (String s) -> Just s
        _               -> Nothing
      _ -> Nothing

handleAction :: GhcSession -> ModulePath -> RefactorArgs -> IO ToolResult
handleAction sess mp args = case raAction args of
  ActRename  -> handleRename  sess mp args
  ActExtract -> handleExtract sess mp args

--------------------------------------------------------------------------------
-- rename_local
--------------------------------------------------------------------------------

handleRename :: GhcSession -> ModulePath -> RefactorArgs -> IO ToolResult
handleRename sess mp args = case raOldName args of
  Nothing  -> pure (errorResult "'old_name' is required for rename_local")
  Just old -> case validateIdentifier old of
    Left err -> pure (errorResult err)
    Right safeOld -> case validateIdentifier (raNewName args) of
      Left err -> pure (errorResult err)
      Right safeNew -> case (raScopeLineStart args, raScopeLineEnd args) of
        (Nothing, _) -> pure (errorResult "'scope_line_start' is required for rename_local")
        (_, Nothing) -> pure (errorResult "'scope_line_end' is required for rename_local")
        (Just ls, Just le) -> withSnapshot sess mp (raDryRun args) $ \orig ->
          case renameInScope safeOld safeNew ls le orig of
            Left err -> pure (Left err)
            Right rr ->
              if rrOccurrences rr == 0
                then pure (Left ( "no occurrences of '" <> safeOld
                               <> "' in lines " <> tshow ls <> "-" <> tshow le ))
                else pure (Right (rrNewContent rr, renameSuccess safeOld safeNew rr))

renameSuccess :: Text -> Text -> RenameResult -> Value
renameSuccess old new rr =
  object
    [ "action"         .= ("rename_local" :: Text)
    , "old_name"       .= old
    , "new_name"       .= new
    , "occurrences"    .= rrOccurrences rr
    , "touched_lines"  .= rrTouchedLines rr
    ]

--------------------------------------------------------------------------------
-- extract_binding
--------------------------------------------------------------------------------

handleExtract :: GhcSession -> ModulePath -> RefactorArgs -> IO ToolResult
handleExtract sess mp args = case validateIdentifier (raNewName args) of
  Left err -> pure (errorResult err)
  Right safeNew -> case (raScopeLineStart args, raScopeLineEnd args) of
    (Nothing, _) -> pure (errorResult "'scope_line_start' is required for extract_binding")
    (_, Nothing) -> pure (errorResult "'scope_line_end' is required for extract_binding")
    (Just ls, Just le) -> withSnapshot sess mp (raDryRun args) $ \orig ->
      case extractBinding safeNew ls le orig of
        Left err -> pure (Left err)
        Right er -> pure (Right (erNewContent er, extractSuccess safeNew er ls le))

extractSuccess :: Text -> ExtractResult -> Int -> Int -> Value
extractSuccess newName er ls le =
  object
    [ "action"        .= ("extract_binding" :: Text)
    , "new_name"      .= newName
    , "line_start"    .= ls
    , "line_end"      .= le
    , "indent"        .= erIndent er
    , "appended"      .= erBindingTxt er
    , "hint"          .=
        ( "The extracted binding was appended as a top-level definition. \
          \If you meant a local `where`/`let` inside a single function, \
          \undo (revert) and re-run with that scope narrower." :: Text )
    ]

--------------------------------------------------------------------------------
-- snapshot / compile / restore
--------------------------------------------------------------------------------

-- | Read the target file, call @rewrite@, and if it returns a new
-- content: stage it, compile, commit on success, restore on error.
-- The callback returns @Left errTxt@ to abort cleanly or
-- @Right (newContent, successPayload)@ to attempt the rewrite.
withSnapshot
  :: GhcSession
  -> ModulePath
  -> Bool                                    -- ^ dry_run
  -> (Text -> IO (Either Text (Text, Value)))
  -> IO ToolResult
withSnapshot sess mp dryRun cont = do
  readRes <- try (TIO.readFile (unModulePath mp))
             :: IO (Either SomeException Text)
  case readRes of
    Left e -> pure (errorResult (T.pack ("Could not read module: " <> show e)))
    Right orig -> do
      outcome <- cont orig
      case outcome of
        Left reason -> pure (errorResult reason)
        Right (newContent, baseSuccess) ->
          if dryRun
            then dryRunWithVerify sess mp orig newContent baseSuccess
            else commitWithVerify sess mp orig newContent baseSuccess

commitWithVerify
  :: GhcSession
  -> ModulePath
  -> Text           -- original file content (snapshot)
  -> Text           -- rewritten content
  -> Value          -- base success payload (augmented with compile info)
  -> IO ToolResult
commitWithVerify ghcSess mp orig newContent baseSuccess = do
  -- Issue #50: diagnostic-diff verify. Before we write the new
  -- content, load the file as-is and snapshot the *pre-existing*
  -- error set. The accept criterion then becomes \"the rewrite
  -- introduced no NEW errors\" rather than \"there are no errors
  -- at all\". A clean rename in a module that already has an
  -- unrelated typed hole used to be rolled back because the hole
  -- showed up post-edit too — that's exactly the symptom the bug
  -- describes.
  invalidateLoadCache ghcSess
  preDiags <- loadAndDiagnose ghcSess mp
  let preErrSigs = errorSignatures preDiags

  writeRes <- try (TIO.writeFile (unModulePath mp) newContent)
              :: IO (Either SomeException ())
  case writeRes of
    Left e -> pure (errorResult (T.pack ("Could not write module: " <> show e)))
    Right _ -> do
      -- Drop the auto-load cache so loadForTarget re-scans the
      -- freshly-written file instead of reusing a stale HscEnv.
      invalidateLoadCache ghcSess
      postDiags <- loadAndDiagnose ghcSess mp
      let postErrs    = filter ((== SevError) . geSeverity) postDiags
          postErrSigs = errorSignatures postDiags
          newErrSigs  = filter (`notElem` preErrSigs) postErrSigs
          -- An error is \"new\" iff its (file, line, column, message)
          -- key wasn't in preDiags. The rewrite is rejected only
          -- when at least one such entry exists.
          regressed   = not (null newErrSigs)
      if regressed
        then do
          restored <- try (TIO.writeFile (unModulePath mp) orig)
                      :: IO (Either SomeException ())
          let restoreMsg = case restored of
                Left _  -> " — AND snapshot restore ALSO failed, file is dirty"
                Right _ -> " — snapshot restored"
              -- Re-render the post-edit error set so the agent
              -- sees what tripped the rollback. We attach the new-
              -- only subset as 'new_errors' for clarity.
              newErrs = filter (\e -> errorKey e `elem` newErrSigs) postErrs
          pure (compileFailResult newErrs (renderDiags postDiags) restoreMsg)
        else
          pure (commitResultWithDiff baseSuccess preDiags postDiags)

-- | F-21: compile-verify even on @dry_run=True@.  Before this fix,
-- dry-run skipped compile-verify, so 'extractBinding' could produce
-- syntactically invalid Haskell (e.g. a @let@-clause fragment) and
-- still return @status: ok@.
--
-- 'dryRunWithVerify' writes the new content, runs the compile check,
-- then ALWAYS restores the original — it is a read-only preview that
-- also validates.  If compile fails, returns 'compileFailResult' so
-- the agent knows the patch is invalid.  If compile passes, returns
-- the usual 'dryRunResult' (no disk change visible to the user).
dryRunWithVerify
  :: GhcSession
  -> ModulePath
  -> Text           -- original file content (snapshot)
  -> Text           -- rewritten content
  -> Value          -- base success payload
  -> IO ToolResult
dryRunWithVerify ghcSess mp orig newContent baseSuccess = do
  invalidateLoadCache ghcSess
  preDiags <- loadAndDiagnose ghcSess mp
  let preErrSigs = errorSignatures preDiags
  writeRes <- try (TIO.writeFile (unModulePath mp) newContent)
              :: IO (Either SomeException ())
  case writeRes of
    Left e -> pure (errorResult (T.pack ("Could not write for dry-run verify: " <> show e)))
    Right _ -> do
      invalidateLoadCache ghcSess
      postDiags <- loadAndDiagnose ghcSess mp
      -- Always restore — this is a read-only preview.
      _ <- try (TIO.writeFile (unModulePath mp) orig) :: IO (Either SomeException ())
      invalidateLoadCache ghcSess
      let postErrs   = filter ((== SevError) . geSeverity) postDiags
          newErrSigs = filter (`notElem` preErrSigs) (errorSignatures postDiags)
          regressed  = not (null newErrSigs)
      if regressed
        then do
          let newErrs = filter (\e -> errorKey e `elem` newErrSigs) postErrs
          pure (compileFailResult newErrs (renderDiags postDiags)
                  " — dry_run, original preserved; patch is invalid")
        else pure (dryRunResult baseSuccess newContent)

-- | Issue #50: structural key for an error diagnostic. Two
-- diagnostics are \"the same\" if they refer to the same
-- (file, line, column, message). Severity is intentionally
-- excluded — a warning becoming an error at the same location
-- still counts as \"already there\". Code is also excluded:
-- typed holes carry a stable \"GHC-88464\" code but their
-- message text is what matters.
errorKey :: GhcError -> (Text, Int, Int, Text)
errorKey e = (geFile e, geLine e, geColumn e, geMessage e)

errorSignatures :: [GhcError] -> [(Text, Int, Int, Text)]
errorSignatures = map errorKey . filter ((== SevError) . geSeverity)

-- | Run @loadForTarget@ for the file's owning stanza and capture
-- whatever diagnostics come back. Exception during load → synthetic
-- error diagnostic (so the diff still works against a baseline).
loadAndDiagnose :: GhcSession -> ModulePath -> IO [GhcError]
loadAndDiagnose ghcSess mp = do
  tgt <- targetForPath ghcSess (unModulePath mp)
  eLoad <- try (loadForTarget ghcSess tgt Strict)
           :: IO (Either SomeException (Bool, [GhcError]))
  pure $ case eLoad of
    Left ex ->
      [ GhcError { geFile = T.pack (unModulePath mp)
                 , geLine = 0, geColumn = 0
                 , geSeverity = SevError
                 , geCode = Nothing
                 , geMessage = T.pack (show ex)
                 } ]
    Right (_ok, diags) -> diags

-- | Issue #50: extended success result. When the rewrite is
-- accepted, surface the pre-existing error set (if any) so the
-- agent knows the module still has known issues — distinct from
-- \"all green\".
commitResultWithDiff :: Value -> [GhcError] -> [GhcError] -> ToolResult
commitResultWithDiff base preDiags postDiags =
  let preErrs  = filter ((== SevError) . geSeverity) preDiags
      postErrs = filter ((== SevError) . geSeverity) postDiags
      compileTag :: Text
      compileTag
        | null postErrs && null preErrs = "ok"
        | null postErrs                 = "ok"  -- rewrite fixed every pre-existing error
        | otherwise                     = "ok-with-pre-existing-errors"
  -- Issue #90 Phase C: success → status='ok' with merged payload
  -- under 'result'.
  in case base of
       Object o ->
         let payload = Object (foldr (uncurry insertKV) o
               [ ("dry_run"             :: Text, toJSON False)
               , ("compile",                     toJSON compileTag)
               , ("pre_existing_errors",         toJSON preErrs)
               , ("new_errors",                  toJSON ([] :: [GhcError]))
               ])
         in Env.toolResponseToResult (Env.mkOk payload)
       _ ->
         let payload = object
               [ "summary"             .= base
               , "compile"             .= compileTag
               , "pre_existing_errors" .= preErrs
               , "new_errors"          .= ([] :: [GhcError])
               ]
         in Env.toolResponseToResult (Env.mkOk payload)

-- | Render a list of diagnostics into a single text blob matching
-- the legacy @grOutput@ shape (file:line:col: message, blank line
-- between entries).
renderDiags :: [GhcError] -> Text
renderDiags = T.intercalate "\n\n" . map one
  where
    one e =
      geFile e <> ":" <> T.pack (show (geLine e)) <> ":"
        <> T.pack (show (geColumn e)) <> ": " <> geMessage e

--------------------------------------------------------------------------------
-- response shaping
--------------------------------------------------------------------------------

-- | Issue #90 Phase C: dry-run success → status='ok' with the
-- merged base+dry-run-extras payload under 'result'.
dryRunResult :: Value -> Text -> ToolResult
dryRunResult base preview =
  let payload = case base of
        Object o ->
          Object (foldr (uncurry insertKV) o
            [ ("dry_run" :: Text, toJSON True)
            , ("preview",         toJSON preview)
            ])
        _ ->
          object
            [ "dry_run" .= True
            , "preview" .= preview
            , "summary" .= base
            ]
  in Env.toolResponseToResult (Env.mkOk payload)

-- | Aeson 2.x 'Object' is a 'KeyMap.KeyMap' — we go through 'Key.fromText'
-- so callers stay in 'Text' land and never touch the @Key@ newtype
-- directly.
insertKV :: Text -> Value -> Object -> Object
insertKV k = KeyMap.insert (Key.fromText k)

-- | Issue #90 §4: post-rewrite type-check failure maps to
-- status='failed' with kind='verify_failed'. The diagnostic detail
-- ('errors', 'raw', 'note') stays under 'result' so consumers can
-- branch per-error.
compileFailResult :: [GhcError] -> Text -> Text -> ToolResult
compileFailResult errs raw restoreMsg =
  let envErr = (Env.mkErrorEnvelope Env.VerifyFailed
                 ("Rewrite did not type-check" <> restoreMsg))
                 { Env.eeCause = Just (T.take 400 raw) }
      payload = object
        [ "dry_run" .= False
        , "compile" .= ("failed" :: Text)
        , "errors"  .= errs
        , "raw"     .= raw
        ]
      response = (Env.mkFailed envErr) { Env.reResult = Just payload }
  in Env.toolResponseToResult response

-- | Issue #90 Phase C: routed through the envelope. Most refactor
-- failures map to kind='validation' (the input was structurally
-- fine but failed a domain check — missing 'old_name', wrong
-- scope, etc.).
errorResult :: Text -> ToolResult
errorResult msg =
  Env.toolResponseToResult (Env.mkFailed
    (Env.mkErrorEnvelope Env.Validation msg))

-- | Issue #100 Phase C: 'mkModulePath' rejected the path →
-- status='refused', kind='path_traversal'.
pathTraversalResult :: Text -> ToolResult
pathTraversalResult msg =
  Env.toolResponseToResult
    (Env.mkRefused (Env.mkErrorEnvelope Env.PathTraversal msg))

formatPathError :: PathError -> Text
formatPathError = \case
  PathNotAbsolute p        -> "Project directory is not absolute: " <> p
  PathEscapesProject a p _ -> "module_path '" <> a <> "' escapes project directory " <> p

tshow :: Show a => a -> Text
tshow = T.pack . show
