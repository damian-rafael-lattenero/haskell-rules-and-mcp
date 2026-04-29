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
import qualified Data.Text.Lazy as TL
import qualified Data.Text.Lazy.Encoding as TLE

import HaskellFlows.Ghc.ApiSession
  ( GhcSession
  , LoadFlavour (..)
  , invalidateLoadCache
  , loadForTarget
  , targetForPath
  )
import qualified HaskellFlows.Mcp.Envelope as Env
import HaskellFlows.Mcp.Protocol
import HaskellFlows.Mcp.ToolName (ToolName (..), toolNameText)
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
        "Small-scope refactors with snapshot-and-compile safety. "
          <> "Actions: 'rename_local' (scoped identifier rename), "
          <> "'extract_binding' (lift a line range to a named top-level "
          <> "binding). If GHCi reports a compile error on the rewrite, "
          <> "the file is restored from snapshot — the refactor is "
          <> "atomic from the agent's perspective."
    , tdInputSchema = schema
    }

schema :: Value
schema = object
  [ "type"       .= ("object" :: Text)
  , "properties" .= object
      [ "action" .= object
          [ "type" .= ("string" :: Text)
          , "enum" .= (["rename_local", "extract_binding"] :: [Text])
          ]
      , "module_path" .= object
          [ "type"        .= ("string" :: Text)
          , "description" .= ("Relative module path." :: Text)
          ]
      , "old_name" .= object
          [ "type"        .= ("string" :: Text)
          , "description" .=
              ("Identifier to replace. Required for rename_local."
               :: Text)
          ]
      , "new_name" .= object
          [ "type"        .= ("string" :: Text)
          , "description" .=
              ("Replacement (rename_local) or new binding name \
               \(extract_binding)." :: Text)
          ]
      , "scope_line_start" .= object
          [ "type"        .= ("integer" :: Text)
          , "description" .= ("Inclusive 1-based start line." :: Text)
          ]
      , "scope_line_end" .= object
          [ "type"        .= ("integer" :: Text)
          , "description" .= ("Inclusive 1-based end line." :: Text)
          ]
      , "dry_run" .= object
          [ "type"        .= ("boolean" :: Text)
          , "description" .=
              ("If true, compute the rewrite and return without \
               \touching disk. Default: false." :: Text)
          ]
      ]
  , "required"             .= (["action", "module_path", "new_name"] :: [Text])
  , "additionalProperties" .= False
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

instance FromJSON RefactorArgs where
  parseJSON = withObject "RefactorArgs" $ \o -> do
    a   <- o .:  "action"
    mp  <- o .:  "module_path"
    old <- o .:? "old_name"
    new <- o .:  "new_name"
    ls  <- o .:? "scope_line_start"
    le  <- o .:? "scope_line_end"
    dr  <- o .:? "dry_run" .!= False
    act <- case (a :: Text) of
      "rename_local"    -> pure ActRename
      "extract_binding" -> pure ActExtract
      other             -> fail ("unknown action: " <> T.unpack other)
    pure RefactorArgs
      { raAction         = act
      , raModulePath     = mp
      , raOldName        = old
      , raNewName        = new
      , raScopeLineStart = ls
      , raScopeLineEnd   = le
      , raDryRun         = dr
      }

handle :: GhcSession -> ProjectDir -> Value -> IO ToolResult
handle ghcSess pd rawArgs = case parseEither parseJSON rawArgs of
  Left parseError ->
    pure (errorResult (T.pack ("Invalid arguments: " <> parseError)))
  Right args -> case mkModulePath pd (T.unpack (raModulePath args)) of
    Left e   -> pure (errorResult (formatPathError e))
    Right mp -> do
      r <- handleAction ghcSess mp args
      invalidateLoadCache ghcSess
      pure r

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
            then pure (dryRunResult baseSuccess newContent)
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
-- scope, etc.). Path-traversal cases still emit kind='path_traversal'
-- via the matching path in 'handle'.
errorResult :: Text -> ToolResult
errorResult msg =
  Env.toolResponseToResult (Env.mkFailed
    (Env.mkErrorEnvelope Env.Validation msg))

formatPathError :: PathError -> Text
formatPathError = \case
  PathNotAbsolute p        -> "Project directory is not absolute: " <> p
  PathEscapesProject a p _ -> "module_path '" <> a <> "' escapes project directory " <> p

tshow :: Show a => a -> Text
tshow = T.pack . show

encodeUtf8Text :: Value -> Text
encodeUtf8Text = TL.toStrict . TLE.decodeUtf8 . encode
