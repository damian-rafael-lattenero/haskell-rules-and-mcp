-- | @ghc_scratch@ — persistent code canvas for LLM hypothesis testing.
--
-- The pizarra del LLM. Action-discriminated tool that lets the LLM:
--
--   * write a Haskell snippet under an id with optional module/imports
--   * type-check it against the live project session (no execution)
--   * list / show / clear scratchpad entries
--   * promote a verified entry into a real module via ghc_refactor
--
-- Lives next to ghc_property_store: same persistence pattern, same
-- two-layer locking, same action-dispatch shape.
--
-- Phase 1 (this file's first landing) implements the data-bound
-- actions only: 'write', 'list', 'show', 'clear'. The compile-bound
-- actions ('check', 'promote') return a structured
-- @kind:"not_implemented"@ error so the wire surface is stable from
-- day one; the next commit fills in 'check' against the GHC API
-- session and the one after wires 'promote' into 'Refactor.handle'.
module HaskellFlows.Tool.Scratch
  ( descriptor
  , handle
  , ScratchArgs (..)
  , ScratchAction (..)
    -- * Internals (exported for unit tests)
  , parseAction
  , renderEntrySummary
  , spliceInto
  ) where

import Control.Exception (SomeException, try)
import Control.Monad (unless)
import Data.Aeson
import Data.Aeson.Types (parseEither)
import Data.Maybe (fromMaybe)
import Data.Text (Text)
import qualified Data.Text as T
import Data.Time.Clock.POSIX (getPOSIXTime)
import GHC (Ghc, TcRnExprMode (TM_Inst), exprType)
import GHC.Utils.Outputable (showPprUnsafe)

import qualified HaskellFlows.Data.Scratchpad as SP
import qualified HaskellFlows.Mcp.Envelope as Env
import HaskellFlows.Ghc.ApiSession (GhcSession, withGhcSession)
import HaskellFlows.Ghc.Sanitize (sanitizeExpression)
import HaskellFlows.Mcp.ParseError (formatParseError)
import HaskellFlows.Mcp.Protocol
import HaskellFlows.Mcp.ToolName (ToolName (..), toolNameText)
import qualified HaskellFlows.Tool.Refactor as Refactor
import HaskellFlows.Types
  ( PathError (..)
  , ProjectDir
  , mkModulePath
  )

--------------------------------------------------------------------------------
-- Tool descriptor (canonical 6-field shape per docs/TOOL_DESCRIPTION_TEMPLATE.md)
--------------------------------------------------------------------------------

descriptor :: ToolDescriptor
descriptor =
  ToolDescriptor
    { tdName        = toolNameText GhcScratch
    , tdDescription = T.unlines
        [ "PURPOSE: Persistent code canvas — write Haskell hypotheses,"
        , "type-check them in project context, promote verified ones into source."
        , "WHEN: action=\"write\" — record a snippet under an id;"
        , "action=\"check\" — type-check without executing;"
        , "action=\"list\" — see the open hypotheses;"
        , "action=\"show\" — inspect one entry in detail;"
        , "action=\"clear\" — drop an entry (or all with confirm=true);"
        , "action=\"promote\" — splice verified code into a real module"
        , "via ghc_refactor's snapshot-and-compile-verify."
        , "WHEN NOT: ghc_eval for one-shot expression evaluation without"
        , "persistence; ghc_refactor when you already know the code"
        , "compiles and just want to move it."
        , "PREREQUISITES: a loaded module (for type-context) — call"
        , "ghc_load first when the snippet refers to project symbols."
        , "OUTPUT: per-action shapes detailed in the JSON schema branches"
        , "below; every successful response carries the affected entry's"
        , "id plus its status."
        , "SEE ALSO: ghc_eval, ghc_refactor, ghc_property_store."
        ]
    , tdInputSchema =
        object
          [ "type"       .= ("object" :: Text)
          , "properties" .= object
              [ "action" .= object
                  [ "type"        .= ("string" :: Text)
                  , "enum"        .= (["write","check","list","show","clear","promote"] :: [Text])
                  , "description" .=
                      ("Which scratchpad action to perform. Default: list." :: Text)
                  ]
              , "id" .= object
                  [ "type"        .= ("string" :: Text)
                  , "description" .=
                      ("Entry id. Required for check/show/promote. Optional \
                       \for write (auto-generated when omitted). Required \
                       \for clear unless confirm=true is set." :: Text)
                  ]
              , "code" .= object
                  [ "type"        .= ("string" :: Text)
                  , "description" .=
                      ("Haskell snippet. Required for write. Sanitized via \
                       \the same boundary that ghc_eval uses." :: Text)
                  ]
              , "module" .= object
                  [ "type"        .= ("string" :: Text)
                  , "description" .=
                      ("Optional module path anchoring this entry's \
                       \type-check context (e.g. \"src/Foo.hs\")." :: Text)
                  ]
              , "imports" .= object
                  [ "type"        .= ("array" :: Text)
                  , "items"       .= object [ "type" .= ("string" :: Text) ]
                  , "description" .=
                      ("Per-entry extra imports applied before type-check." :: Text)
                  ]
              , "kind" .= object
                  [ "type"        .= ("string" :: Text)
                  , "enum"        .= (["hypothesis","type_check","eval","note"] :: [Text])
                  , "description" .=
                      ("Classification — defaults to hypothesis." :: Text)
                  ]
              , "note" .= object
                  [ "type"        .= ("string" :: Text)
                  , "description" .=
                      ("Natural-language annotation explaining why this \
                       \entry exists. Visible to the user." :: Text)
                  ]
              , "confirm" .= object
                  [ "type"        .= ("boolean" :: Text)
                  , "description" .=
                      ("Required for action=clear without an id (truncates \
                       \the whole scratchpad)." :: Text)
                  ]
              , "target_module" .= object
                  [ "type"        .= ("string" :: Text)
                  , "description" .=
                      ("Where to splice on promote (e.g. \"src/Foo.hs\")." :: Text)
                  ]
              , "target_line" .= object
                  [ "type"        .= ("integer" :: Text)
                  , "description" .=
                      ("1-based line in target_module where the binding \
                       \should be spliced on promote." :: Text)
                  ]
              ]
          , "required"             .= ([] :: [Text])
          , "additionalProperties" .= False
          ]
    }

--------------------------------------------------------------------------------
-- Action ADT + arg parsing
--------------------------------------------------------------------------------

data ScratchAction
  = ActWrite
  | ActCheck
  | ActList
  | ActShow
  | ActClear
  | ActPromote
  deriving stock (Eq, Show)

parseAction :: Maybe Text -> Either Text ScratchAction
parseAction = \case
  Nothing         -> Right ActList
  Just "write"    -> Right ActWrite
  Just "check"    -> Right ActCheck
  Just "list"     -> Right ActList
  Just "show"     -> Right ActShow
  Just "clear"    -> Right ActClear
  Just "promote"  -> Right ActPromote
  Just other      -> Left ("unknown action: " <> other)

data ScratchArgs = ScratchArgs
  { saAction       :: !ScratchAction
  , saId           :: !(Maybe Text)
  , saCode         :: !(Maybe Text)
  , saModule       :: !(Maybe Text)
  , saImports      :: ![Text]
  , saKind         :: !(Maybe SP.ScratchKind)
  , saNote         :: !(Maybe Text)
  , saConfirm      :: !Bool
  , saTargetModule :: !(Maybe Text)
  , saTargetLine   :: !(Maybe Int)
  }
  deriving stock (Show)

instance FromJSON ScratchArgs where
  parseJSON = withObject "ScratchArgs" $ \o -> do
    mAction <- o .:? "action"
    act <- case parseAction mAction of
      Right a  -> pure a
      Left err -> fail (T.unpack err)
    i  <- o .:? "id"
    c  <- o .:? "code"
    m  <- o .:? "module"
    is <- o .:? "imports" .!= []
    k  <- o .:? "kind"
    n  <- o .:? "note"
    cf <- o .:? "confirm" .!= False
    tm <- o .:? "target_module"
    tl <- o .:? "target_line"
    pure ScratchArgs
      { saAction       = act
      , saId           = i
      , saCode         = c
      , saModule       = m
      , saImports      = is
      , saKind         = k
      , saNote         = n
      , saConfirm      = cf
      , saTargetModule = tm
      , saTargetLine   = tl
      }

--------------------------------------------------------------------------------
-- Handler
--------------------------------------------------------------------------------

-- | Phase 1 threaded only the 'SP.Store'. Phase 2 adds 'GhcSession'
-- so 'action=check' can call @exprType@ against the live GHC API
-- session. Phase 4 adds 'ProjectDir' so 'action=promote' can build
-- a 'ModulePath' for the splice target.
--
-- The session and pd are lazy in all non-check / non-promote branches
-- so callers may safely pass 'undefined' when they know neither
-- 'check' nor 'promote' will run (unit tests for write/list/show/clear).
handle :: SP.Store -> GhcSession -> ProjectDir -> Value -> IO ToolResult
handle store ghcSess pd rawArgs = case parseEither parseJSON rawArgs of
  Left err -> pure (formatParseError err)
  Right args -> case saAction args of
    ActWrite   -> handleWrite store args
    ActList    -> handleList store
    ActShow    -> handleShow store args
    ActClear   -> handleClear store args
    ActCheck   -> handleCheck store ghcSess args
    ActPromote -> handlePromote store ghcSess pd args

--------------------------------------------------------------------------------
-- check (#253 Phase 2)
--------------------------------------------------------------------------------

-- | Type-check the stored snippet against the live GHC API session.
--
-- Steps:
--   1. Require an @id@.
--   2. Look up the entry — return @no_match@ if it doesn't exist.
--   3. Run 'sanitizeExpression' on the stored code (same gate as
--      @ghc_eval@ and @ghc_type@; rejects newlines, sentinel,
--      empty, oversized, large-integer literals).
--   4. Call @exprType TM_Inst@ inside 'withGhcSession'.
--   5. On type_ok: persist 'ScratchVerified' + the inferred type.
--      On type_error: persist the error text, leave status 'ScratchOpen'
--      so the LLM can rewrite and re-check.
--
-- Both outcomes are returned as @status=\"ok\"@ with @kind@ at the
-- top level of the result object so @nextStep@ (which drills one
-- level via @envField \"result\"@) can route: @type_ok@ → promote,
-- @type_error@ → write + re-check.  The full 'SP.ScratchResult' is
-- persisted to the store and visible via @action=show@; the response
-- carries only the fields needed for immediate routing.
handleCheck :: SP.Store -> GhcSession -> ScratchArgs -> IO ToolResult
handleCheck store ghcSess args = case saId args of
  Nothing ->
    pure (Env.toolResponseToResult (Env.mkFailed
      (Env.mkErrorEnvelope Env.MissingArg
        "action=check requires 'id'")))
  Just i -> do
    mEntry <- SP.findById store i
    case mEntry of
      Nothing ->
        pure (Env.toolResponseToResult (Env.mkNoMatch (object
          [ "id"    .= i
          , "found" .= False
          , "hint"  .= ("No scratchpad entry with that id. \
                        \Use action=list to see existing ids." :: Text)
          ])))
      Just entry ->
        case sanitizeExpression (SP.seCode entry) of
          Left cmdErr ->
            pure (Env.toolResponseToResult (Env.mkRefused
              (Env.sanitizeRejection "code" cmdErr)))
          Right safe -> do
            now <- realToFrac <$> getPOSIXTime
            eRes <- try (withGhcSession ghcSess (queryExprType safe))
            case eRes :: Either SomeException Text of
              Right typeText -> do
                let result  = SP.ScratchResult
                                { SP.srKind   = "type_ok"
                                , SP.srDetail = typeText
                                , SP.srAt     = now
                                }
                    updated = entry
                                { SP.seResult  = Just result
                                , SP.seStatus  = SP.ScratchVerified
                                , SP.seUpdated = now
                                }
                SP.save store updated
                -- F-01 fix: 'kind' at the top level of the mkOk payload so
                -- 'scratchNext' (envField "result" → lookup "kind") routes to
                -- promote without drilling a second level.
                pure (Env.toolResponseToResult (Env.mkOk (object
                  [ "id"     .= i
                  , "status" .= SP.seStatus updated   -- "verified"
                  , "kind"   .= SP.srKind result      -- "type_ok"
                  , "type"   .= typeText
                  , "hint"   .=
                      ("Type checks! Use action=promote to splice this \
                       \code into a real module, or action=show to see \
                       \the persisted result." :: Text)
                  ])))
              Left ex -> do
                let errText = T.pack (show ex)
                    result  = SP.ScratchResult
                                { SP.srKind   = "type_error"
                                , SP.srDetail = errText
                                , SP.srAt     = now
                                }
                    updated = entry
                                { SP.seResult  = Just result
                                , SP.seStatus  = SP.ScratchOpen
                                , SP.seUpdated = now
                                }
                SP.save store updated
                -- F-01 fix: same pattern — 'kind' at top level for routing.
                pure (Env.toolResponseToResult (Env.mkOk (object
                  [ "id"         .= i
                  , "status"     .= SP.seStatus updated   -- "open"
                  , "kind"       .= SP.srKind result      -- "type_error"
                  , "type_error" .= errText
                  , "hint"       .=
                      ("Type error. Use action=write to fix the code \
                       \under the same id, then run action=check again." :: Text)
                  ])))

-- | Issue @:t expr@ inside an active 'GhcSession'.  Mirrors
-- 'HaskellFlows.Tool.Type.queryExprType' — kept local to avoid a
-- cross-tool import dependency.
queryExprType :: Text -> Ghc Text
queryExprType safe = do
  ty <- exprType TM_Inst (T.unpack safe)
  pure (T.pack (showPprUnsafe ty))

--------------------------------------------------------------------------------
-- write
--------------------------------------------------------------------------------

handleWrite :: SP.Store -> ScratchArgs -> IO ToolResult
handleWrite store args = case saCode args of
  Nothing ->
    pure (Env.toolResponseToResult (Env.mkFailed
      (Env.mkErrorEnvelope Env.MissingArg
        "action=write requires 'code'")))
  Just code -> do
    now <- realToFrac <$> getPOSIXTime
    entryId <- case saId args of
      Just i  -> pure i
      Nothing -> autoId store now
    let entry = SP.ScratchEntry
          { SP.seId      = entryId
          , SP.seKind    = fromMaybe SP.ScratchHypothesis (saKind args)
          , SP.seCode    = code
          , SP.seModule  = saModule args
          , SP.seImports = saImports args
          , SP.seNote    = saNote args
          , SP.seResult  = Nothing
          , SP.seStatus  = SP.ScratchOpen
          , SP.seCreated = now
          , SP.seUpdated = now
          }
    SP.save store entry
    pure (Env.toolResponseToResult (Env.mkOk (object
      [ "id"     .= entryId
      , "kind"   .= SP.seKind entry
      , "status" .= SP.seStatus entry
      , "hint"   .= ("Entry persisted. Use action=check to type-check it \
                     \against the live session, or action=promote when verified." :: Text)
      ])))

-- | Auto-generate an entry id based on the existing entry count.
-- Format: @scratch-N@ where N is the smallest unused integer.
autoId :: SP.Store -> Double -> IO Text
autoId store _ = do
  existing <- SP.loadAll store
  let used = [n | e <- existing
                , Just n <- [parseAutoId (SP.seId e)]]
      next = if null used then 1 else maximum used + 1
  pure ("scratch-" <> T.pack (show next))
  where
    parseAutoId :: Text -> Maybe Int
    parseAutoId t = case T.stripPrefix "scratch-" t of
      Just suffix -> case reads (T.unpack suffix) of
        [(n, "")] -> Just n
        _         -> Nothing
      Nothing -> Nothing

--------------------------------------------------------------------------------
-- list
--------------------------------------------------------------------------------

handleList :: SP.Store -> IO ToolResult
handleList store = do
  entries <- SP.loadAll store
  let summaries = map renderEntrySummary entries
      counts    = tallyStatuses entries
  pure (Env.toolResponseToResult (Env.mkOk (object
    [ "count"   .= length entries
    , "entries" .= summaries
    , "counts"  .= counts
    , "hint"    .= listHint entries
    ])))

-- | Per-entry compact summary for the list view. Code is preview-only
-- (first 80 chars) so the response stays small even when entries hold
-- multi-line snippets.
renderEntrySummary :: SP.ScratchEntry -> Value
renderEntrySummary e =
  let preview = T.take 80 (SP.seCode e)
      truncated = T.length (SP.seCode e) > 80
  in object
       [ "id"           .= SP.seId e
       , "kind"         .= SP.seKind e
       , "status"       .= SP.seStatus e
       , "module"       .= SP.seModule e
       , "code_preview" .= (if truncated then preview <> "…" else preview)
       , "updated"      .= SP.seUpdated e
       ]

tallyStatuses :: [SP.ScratchEntry] -> Value
tallyStatuses es =
  object
    [ "open"      .= length (filter ((== SP.ScratchOpen)      . SP.seStatus) es)
    , "verified"  .= length (filter ((== SP.ScratchVerified)  . SP.seStatus) es)
    , "promoted"  .= length (filter ((== SP.ScratchPromoted)  . SP.seStatus) es)
    , "abandoned" .= length (filter ((== SP.ScratchAbandoned) . SP.seStatus) es)
    ]

listHint :: [SP.ScratchEntry] -> Text
listHint [] =
  "Scratchpad is empty. Use action=write to record a Haskell hypothesis \
  \you want to type-check before touching source."
listHint xs =
  let open = length (filter ((== SP.ScratchOpen) . SP.seStatus) xs)
  in if open > 0
       then T.pack (show open) <> " entries are still Open — run action=check \
                                   \to type-check them against the live session."
       else "Every entry is Verified, Promoted, or Abandoned. \
            \Use action=show to inspect one, or action=clear with confirm=true \
            \to drop the whole scratchpad."

--------------------------------------------------------------------------------
-- show
--------------------------------------------------------------------------------

handleShow :: SP.Store -> ScratchArgs -> IO ToolResult
handleShow store args = case saId args of
  Nothing ->
    pure (Env.toolResponseToResult (Env.mkFailed
      (Env.mkErrorEnvelope Env.MissingArg
        "action=show requires 'id'")))
  Just i -> do
    mEntry <- SP.findById store i
    case mEntry of
      Nothing ->
        pure (Env.toolResponseToResult (Env.mkNoMatch (object
          [ "id"    .= i
          , "found" .= False
          , "hint"  .= ("No scratchpad entry with that id. \
                        \Use action=list to see existing ids." :: Text)
          ])))
      Just e ->
        pure (Env.toolResponseToResult (Env.mkOk (toJSON e)))

--------------------------------------------------------------------------------
-- clear
--------------------------------------------------------------------------------

handleClear :: SP.Store -> ScratchArgs -> IO ToolResult
handleClear store args = case (saId args, saConfirm args) of
  (Just i, _) -> do
    -- Single-entry remove. No confirm needed — caller named the id.
    mEntry <- SP.findById store i
    case mEntry of
      Nothing ->
        pure (Env.toolResponseToResult (Env.mkNoMatch (object
          [ "id"      .= i
          , "removed" .= False
          , "hint"    .= ("No entry with that id; nothing to remove." :: Text)
          ])))
      Just _ -> do
        SP.remove store i
        pure (Env.toolResponseToResult (Env.mkOk (object
          [ "id"      .= i
          , "removed" .= True
          ])))
  (Nothing, True) -> do
    -- Bulk truncate.
    SP.clearAll store
    pure (Env.toolResponseToResult (Env.mkOk (object
      [ "cleared" .= True
      , "hint"    .= ("Scratchpad truncated." :: Text)
      ])))
  (Nothing, False) ->
    pure (Env.toolResponseToResult (Env.mkRefused
      (Env.mkErrorEnvelope Env.Validation
        "action=clear without 'id' requires confirm=true to drop the whole scratchpad")))

--------------------------------------------------------------------------------
-- promote (#253 Phase 4)
--------------------------------------------------------------------------------

-- | Splice the stored code into 'target_module' at 'target_line' (or
-- the end of the file when 'target_line' is omitted), then verify
-- the module still compiles via 'Refactor.withSnapshot'.
--
-- On success   : entry status → 'SP.ScratchPromoted' + module recorded.
-- On compile fail: 'Refactor.withSnapshot' restores the original file
--   atomically; entry stays 'SP.ScratchOpen'; the GHC error text is
--   surfaced so the LLM can fix the snippet.
--
-- Prerequisites:
--   * 'id' — required; lookup fails with no_match if not found.
--   * 'target_module' — required; must be in-project (path-traversal guard).
--   * 'target_line' — optional; inserts after that line (1-based) when given,
--     appends to end of file otherwise.
--   * A loaded GHC session (caller must have run ghc_load first).
handlePromote :: SP.Store -> GhcSession -> ProjectDir -> ScratchArgs -> IO ToolResult
handlePromote store ghcSess pd args =
  case saId args of
    Nothing ->
      pure (Env.toolResponseToResult (Env.mkFailed
        (Env.mkErrorEnvelope Env.MissingArg
          "action=promote requires 'id'")))
    Just i ->
      case saTargetModule args of
        Nothing ->
          pure (Env.toolResponseToResult (Env.mkFailed
            (Env.mkErrorEnvelope Env.MissingArg
              "action=promote requires 'target_module' \
              \(e.g. \"src/Foo.hs\") — the file where the code will be spliced.")))
        Just rawModule ->
          case mkModulePath pd (T.unpack rawModule) of
            Left pe ->
              pure (Env.toolResponseToResult (Env.mkRefused
                (Env.mkErrorEnvelope Env.PathTraversal
                  (renderPathErr pe))))
            Right mp -> do
              mEntry <- SP.findById store i
              case mEntry of
                Nothing ->
                  pure (Env.toolResponseToResult (Env.mkNoMatch (object
                    [ "id"    .= i
                    , "found" .= False
                    , "hint"  .= ("No scratchpad entry with that id. \
                                  \Use action=list to see existing ids." :: Text)
                    ])))
                Just entry -> do
                  now <- realToFrac <$> getPOSIXTime
                  let spliceCode  = SP.seCode entry
                      targetLine  = saTargetLine args
                      successBase = object
                        [ "id"            .= i
                        , "target_module" .= rawModule
                        , "kind"          .= ("promoted" :: Text)
                        , "hint"          .=
                            ("Code spliced and verified. \
                             \Entry status is now 'promoted'." :: Text)
                        ]
                  result <- Refactor.withSnapshot ghcSess mp False $ \orig ->
                    pure (Right (spliceInto orig spliceCode targetLine, successBase))
                  -- Only promote the entry if the refactor succeeded.
                  -- trIsError=True means the snapshot was rolled back.
                  unless (trIsError result) $ do
                    let promoted = entry
                          { SP.seStatus  = SP.ScratchPromoted
                          , SP.seModule  = Just rawModule
                          , SP.seUpdated = now
                          }
                    SP.save store promoted
                  pure result

-- | Splice @code@ into @orig@ at @targetLine@ (1-based, insert after
-- that line) or append to the end when 'Nothing'.
--
-- Exported for unit tests so the splice logic can be verified
-- independently of the GHC compile step.
spliceInto :: Text  -- ^ original file content
           -> Text  -- ^ code to insert
           -> Maybe Int  -- ^ 1-based line number to insert after (Nothing = append)
           -> Text
spliceInto orig code Nothing =
  -- Append at end with a blank-line separator so the new binding
  -- starts on its own visual paragraph.
  T.stripEnd orig <> "\n\n" <> code <> "\n"
spliceInto orig code (Just lineN) =
  let ls           = T.lines orig
      n            = max 0 (min lineN (length ls))
      (before, after) = splitAt n ls
  in T.unlines before <> "\n" <> code <> "\n" <> T.unlines after

-- | Render a 'PathError' as a human-readable refusal message.
renderPathErr :: PathError -> Text
renderPathErr = \case
  PathNotAbsolute p      -> "target_module must be a relative path under the project, got: " <> p
  PathEscapesProject a p _ -> "target_module escapes the project root: '" <> a
                               <> "' is not under '" <> p <> "'"
