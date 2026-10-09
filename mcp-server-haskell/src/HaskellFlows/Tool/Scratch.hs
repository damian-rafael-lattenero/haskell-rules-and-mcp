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
  ( handle
  , runHandle
  , ScratchArgs (..)
  , ScratchAction (..)
    -- * Backend queries (W6.4)
  , ScratchQueries (..)
  , legacyQueries
  , queryExprTypeWithImports
  , runDeclsWithImports
    -- * Internals (exported for unit tests)
  , parseAction
  , renderEntrySummary
  , spliceInto
  , wrapAsLetBlock
  , splitImports
  , hasTopLevelDecl
  ) where

import Control.Exception (SomeException, try)
import Control.Monad (unless)
import Data.Aeson
import Data.Aeson.Types (parseEither)
import Data.Maybe (fromMaybe)
import Data.Text (Text)
import qualified Data.Text as T
import Data.Time.Clock.POSIX (getPOSIXTime)
import GHC
  ( Ghc
  , InteractiveImport (IIDecl)
  , TcRnExprMode (TM_Inst)
  , exprType
  , getContext
  , parseImportDecl
  , runDecls
  , setContext
  )
import GHC.Utils.Outputable (showPprUnsafe)

import qualified HaskellFlows.Data.Scratchpad as SP
import HaskellFlows.Mcp.Envelope (ToolResponse)
import qualified HaskellFlows.Mcp.Envelope as Env
import HaskellFlows.Ghc.ApiSession (GhcSession, withGhcSession)
import HaskellFlows.Ghc.Sanitize (sanitizeDeclarations, sanitizeExpression)
import HaskellFlows.Mcp.ParseError (formatParseError)
import HaskellFlows.Mcp.Protocol
import HaskellFlows.Mcp.ToolName (ToolName (..), toolNameText)
import qualified HaskellFlows.Tool.Refactor as Refactor
import HaskellFlows.Tool.Env (ToolEnv (..))
import HaskellFlows.Types
  ( ModulePath
  , PathError (..)
  , ProjectDir
  , mkModulePath
  )

--------------------------------------------------------------------------------
-- Tool descriptor (canonical 6-field shape per docs/TOOL_DESCRIPTION_TEMPLATE.md)
--------------------------------------------------------------------------------


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
  , saBindingName  :: !(Maybe Text)
  -- ^ F-04: when the stored code is a single-line expression, wrap it
  -- as @binding_name = code@ before splicing so the result is a valid
  -- top-level declaration.
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
    bn <- o .:? "binding_name"
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
      , saBindingName  = bn
      }

--------------------------------------------------------------------------------
-- Handler
--------------------------------------------------------------------------------

-- | Phase 1 threaded only the 'SP.Store'. Phase 2 added 'GhcSession'
-- so 'action=check' can call @exprType@ against the live GHC API
-- session. Phase 4 added 'ProjectDir' so 'action=promote' can build
-- a 'ModulePath' for the splice target.
--
-- W6.4 replaces the live 'GhcSession' with 'ScratchQueries' — the
-- backend-neutral check/promote surface. The legacy handler builds it
-- closing over 'withGhcSession'; the ghcide backend builds it closing
-- over 'IdeSession.ideInteractiveIn' and a diagnostics-diff snapshot.
-- The session is lazy in all non-check / non-promote branches so
-- callers may safely pass 'undefined' when they know neither 'check'
-- nor 'promote' will run (unit tests for write/list/show/clear).
handle :: ToolEnv -> Value -> IO ToolResponse
handle env rawArgs = do
  scratch <- teScratchpad env
  ghcSess <- teSession env
  pd      <- teProjectDir env
  runHandle scratch (legacyQueries ghcSess) pd rawArgs

-- | Backend-neutral GHC queries the check/promote actions need.
--
-- * @sqExprType hint imports expr@ — type-check an expression (or a
--   @let … in ()@-wrapped declaration block) with @imports@ spliced
--   into the interactive context. @hint@ is the entry's module hint
--   (a project-relative path): backends that anchor per-module use it
--   to pick the evaluation context; the legacy session ignores it.
--
-- * @sqRunDecls hint imports code@ — compile a top-level declaration
--   block (the 'runDecls' path GHCi uses for prompt input).
--
-- * @sqPromote mp cont@ — snapshot-and-compile-verify splice:
--   @cont orig@ returns the new content + success payload; a compile
--   regression rolls the file back and returns the error envelope.
data ScratchQueries = ScratchQueries
  { sqExprType :: Maybe Text -> [Text] -> Text -> IO (Either Text Text)
  , sqRunDecls :: Maybe Text -> [Text] -> Text -> IO (Either Text Text)
  , sqPromote  :: ModulePath
               -> (Text -> IO (Either Text (Text, Value)))
               -> IO ToolResponse
  }

-- | The legacy 'ApiSession' wiring: every query runs against the one
-- global session (the module hint is irrelevant — the session holds
-- the whole loaded graph).
legacyQueries :: GhcSession -> ScratchQueries
legacyQueries ghcSess = ScratchQueries
  { sqExprType = \_hint imports expr ->
      renderExc (withGhcSession ghcSess (queryExprTypeWithImports imports expr))
  , sqRunDecls = \_hint imports code ->
      renderExc (withGhcSession ghcSess (runDeclsWithImports imports code))
  , sqPromote  = \mp cont -> Refactor.withSnapshot ghcSess mp False cont
  }
  where
    renderExc :: IO Text -> IO (Either Text Text)
    renderExc act =
      either (Left . T.pack . show) Right
        <$> (try act :: IO (Either SomeException Text))

runHandle :: SP.Store -> ScratchQueries -> ProjectDir -> Value -> IO ToolResponse
runHandle store q pd rawArgs = case parseEither parseJSON rawArgs of
  Left err -> pure (formatParseError err)
  Right args -> case saAction args of
    ActWrite   -> handleWrite store args
    ActList    -> handleList store
    ActShow    -> handleShow store args
    ActClear   -> handleClear store args
    ActCheck   -> handleCheck store q args
    ActPromote -> handlePromote store q pd args

--------------------------------------------------------------------------------
-- check (#253 Phase 2, F-03 multi-line fix)
--------------------------------------------------------------------------------

-- | Type-check the stored snippet against the live GHC API session.
--
-- Dispatches on whether the stored code is single-line or multi-line:
--
-- * Single-line — sanitized via 'sanitizeExpression' (rejects newlines,
--   sentinel, oversized, large-int literals) then passed to @exprType
--   TM_Inst@.  The inferred type is returned as @\"type\"@.
--
-- * Multi-line — sanitized via 'sanitizeDeclarations' (same checks
--   minus the newline rejection), then wrapped in
--   @let \\n  <decls>\\n in ()@ and passed to @exprType@.  This lets
--   GHC's layout rule handle guards, multi-equation definitions, and
--   type signatures naturally.  The returned type is always @()@;
--   @\"type\"@ is set to @\"declarations type-checked OK\"@ instead so
--   the caller sees a human-readable confirmation rather than @()@.
--
-- Both outcomes surface @kind@ at the top level of the result object
-- so @nextStep@ can route: @type_ok@ → promote, @type_error@ →
-- write + re-check.  The full 'SP.ScratchResult' is persisted and
-- visible via @action=show@.
handleCheck :: SP.Store -> ScratchQueries -> ScratchArgs -> IO ToolResponse
handleCheck store q args = case saId args of
  Nothing ->
    pure (Env.mkFailed
      (Env.mkErrorEnvelope Env.MissingArg
        "action=check requires 'id'"))
  Just i -> do
    mEntry <- SP.findById store i
    case mEntry of
      Nothing ->
        pure (Env.mkNoMatch (object
          [ "id"    .= i
          , "found" .= False
          , "hint"  .= ("No scratchpad entry with that id. \
                        \Use action=list to see existing ids." :: Text)
          ]))
      Just entry -> do
        now <- realToFrac <$> getPOSIXTime
        -- #276: split inline `import …` lines out of the snippet. They can't
        -- live inside the `let … in ()` wrapper (parse error on `import`), so
        -- we parse them into the interactive context instead. Per-entry
        -- 'seImports' (previously ignored by check) are honoured here too.
        let code                   = SP.seCode entry
            (inlineImports, body)  = splitImports code
            imports                = SP.seImports entry ++ inlineImports
            decl                   = T.strip body
        if T.null decl
          then checkImportsOnly store q entry i imports now
          -- #294: type/data/newtype/class/instance declarations cannot
          -- live inside the `let … in ()` wrapper (GHC: "parse error on
          -- input 'type'"). Route them — and any block that opens with a
          -- top-level declaration keyword — through runDecls, which is the
          -- exact mechanism GHCi uses for top-level prompt input. This also
          -- lifts the where-clause limitation the let-wrapper had.
          else if hasTopLevelDecl body
            then checkDecls store q entry i imports body now
            else if T.any (== '\n') body
              then checkMultiLine store q entry i imports body now
              else checkSingleLine store q entry i imports body now

-- | Single-line path: uses 'sanitizeExpression' + 'exprType' directly.
-- Returns the inferred type as @\"type\"@ in the response.
checkSingleLine :: SP.Store -> ScratchQueries -> SP.ScratchEntry
                -> Text -> [Text] -> Text -> Double -> IO ToolResponse
checkSingleLine store q entry i imports code now =
  case sanitizeExpression code of
    Left cmdErr ->
      pure (Env.mkRefused
        (Env.sanitizeRejection "code" cmdErr))
    Right safe -> do
      eRes <- sqExprType q (SP.seModule entry) imports safe
      saveAndRespond store entry now eRes
        (\typeText -> object
          [ "id"     .= i
          , "status" .= SP.ScratchVerified
          , "kind"   .= ("type_ok" :: Text)
          , "module" .= SP.seModule entry   -- #274: lets nextStep resolve target_module
          , "type"   .= typeText
          , "hint"   .=
              ("Type checks! Use action=promote to splice this code \
               \into a real module, or action=show to see the \
               \persisted result." :: Text)
          ])
        (\errText -> object
          [ "id"         .= i
          , "status"     .= SP.ScratchOpen
          , "kind"       .= ("type_error" :: Text)
          , "type_error" .= errText
          , "hint"       .=
              ("Type error. Use action=write to fix the code under \
               \the same id, then run action=check again." :: Text)
          ])

-- | Multi-line path (F-03): uses 'sanitizeDeclarations' + wraps code
-- in @let ... in ()@ so GHC's layout rule handles guards and
-- multi-equation definitions.  Returns @\"type\": \"declarations
-- type-checked OK\"@ on success.
checkMultiLine :: SP.Store -> ScratchQueries -> SP.ScratchEntry
               -> Text -> [Text] -> Text -> Double -> IO ToolResponse
checkMultiLine store q entry i imports code now =
  case sanitizeDeclarations code of
    Left cmdErr ->
      pure (Env.mkRefused
        (Env.sanitizeRejection "code" cmdErr))
    Right safe -> do
      eRes <- sqExprType q (SP.seModule entry) imports (wrapAsLetBlock safe)
      saveAndRespond store entry now (fmap (const declOkMsg) eRes)
        (\_ -> object
          [ "id"     .= i
          , "status" .= SP.ScratchVerified
          , "kind"   .= ("type_ok" :: Text)
          , "module" .= SP.seModule entry   -- #274: lets nextStep resolve target_module
          , "type"   .= declOkMsg
          , "hint"   .=
              ("Declarations compile. Use action=promote to splice them \
               \into a target module, or action=show to see the \
               \persisted result." :: Text)
          ])
        (\errText -> object
          [ "id"         .= i
          , "status"     .= SP.ScratchOpen
          , "kind"       .= ("type_error" :: Text)
          , "type_error" .= errText
          , "hint"       .=
              ("Type error in declarations. Use action=write to fix \
               \the code under the same id, then run action=check again." :: Text)
          ])
  where
    declOkMsg :: Text
    declOkMsg = "declarations type-checked OK"

-- | #294 declaration path: type-check a block that opens with a top-level
-- declaration keyword (@type@ / @data@ / @newtype@ / @class@ / @instance@ /
-- standalone @deriving@ / fixity). These are illegal inside the
-- @let … in ()@ wrapper, so we hand the raw source to 'runDecls' — the
-- same primitive GHCi uses for top-level prompt input. Success means the
-- declarations compiled; a parse/type error is caught by 'try' and
-- surfaced as @type_error@.
checkDecls :: SP.Store -> ScratchQueries -> SP.ScratchEntry
           -> Text -> [Text] -> Text -> Double -> IO ToolResponse
checkDecls store q entry i imports code now =
  case sanitizeDeclarations code of
    Left cmdErr ->
      pure (Env.mkRefused (Env.sanitizeRejection "code" cmdErr))
    Right safe -> do
      eRes <- sqRunDecls q (SP.seModule entry) imports safe
      saveAndRespond store entry now eRes
        (\_ -> object
          [ "id"     .= i
          , "status" .= SP.ScratchVerified
          , "kind"   .= ("type_ok" :: Text)
          , "module" .= SP.seModule entry
          , "type"   .= declOkMsg
          , "hint"   .=
              ("Declarations compile. Use action=promote to splice them \
               \into a target module, or action=show to see the \
               \persisted result." :: Text)
          ])
        (\errText -> object
          [ "id"         .= i
          , "status"     .= SP.ScratchOpen
          , "kind"       .= ("type_error" :: Text)
          , "type_error" .= errText
          , "hint"       .=
              ("Type error in declarations. Use action=write to fix \
               \the code under the same id, then run action=check again." :: Text)
          ])
  where
    declOkMsg :: Text
    declOkMsg = "declarations type-checked OK"

-- | Run a block of top-level declarations against the live session via
-- 'runDecls'. Per-entry imports are spliced in first (and the import
-- context restored afterwards, exactly like 'queryExprTypeWithImports').
--
-- Note: 'runDecls' binds the declared names into the interactive context.
-- That binding is transient — the next 'ghc_load' / 'loadForTarget' resets
-- the context to @Prelude + home modules@ — and harmless (it only makes the
-- just-checked names visible to a subsequent scratch check in the same
-- session, which is the scratchpad's intent). Returns a human-readable
-- confirmation string so 'saveAndRespond' has something to persist.
runDeclsWithImports :: [Text] -> Text -> Ghc Text
runDeclsWithImports imports code = do
  saved <- getContext
  unless (null imports) $ do
    idecls <- mapM (parseImportDecl . T.unpack) imports
    setContext (map IIDecl idecls ++ saved)
  _names <- runDecls (T.unpack code)
  setContext saved
  pure "declarations type-checked OK"

-- | True when the snippet opens (at column 0) with a top-level declaration
-- keyword that cannot be wrapped in @let … in ()@. Used by 'handleCheck' to
-- route such blocks to 'checkDecls'/'runDecls' instead. Exported for tests.
hasTopLevelDecl :: Text -> Bool
hasTopLevelDecl = any isDeclLine . T.lines
  where
    isDeclLine l =
      let s = T.stripStart l
       in l == s && any (`opensWith` s) declKeywords
    -- A keyword "opens" a line when it is followed by a space (the common
    -- shape: `data Foo`, `type Name = …`, `instance Show …`) or is the
    -- whole line (degenerate, still a decl).
    opensWith kw s = (kw <> " ") `T.isPrefixOf` s || s == kw
    declKeywords :: [Text]
    declKeywords =
      ["type", "data", "newtype", "class", "instance", "deriving"
      , "infixl", "infixr", "infix"]

-- | Shared logic: persist the check result, return the appropriate
-- response using the caller-provided payload builders.
saveAndRespond
  :: SP.Store
  -> SP.ScratchEntry
  -> Double            -- now (POSIX seconds)
  -> Either Text Text  -- rendered backend result or error text
  -> (Text -> Value)   -- success payload builder
  -> (Text -> Value)   -- failure payload builder
  -> IO ToolResponse
saveAndRespond store entry now eRes mkOkPayload mkErrPayload =
  case eRes of
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
      pure (Env.mkOk (mkOkPayload typeText))
    Left errText -> do
      let result  = SP.ScratchResult
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
      pure (Env.mkOk (mkErrPayload errText))

-- | Wrap a multi-line declaration block in @let ... in ()@ so
-- 'exprType' can type-check it as a single expression.
--
-- Each line of @code@ is indented by two spaces so GHC's layout rule
-- treats all bindings as siblings at the same indentation level.
-- The expression in the @in@ clause is @()@; the wrapper always
-- type-checks as @()@ when the declarations are valid.
--
-- Works for: multi-equation bindings, guards, type signatures.
-- Does not work for: @where@-clauses (not valid inside @let@
-- in standard Haskell) or @import@ declarations.
wrapAsLetBlock :: Text -> Text
wrapAsLetBlock code =
  let indented = T.unlines (map ("  " <>) (T.lines code))
  in "let\n" <> indented <> " in ()"

-- | Issue @:t expr@ inside an active 'GhcSession'.  Mirrors
-- 'HaskellFlows.Tool.Type.queryExprType' — kept local to avoid a
-- cross-tool import dependency.
queryExprType :: Text -> Ghc Text
queryExprType safe = do
  ty <- exprType TM_Inst (T.unpack safe)
  pure (T.pack (showPprUnsafe ty))

-- | Like 'queryExprType', but first splices the given @import …@ statements
-- into the interactive context so the expression can reference names they
-- bring into scope (#276).  The original context is always restored
-- afterwards (via 'gfinally') so a scratch check never pollutes later evals.
--
-- An import that fails to parse / resolve propagates as an exception, which
-- the caller's 'try' surfaces as a @type_error@ — the same path a bad
-- declaration takes.
queryExprTypeWithImports :: [Text] -> Text -> Ghc Text
queryExprTypeWithImports [] expr = queryExprType expr
queryExprTypeWithImports imports expr = do
  saved  <- getContext
  idecls <- mapM (parseImportDecl . T.unpack) imports
  setContext (map IIDecl idecls ++ saved)
  ty <- queryExprType expr
  -- Restore on the happy path. On exception, 'withGhcSession' never writes the
  -- mutated env back to its IORef (it only persists the session after the
  -- action returns), so the added imports are discarded — no pollution either
  -- way, no exception-handling combinator needed.
  setContext saved
  pure ty

-- | Verify that a scratch entry consisting only of @import …@ lines resolves,
-- by splicing the imports into the context and type-checking @()@ against it.
-- Restores the context afterwards.
checkImportsOnly :: SP.Store -> ScratchQueries -> SP.ScratchEntry
                 -> Text -> [Text] -> Double -> IO ToolResponse
checkImportsOnly store q entry i imports now = do
  eRes <- sqExprType q (SP.seModule entry) imports "()"
  saveAndRespond store entry now (fmap (const importsOkMsg) eRes)
    (\_ -> object
      [ "id"     .= i
      , "status" .= SP.ScratchVerified
      , "kind"   .= ("type_ok" :: Text)
      , "module" .= SP.seModule entry   -- #274: lets nextStep resolve target_module
      , "type"   .= importsOkMsg
      , "hint"   .=
          ("Imports resolve. Add a declaration or expression under the same \
           \id and re-check, or action=promote." :: Text)
      ])
    (\errText -> object
      [ "id"         .= i
      , "status"     .= SP.ScratchOpen
      , "kind"       .= ("type_error" :: Text)
      , "type_error" .= errText
      , "hint"       .=
          ("An import failed to resolve. Check the module name / that the \
           \package is a dependency, then action=check again." :: Text)
      ])
  where
    importsOkMsg :: Text
    importsOkMsg = "imports resolve"

-- | Partition a scratch snippet into its leading @import …@ statements and the
-- remaining declaration/expression body (#276).
--
-- Haskell requires every import to precede all declarations, so the import
-- section is the maximal prefix of lines up to the first line that starts a
-- non-import top-level declaration (column 0, non-blank, not @import@).
-- Multi-line import lists (continuation lines are indented) are rejoined into
-- a single statement so 'parseImportDecl' sees the whole declaration.
--
-- Returns @(importStatements, bodyText)@.  @bodyText@ preserves the original
-- line layout of everything after the import section.
splitImports :: Text -> ([Text], Text)
splitImports code =
  let ls                 = T.lines code
      (impSection, rest) = break isDeclStart ls
   in (groupImports impSection, T.unlines rest)
  where
    -- A line that starts a top-level declaration: column 0, non-blank, and not
    -- the keyword @import@. This is where the import section ends.
    isDeclStart :: Text -> Bool
    isDeclStart l =
      let s = T.stripStart l
       in not (T.null s)
            && not (isIndented l)
            && not (importHead s)

    isIndented :: Text -> Bool
    isIndented l = case T.uncons l of
      Just (c, _) -> c == ' ' || c == '\t'
      Nothing     -> False

    importHead :: Text -> Bool
    importHead s = s == "import" || T.isPrefixOf "import " s

-- | Group a run of import-section lines into whole import statements: each line
-- beginning with @import@ starts a new statement; indented continuation lines
-- (e.g. a multi-line import list) attach to the current one; blank lines are
-- dropped.
groupImports :: [Text] -> [Text]
groupImports = finish . foldl step []
  where
    step :: [[Text]] -> Text -> [[Text]]
    step groups l
      | importHead (T.stripStart l) = [l] : groups          -- new statement
      | T.null (T.strip l)          = groups                -- skip blanks
      | otherwise = case groups of
          (g : gs) -> (l : g) : gs                          -- continuation
          []       -> groups                                -- stray pre-import line
    finish :: [[Text]] -> [Text]
    finish = reverse . map (T.intercalate "\n" . reverse)
    importHead :: Text -> Bool
    importHead s = s == "import" || T.isPrefixOf "import " s

--------------------------------------------------------------------------------
-- write
--------------------------------------------------------------------------------

handleWrite :: SP.Store -> ScratchArgs -> IO ToolResponse
handleWrite store args = case saCode args of
  Nothing ->
    pure (Env.mkFailed
      (Env.mkErrorEnvelope Env.MissingArg
        "action=write requires 'code'"))
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
    pure (Env.mkOk (object
      [ "id"     .= entryId
      , "kind"   .= SP.seKind entry
      , "status" .= SP.seStatus entry
      , "hint"   .= ("Entry persisted. Use action=check to type-check it \
                     \against the live session, or action=promote when verified." :: Text)
      ]))

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

handleList :: SP.Store -> IO ToolResponse
handleList store = do
  entries <- SP.loadAll store
  let summaries = map renderEntrySummary entries
      counts    = tallyStatuses entries
  pure (Env.mkOk (object
    [ "count"   .= length entries
    , "entries" .= summaries
    , "counts"  .= counts
    , "hint"    .= listHint entries
    ]))

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

handleShow :: SP.Store -> ScratchArgs -> IO ToolResponse
handleShow store args = case saId args of
  Nothing ->
    pure (Env.mkFailed
      (Env.mkErrorEnvelope Env.MissingArg
        "action=show requires 'id'"))
  Just i -> do
    mEntry <- SP.findById store i
    case mEntry of
      Nothing ->
        pure (Env.mkNoMatch (object
          [ "id"    .= i
          , "found" .= False
          , "hint"  .= ("No scratchpad entry with that id. \
                        \Use action=list to see existing ids." :: Text)
          ]))
      Just e ->
        pure (Env.mkOk (toJSON e))

--------------------------------------------------------------------------------
-- clear
--------------------------------------------------------------------------------

handleClear :: SP.Store -> ScratchArgs -> IO ToolResponse
handleClear store args = case (saId args, saConfirm args) of
  (Just i, _) -> do
    -- Single-entry remove. No confirm needed — caller named the id.
    mEntry <- SP.findById store i
    case mEntry of
      Nothing ->
        pure (Env.mkNoMatch (object
          [ "id"      .= i
          , "removed" .= False
          , "hint"    .= ("No entry with that id; nothing to remove." :: Text)
          ]))
      Just _ -> do
        SP.remove store i
        pure (Env.mkOk (object
          [ "id"      .= i
          , "removed" .= True
          ]))
  (Nothing, True) -> do
    -- Bulk truncate.
    SP.clearAll store
    pure (Env.mkOk (object
      [ "cleared" .= True
      , "hint"    .= ("Scratchpad truncated." :: Text)
      ]))
  (Nothing, False) ->
    pure (Env.mkRefused
      (Env.mkErrorEnvelope Env.Validation
        "action=clear without 'id' requires confirm=true to drop the whole scratchpad"))

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
handlePromote :: SP.Store -> ScratchQueries -> ProjectDir -> ScratchArgs -> IO ToolResponse
handlePromote store q pd args =
  case saId args of
    Nothing ->
      pure (Env.mkFailed
        (Env.mkErrorEnvelope Env.MissingArg
          "action=promote requires 'id'"))
    Just i ->
      case saTargetModule args of
        Nothing ->
          pure (Env.mkFailed
            (Env.mkErrorEnvelope Env.MissingArg
              "action=promote requires 'target_module' \
              \(e.g. \"src/Foo.hs\") — the file where the code will be spliced."))
        Just rawModule ->
          case mkModulePath pd (T.unpack rawModule) of
            Left pe ->
              pure (Env.mkRefused
                (Env.mkErrorEnvelope Env.PathTraversal
                  (renderPathErr pe)))
            Right mp -> do
              mEntry <- SP.findById store i
              case mEntry of
                Nothing ->
                  pure (Env.mkNoMatch (object
                    [ "id"    .= i
                    , "found" .= False
                    , "hint"  .= ("No scratchpad entry with that id. \
                                  \Use action=list to see existing ids." :: Text)
                    ]))
                Just entry -> do
                  now <- realToFrac <$> getPOSIXTime
                  -- F-04: if binding_name is given, wrap single-line
                  -- expressions as "name = expr" so they splice as
                  -- valid top-level declarations.
                  let spliceCode  = case saBindingName args of
                        Nothing   -> SP.seCode entry
                        Just name -> name <> " = " <> SP.seCode entry
                      targetLine  = saTargetLine args
                      successBase = object
                        [ "id"            .= i
                        , "target_module" .= rawModule
                        , "kind"          .= ("promoted" :: Text)
                        , "hint"          .=
                            ("Code spliced and verified. \
                             \Entry status is now 'promoted'." :: Text)
                        ]
                  result <- sqPromote q mp $ \orig ->
                    pure (Right (spliceInto orig spliceCode targetLine, successBase))
                  -- Only promote the entry if the refactor succeeded.
                  -- trIsError=True means the snapshot was rolled back.
                  unless (trIsError (Env.toolResponseToResult result)) $ do
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
