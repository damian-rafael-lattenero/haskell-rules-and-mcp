-- | @ghc_check_module@ — Wave-5 full GhcSession.
--
-- All four gates (compile / warnings / holes / regression) run
-- in-process: 'loadForTarget' Strict → errors + warnings;
-- 'loadForTarget' Deferred → hole warnings; property replay via
-- 'Regression.runOne' (which itself is in-process Wave-3).
module HaskellFlows.Tool.CheckModule
  ( descriptor
  , handle
  , CheckArgs (..)
    -- * Issue #42 — properties-gate computation
  , propertiesGate
    -- * Issue #74 — path → module-name resolver helpers
  , resolveModuleName
  , parseModuleHeader
  ) where

import Control.Exception (SomeException, try)
import Data.Aeson
import Data.Aeson.Types (parseEither)
import Data.Char (isAlphaNum)
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.IO as TIO
import System.FilePath ((</>))

import HaskellFlows.Data.PropertyStore
  ( Store
  , StoredProperty (..)
  , loadAll
  )
import HaskellFlows.Ghc.ApiSession
  ( GhcSession
  , LoadFlavour (..)
  , invalidateLoadCache
  , loadForTarget
  , targetForPath
  )
import qualified HaskellFlows.Mcp.Envelope as Env
import HaskellFlows.Mcp.ParseError (formatParseError)
import HaskellFlows.Mcp.Protocol
import HaskellFlows.Mcp.ToolName (ToolName (..), toolNameText)
import HaskellFlows.Parser.Error
  ( GhcError (..)
  , Severity (..)
  , renderGhciStyle
  )
import HaskellFlows.Parser.Hole (parseTypedHoles)
import HaskellFlows.Parser.QuickCheck (QuickCheckResult (..))
import qualified HaskellFlows.Tool.Regression as RegTool
import HaskellFlows.Types
  ( ProjectDir
  , PathError (..)
  , mkModulePath
  , unProjectDir
  )

descriptor :: ToolDescriptor
descriptor =
  ToolDescriptor
    { tdName        = toolNameText GhcCheckModule
    , tdDescription =
        "Aggregate all module-health gates into one report: compiles? "
          <> "no errors? no warnings? no typed holes? stored properties "
          <> "still pass? Returns per-gate pass/fail plus a single "
          <> "'overall' boolean. Use after editing a module to confirm "
          <> "it is clean before moving on. For whole-project health use "
          <> "ghc_check_project instead. SEE ALSO: ghc_check_project "
          <> "(all modules), ghc_lint (style hints), ghc_gate (pre-push)."
    , tdInputSchema =
        object
          [ "type"       .= ("object" :: Text)
          , "properties" .= object
              [ "module_path" .= object
                  [ "type"        .= ("string" :: Text)
                  , "description" .=
                      ("Path to the module to check, relative to the \
                       \project directory." :: Text)
                  ]
              , "warnings_block" .= object
                  [ "type"        .= ("boolean" :: Text)
                  , "description" .=
                      ("When true (default), '-Wall' warnings count \
                       \against 'overall' — the strict pre-push gate. \
                       \Set false during early iteration to keep \
                       \warnings informational; they still appear in \
                       \'diagnostics.warnings' but don't fail the \
                       \gate. Errors and hole/regression gates are \
                       \always blocking." :: Text)
                  ]
              ]
          , "required"             .= ["module_path" :: Text]
          , "additionalProperties" .= False
          ]
    }

data CheckArgs = CheckArgs
  { caModulePath    :: !Text
  , caWarningsBlock :: !Bool
  }
  deriving stock (Show)

instance FromJSON CheckArgs where
  parseJSON = withObject "CheckArgs" $ \o -> do
    mp <- o .:  "module_path"
    wb <- o .:? "warnings_block" .!= True
    pure CheckArgs { caModulePath = mp, caWarningsBlock = wb }

handle :: GhcSession -> Store -> ProjectDir -> Value -> IO ToolResult
handle ghcSess store pd rawArgs = case parseEither parseJSON rawArgs of
  Left parseError ->
    pure (formatParseError parseError)
  Right (CheckArgs raw warnBlock) -> case mkModulePath pd (T.unpack raw) of
    Left e -> pure (pathTraversalResult (formatPathError e))
    Right _ -> do
      invalidateLoadCache ghcSess
      tgt <- targetForPath ghcSess (T.unpack raw)
      eStrict <- try (loadForTarget ghcSess tgt Strict)
      case eStrict :: Either SomeException (Bool, [GhcError]) of
        Left ex ->
          pure (subprocessResult
                  ("loadForTarget failed: " <> T.pack (show ex)))
        Right (strictOk, strictDiags) -> do
          -- 'loadForTarget' loads the whole target (library or
          -- test-suite), so 'strictDiags' is the UNION of warnings
          -- across every module in that target. Filter to this
          -- module's file only: without the filter, a warning in
          -- 'Expr.Pretty' would red-gate 'Expr.Syntax' too, and
          -- 'check_project' would show the same warnings attributed
          -- to N modules (one per module it iterated).
          -- Diagnostic attribution: GHC reports absolute paths in
          -- 'geFile' (e.g. @/tmp/proj/src/Foo.hs@); the user passed
          -- a project-relative path (e.g. @src/Foo.hs@). A suffix
          -- match on the relative path is enough to own/disown a
          -- diag — the absolute path will always end with the
          -- relative one when GHC is pointed at this project root.
          let ownDiag d = raw `T.isSuffixOf` geFile d
              ownDiags  = filter ownDiag strictDiags
              -- Issue #108 (F-23 propagation): GHC reports typed holes
              -- (code GHC-88464) as SevError in the strict pass, which
              -- caused compile.ok=false + holes.ok=true (vacuously) when
              -- the only issues were holes. Reclassify holes out of the
              -- errors bucket so they flow to the holes gate instead.
              isHoleErr d = geSeverity d == SevError
                         && geCode d == Just "GHC-88464"
              errors    = filter (\d -> geSeverity d == SevError
                                     && not (isHoleErr d)) ownDiags
              warnings  = filter ((== SevWarning) . geSeverity) ownDiags
              -- compileOk: null real errors, and either the project-wide
              -- strict flag says OK, or this module's only SevErrors were
              -- holes (which means strictOk=False is caused by holes, not
              -- a real compile failure in this module's own stanza).
              ownHoleOnly = null errors
                         && any isHoleErr ownDiags
              compileOk = null errors
                       && (strictOk || ownHoleOnly)
          holes <- if compileOk
                     then do
                       eDef <- try (loadForTarget ghcSess tgt Deferred)
                       pure $ case eDef :: Either SomeException (Bool, [GhcError]) of
                         Left _           -> []
                         Right (_, diags) ->
                           parseTypedHoles (renderGhciStyle diags)
                     else pure []
          allProps <- loadAll store
          -- Issue #74: 'ghc_quickcheck' persists 'module' as the
          -- Haskell module name ("Foo.Bar"), but 'check_module' is
          -- called with a relative path ("src/Foo/Bar.hs"). Resolve
          -- the path to its module name and accept either shape so
          -- the gate sees the properties it should be guarding.
          mModName <- resolveModuleName pd raw
          let propMatches sp = case spModule sp of
                Just s  -> s == raw
                       || (Just s == mModName)
                Nothing -> False
              relevant = filter propMatches allProps
          -- Reuse the Wave-3 Regression.runOne — it's already
          -- in-process via evalIOString.
          replays <- mapM (RegTool.runOne ghcSess) relevant
          -- Issue #42 + #51: split replays into three buckets:
          --   * load_failed — module didn't compile, replay never ran;
          --   * regressed   — replay ran, found a counterexample;
          --   * passed      — implicit (everything else).
          let isLoadFailed r = case RegTool.rpLoadFailure r of
                                 Just _  -> True
                                 Nothing -> False
              loadFailedReplays = filter isLoadFailed replays
              evaluatedReplays  = filter (not . isLoadFailed) replays
              regressions =
                [ (RegTool.rpStored r, RegTool.rpResult r)
                | r <- evaluatedReplays
                , case RegTool.rpResult r of
                    QcPassed _ _ -> False
                    _            -> True
                ]
          pure $ renderResult
            raw compileOk errors warnings holes regressions
            (length relevant) (length loadFailedReplays) warnBlock

--------------------------------------------------------------------------------
-- response shaping
--------------------------------------------------------------------------------

renderResult
  :: Text
  -> Bool
  -> [GhcError]
  -> [GhcError]
  -> [a]
  -> [(StoredProperty, QuickCheckResult)]
  -> Int      -- ^ total stored properties for this module.
  -> Int      -- ^ count of replays that failed to load (#51 + #42).
  -> Bool     -- ^ warnings_block — True (default) keeps warnings blocking.
  -> ToolResult
renderResult mp compileOk errs warns holes regressions totalProps loadFailed warnBlock =
  let gateCompile    = gate compileOk     "module compiles strictly"
      gateNoWarnings = gate (null warns || not warnBlock) $
        if null warns
          then "no warnings (-Wall clean)"
          else if warnBlock
            then T.pack (show (length warns)) <> " warning(s) (blocking — "
               <> "pass warnings_block=false to keep iterating)"
            else T.pack (show (length warns))
              <> " warning(s) (informational; warnings_block=false)"
      gateNoHoles    = gate (null holes)  "no deferred typed holes"
      -- Issue #42: gates.properties used to say
      -- '"reason": "1 stored properties pass"' even when ok=false,
      -- because the reason text was computed independently of
      -- whether 'regressions' was empty. Now the reason flows
      -- from the (totalProps, regressed, loadFailed) triple, so
      -- ok and reason can never disagree.
      regressed      = length regressions
      passed         = totalProps - regressed - loadFailed
      gateProps      = propertiesGate totalProps passed regressed loadFailed
      overall = compileOk
             && (null warns || not warnBlock)
             && null holes
             && null regressions
             && loadFailed == 0
      payload =
        object
          [ "module"     .= mp
          , "overall"    .= overall
          , "gates"      .= object
              [ "compile"    .= gateCompile
              , "warnings"   .= gateNoWarnings
              , "holes"      .= gateNoHoles
              , "properties" .= gateProps
              ]
          , "diagnostics" .= object
              [ "errors"   .= errs
              , "warnings" .= warns
              ]
          , "summary" .= summarise overall errs warns holes regressions
          ]
  -- Issue #90 Phase C: 'overall=true' → status='ok'. Any red gate
  -- → status='failed' with kind matching the dominant signal:
  -- compile error → 'compile_error', otherwise 'gate_failure' (the
  -- compile passed but a quality gate — warnings, holes, property
  -- regression — refused the module). #119: 'validation' implies the
  -- caller's INPUT was malformed; use 'gate_failure' here.
  in if overall
       then Env.toolResponseToResult (Env.mkOk payload)
       else
         let kind | not compileOk = Env.CompileError
                  | otherwise     = Env.GateFailure
             envErr   = Env.mkErrorEnvelope kind
                          (summarise overall errs warns holes regressions)
             response = (Env.mkFailed envErr) { Env.reResult = Just payload }
         in Env.toolResponseToResult response

-- | Issue #42: structured properties-gate value with a status
-- discriminator and per-bucket counts. Four states:
--
--   * @"empty"@     — no stored properties; nothing to verify.
--   * @"pass"@      — every stored property replayed and passed.
--   * @"regressed"@ — at least one stored property changed semantics.
--   * @"skipped"@   — at least one stored property couldn't replay
--                     because the module failed to load (#51 sibling).
--
-- The @reason@ text now matches the status, so consumers that
-- pattern-match on the text never see \"N stored properties pass\"
-- on a red gate.
propertiesGate :: Int -> Int -> Int -> Int -> Value
propertiesGate total passed regressed loadFailed =
  let (ok, status, reason) =
        case (total, regressed, loadFailed) of
          (0, _, _) ->
            ( True, "empty" :: Text
            , "no stored properties for this module (nothing to regress)" )
          (_, 0, 0) ->
            ( True, "pass"
            , T.pack (show total) <> " stored properties pass" )
          (_, r, 0) ->
            ( False, "regressed"
            , T.pack (show r) <> "/" <> T.pack (show total)
                <> " stored properties regressed" )
          (_, 0, lf) ->
            ( False, "skipped"
            , T.pack (show lf) <> "/" <> T.pack (show total)
                <> " stored properties could not replay — \
                   \module failed to load" )
          (_, r, lf) ->
            ( False, "regressed"
            , T.pack (show r) <> " regressed, "
                <> T.pack (show lf) <> " skipped (load failed) of "
                <> T.pack (show total) <> " stored properties" )
  in object
       [ "ok"         .= ok
       , "status"     .= status
       , "total"      .= total
       , "passed"     .= passed
       , "regressed"  .= regressed
       , "skipped"    .= loadFailed
       , "reason"     .= reason
       ]

gate :: Bool -> Text -> Value
gate ok reason =
  object
    [ "ok"     .= ok
    , "reason" .= reason
    ]

summarise
  :: Bool
  -> [GhcError]
  -> [GhcError]
  -> [a]
  -> [(StoredProperty, QuickCheckResult)]
  -> Text
summarise True _ _ _ _ =
  "All gates green. Module is complete."
summarise False errs warns holes regs =
  T.intercalate "; " $ filter (not . T.null)
    [ if null errs  then "" else T.pack (show (length errs))  <> " error(s)"
    , if null warns then "" else T.pack (show (length warns)) <> " warning(s)"
    , if null holes then "" else T.pack (show (length holes)) <> " hole(s)"
    , if null regs  then "" else T.pack (show (length regs))  <> " property regression(s)"
    ]


-- | Issue #90 Phase C: 'mkModulePath' rejection.
pathTraversalResult :: Text -> ToolResult
pathTraversalResult msg =
  Env.toolResponseToResult
    (Env.mkRefused (Env.mkErrorEnvelope Env.PathTraversal msg))

-- | Issue #90 Phase C: GHC API exception.
subprocessResult :: Text -> ToolResult
subprocessResult msg =
  Env.toolResponseToResult
    (Env.mkFailed (Env.mkErrorEnvelope Env.SubprocessError msg))

formatPathError :: PathError -> Text
formatPathError = \case
  PathNotAbsolute p        -> "Project directory is not absolute: " <> p
  PathEscapesProject a p _ -> "module_path '" <> a <> "' escapes project directory " <> p


--------------------------------------------------------------------------------
-- Issue #74 — path → module-name resolver
--------------------------------------------------------------------------------

-- | Issue #74: read the on-disk source file and recover its
-- declared module name (e.g. @"DogfoodSuite.Math"@ from
-- @src/DogfoodSuite/Math.hs@). Returns 'Nothing' when the file
-- can't be read or has no parseable @module … where@ header.
--
-- The IO bit is the file read; the parsing is delegated to
-- 'parseModuleHeader' which is pure and unit-tested independently.
resolveModuleName :: ProjectDir -> Text -> IO (Maybe Text)
resolveModuleName pd relPath = do
  let absPath = unProjectDir pd </> T.unpack relPath
  e <- try @SomeException (TIO.readFile absPath)
  pure $ case e of
    Left _    -> Nothing
    Right src -> parseModuleHeader src

-- | Issue #74: pure parser for the @module Foo.Bar where@ /
-- @module Foo.Bar (…) where@ header.
--
-- We accept the line-by-line shape Haskell actually uses:
--
-- @
-- -- | Optional Haddock blurb at the top.
-- {-\# LANGUAGE Foo \#-}
-- module Foo.Bar
--   ( foo
--   , bar
--   ) where
-- @
--
-- The parser:
--
--   * skips blank lines, single-line comments (@-- …@) and
--     pragmas (@{-\# … \#-}@) at the top of the file;
--   * looks for the first line that starts with @module @
--     (after optional indent — Haskell allows it but the
--     scaffold doesn't);
--   * reads the next whitespace-delimited token as the module
--     name and strips off any trailing @(@ that begins an
--     explicit export list on the same line.
--
-- Returns 'Nothing' for a file with no recognisable header
-- rather than guessing — the caller falls back to path-based
-- comparison so the gate still behaves sensibly on, e.g.,
-- @hs-boot@ files or partially-written sources.
parseModuleHeader :: Text -> Maybe Text
parseModuleHeader src = go (T.lines src)
  where
    go []         = Nothing
    go (line:rest) =
      let stripped = T.strip line
      in case classify stripped of
        SkipLine    -> go rest
        ModuleStart name
          | not (T.null name) && validModuleName name -> Just name
          | otherwise                                 -> Nothing
        UnknownLine -> Nothing  -- first non-skippable, non-module line
                                -- means we're past the header.

-- | Classification of a stripped source line for header-walking.
data LineKind
  = SkipLine          -- ^ blank, line comment, or pragma — keep walking.
  | ModuleStart !Text -- ^ @module Foo.Bar (...)@ — the carried name.
  | UnknownLine       -- ^ first \"real\" line — header search ends.

classify :: Text -> LineKind
classify s
  | T.null s                  = SkipLine
  | "--" `T.isPrefixOf` s     = SkipLine
  | "{-#" `T.isPrefixOf` s    = SkipLine
  | "{-" `T.isPrefixOf` s     = SkipLine     -- block-comment starts; bail to UnknownLine on next non-skip
  | "module " `T.isPrefixOf` s =
      let afterKw  = T.drop (T.length "module ") s
          modName  = T.takeWhile (\c -> isAlphaNum c || c == '.') (T.stripStart afterKw)
      in ModuleStart modName
  | otherwise                 = UnknownLine

validModuleName :: Text -> Bool
validModuleName n = case T.uncons n of
  Just (c, _) -> c >= 'A' && c <= 'Z'
              && T.all (\ch -> isAlphaNum ch || ch == '.') n
  Nothing     -> False
