-- | @ghc_quickcheck@ — Wave-3 full in-process.
--
-- Runs a QuickCheck property against the project using the GHC API's
-- @compileExpr@ + @unsafeCoerce@ path ('evalIOString'). No more
-- subprocess ghci, no more chatty-stdout capture — the property is
-- compiled in-process under the relevant stanza's flags and its
-- @Result.output@ string is parsed by the existing
-- 'parseQuickCheckOutput' (the formatting matches GHCi's exactly
-- because we ask QuickCheck for the same output).
--
-- On success the property expression + module are persisted to the
-- property store so @ghc_regression@ can replay it later.
module HaskellFlows.Tool.QuickCheck
  ( descriptor
  , handle
  , QuickCheckArgs (..)
    -- * Shared runtime-execution helper (Regression, Determinism)
  , runQuickCheckViaCabalRepl
  , runQuickCheckWithLabelsViaCabalRepl
  , extractLabelsBlock
    -- * Pure helpers exposed for unit tests
  , chooseStoreModule
  , isSimpleIdent
  , summariseStderr
    -- * Cabal library introspection (re-used by 'Tool.QuickCheckExport')
  , libraryExposedModules
  , scanLibraryExposedModules
  ) where

import Control.Applicative ((<|>))
import Control.Exception (SomeException, try)
import Data.Aeson
import qualified Data.Aeson.Types as AesonTypes
import Data.Aeson.Types (parseEither)
import Data.Char (isAlpha, isAlphaNum)
import Data.Text (Text)
import qualified Data.Text as T
import System.Timeout (timeout)

import qualified System.Process as Proc
import System.Exit (ExitCode (..))
import System.Directory (listDirectory)
import System.FilePath (takeExtension, (</>))
import qualified Data.Text.IO as TIO

import qualified HaskellFlows.Tool.Deps as Deps

import HaskellFlows.Data.PropertyStore (Store, save)
import HaskellFlows.Ghc.ApiSession
  ( GhcSession
  , LoadFlavour (..)
  , firstTestSuiteOrLibrary
  , gsProject
  , loadForTarget
  , withGhcSession
  )
import HaskellFlows.Types (ProjectDir, unProjectDir)

import GHC
  ( getModuleGraph
  , getModuleInfo
  , mgModSummaries
  , modInfoExports
  , ms_mod
  )
import GHC.Data.FastString (unpackFS)
import GHC.Types.Name (nameOccName, nameSrcSpan)
import GHC.Types.Name.Occurrence (occNameString)
import GHC.Types.SrcLoc (RealSrcSpan, SrcSpan (..), srcSpanFile)
import HaskellFlows.Ghc.Sanitize
  ( sanitizeExpression
  )
import qualified HaskellFlows.Mcp.Envelope as Env
import HaskellFlows.Mcp.ParseError (formatParseError)
import HaskellFlows.Mcp.Protocol
import HaskellFlows.Mcp.ToolName (ToolName (..), toolNameText)
import HaskellFlows.Parser.QuickCheck

descriptor :: ToolDescriptor
descriptor =
  ToolDescriptor
    { tdName        = toolNameText GhcQuickCheck
    , tdDescription =
        "Run a QuickCheck property against the current session. "
          <> "The property is passed directly to quickCheckWithResult, "
          <> "so it must be a value of type Testable (e.g. "
          <> "`\\x -> reverse (reverse x) == x`). Returns structured "
          <> "pass/fail/gave-up/exception output. Pass `runs` >= 2 to "
          <> "run the same property N times for flakiness detection "
          <> "(#94 Phase C: subsumes the retired ghc_determinism)."
    , tdInputSchema =
        object
          [ "type"       .= ("object" :: Text)
          , "properties" .= object
              [ "property" .= object
                  [ "type"        .= ("string" :: Text)
                  , "description" .=
                      ("QuickCheck-testable property expression. Examples: \
                       \\"\\\\(xs :: [Int]) -> reverse (reverse xs) == xs\", \
                       \\"prop_idempotent\"" :: Text)
                  ]
              , "module" .= object
                  [ "type"        .= ("string" :: Text)
                  , "description" .=
                      ("Optional: module path to associate with the property \
                       \in the regression store. Lets ghc_regression reload \
                       \the right scope before re-running. Example: \
                       \\"src/Foo.hs\"." :: Text)
                  ]
              , "runs" .= object
                  [ "type"        .= ("integer" :: Text)
                  , "description" .=
                      ("Optional: number of runs. Default 1 (single QC run). \
                       \Pass >= 2 to run the same property N times and report \
                       \flakiness — the determinism mode that used to be \
                       \exposed as the separate ghc_determinism tool." :: Text)
                  , "minimum"     .= (1 :: Int)
                  ]
              ]
          , "required"             .= ["property" :: Text]
          , "additionalProperties" .= False
          ]
    }

data QuickCheckArgs = QuickCheckArgs
  { qaProperty :: !Text
  , qaModule   :: !(Maybe Text)
  }
  deriving stock (Show)

instance FromJSON QuickCheckArgs where
  parseJSON = withObject "QuickCheckArgs" $ \o -> do
    prop <- o .: "property"
    md   <- o .:? "module"
    -- #94 Phase C: 'runs' is consumed by the dispatcher (Server.hs)
    -- to route to the determinism handler when >= 2. Pinning it
    -- in the parser would force every QC call to declare it; we
    -- accept the field as a no-op when it's <= 1 / absent and let
    -- the dispatcher do the routing.
    _    <- o .:? "runs" :: AesonTypes.Parser (Maybe Int)
    pure QuickCheckArgs { qaProperty = prop, qaModule = md }

-- | Runtime ceiling for a single quickCheck invocation. Mirrors the
-- 30 s budget the legacy subprocess path used. Properties that loop
-- forever or expand exponentially hit this and surface as a
-- QcException with an explicit timeout message.
quickCheckTimeoutMicros :: Int
quickCheckTimeoutMicros = 30_000_000

handle :: Store -> GhcSession -> Value -> IO ToolResult
handle store ghcSess rawArgs = case parseEither parseJSON rawArgs of
  Left parseError ->
    pure (formatParseError parseError)
  Right (QuickCheckArgs prop md) -> case sanitizeExpression prop of
    Left cmdErr ->
      pure (Env.toolResponseToResult
              (Env.mkRefused (Env.sanitizeRejection "property" cmdErr)))
    Right safe -> do
      -- Resolve the property's defining module via the GHC API.
      -- If the property is a bare identifier we can look up
      -- 'parseName + nameSrcSpan' and the resulting file path
      -- becomes authoritative; the caller hint ('md') is treated
      -- as a fallback for lambda/expression properties where
      -- parseName would legitimately fail.
      --
      -- This restores the pre-Wave-5 behaviour where
      -- 'ghc_quickcheck prop_x module="src/Foo.hs"' — a common
      -- caller mistake when the property actually lives in the
      -- test suite — still persisted test/Spec.hs in the
      -- regression store, so replay loaded the right scope.
      resolved <- resolvePropertyModule ghcSess safe
      let loadHint = resolved <|> md
      mRes <- timeout quickCheckTimeoutMicros $
        try $ runQuickCheckViaCabalRepl (gsProject ghcSess) loadHint safe
      case mRes of
        Nothing ->
          pure (renderResult
            (QcException prop "timeout: property exceeded 30s budget") Nothing)
        Just (Left (ex :: SomeException)) ->
          pure (renderResult (QcException prop (T.pack (show ex))) Nothing)
        Just (Right (out, stderrText)) -> do
          let qr = parseQuickCheckOutput prop out
          case qr of
            QcPassed _ _ -> save store prop loadHint
            _            -> pure ()
          -- Surface stderr on parse-failure so the caller sees
          -- 'Variable not in scope: …' instead of a silent
          -- "raw: \"\", state: unparsed".
          let hintForAgent = case qr of
                QcUnparsed {} -> Just (summariseStderr stderrText)
                _             -> Nothing
          pure (renderResult qr hintForAgent)

-- | Resolve a property name to the source file it was defined in
-- by asking the GHC API. Returns 'Nothing' when the input isn't a
-- simple identifier, when parseName can't resolve it in the
-- current interactive scope, or when the resulting Name has no
-- RealSrcSpan (e.g. a name from a pre-built package). Callers then
-- fall back to the user-provided hint.
resolvePropertyModule :: GhcSession -> Text -> IO (Maybe Text)
resolvePropertyModule ghcSess nm
  | not (isSimpleIdent nm) = pure Nothing
  | otherwise = do
      -- Prime the session against the test-suite stanza first.
      -- The cached env a prior 'ghc_load' left behind may reflect
      -- a DIFFERENT target (e.g. library), in which case the test-
      -- suite's Main module is not in the graph and its
      -- 'prop_trivial' is invisible. 'firstTestSuiteOrLibrary'
      -- picks the test-suite when one exists (where named
      -- properties typically live); 'loadForTarget' then loads
      -- test/ + src/ sources under the test-suite's stanza flags,
      -- producing a module graph that contains Main alongside the
      -- library modules.
      tgt <- firstTestSuiteOrLibrary ghcSess
      _   <- try @SomeException (loadForTarget ghcSess tgt Strict)
      -- Walk the module graph: for each loaded module, scan its
      -- exports for a Name whose OccName matches the property.
      -- Beats 'parseName' here because (a) 'IIDecl (import Main)'
      -- doesn't expose Main's top-level names and (b) 'IIModule'
      -- requires interpreted mode while cabal compiles to objects.
      eRes <- try @SomeException $ withGhcSession ghcSess $ do
        mg <- getModuleGraph
        matches <- sequence
          [ do
              mi <- getModuleInfo (ms_mod ms)
              pure $ case mi of
                Nothing   -> Nothing
                Just info ->
                  case filter matchesName (modInfoExports info) of
                    (n:_) -> fileFromSpan (nameSrcSpan n)
                    []    -> Nothing
          | ms <- mgModSummaries mg
          ]
        pure (firstJust matches)
      pure $ case eRes of
        Left _           -> Nothing
        Right (Just fp)  -> Just (T.pack fp)
        Right Nothing    -> Nothing
  where
    matchesName n =
      occNameString (nameOccName n) == T.unpack nm
    firstJust = foldr (\x acc -> case x of Just _ -> x; Nothing -> acc) Nothing
    fileFromSpan :: SrcSpan -> Maybe FilePath
    fileFromSpan = \case
      RealSrcSpan s _ -> Just (unpackFS (srcSpanFile (s :: RealSrcSpan)))
      UnhelpfulSpan _ -> Nothing

-- | Run a QuickCheck property via @cabal v2-repl@ on the project's
-- test-suite target. Returns @(qcOutput, compileStderr)@:
--
--   * @qcOutput@  — the raw @Result.output@ text between our
--     sentinels. Fed to 'parseQuickCheckOutput' for pass/fail
--     classification. Empty when the load or compile failed
--     before QC could run.
--   * @compileStderr@ — the captured stderr from cabal v2-repl.
--     Previously discarded ('@_errStr@'); now bubbled up so the
--     handler can surface it as a 'hint' when parseQuickCheck
--     returns 'Unparsed'. Closes BUG-PLUS-09: a property that
--     references an out-of-scope name used to produce a
--     @raw: "", state: "unparsed"@ response with no explanation.
--
-- The statement we pipe into repl is the same shape as the
-- in-process Wave-3 one (show Result.output) — the only difference
-- is the execution vehicle. cabal invokes ghci with the correct
-- per-stanza flags, resolves deps (QuickCheck included) natively,
-- and returns clean.
runQuickCheckViaCabalRepl :: ProjectDir -> Maybe Text -> Text -> IO (Text, Text)
runQuickCheckViaCabalRepl pd mModule safeProp = do
  libMods <- libraryExposedModules pd
  let loadDirective = case mModule of
        Just modPath | not (T.null modPath) ->
          [":load " <> T.unpack modPath]
        _ -> []
      -- ':m +' widens the interactive context to every library
      -- exposed-module. Without this, a ':load test/Gen.hs' left
      -- only Gen's own imports in scope — so properties that
      -- referenced lib functions ('eval', 'simplify', …) failed
      -- with 'Variable not in scope'. Now the full library surface
      -- is reachable from any property body, matching the mental
      -- model of \"run this law against the project\".
      moduleImport
        | null libMods = []
        | otherwise    = [":m + " <> unwords (map T.unpack libMods)]
      input = unlines $
        loadDirective <>
        moduleImport <>
        [ "import Test.QuickCheck"
        -- Record-update with unqualified field names — GHC 9.12
        -- panics on the fully-qualified @Test.QuickCheck.chatty@
        -- variant inside a record update.
        , "let qcArgs = stdArgs { chatty = False }"
        , "r <- quickCheckWithResult qcArgs (" <> T.unpack safeProp <> ")"
        , "putStrLn \"__QC_OUTPUT_START__\""
        , "putStr (output r)"
        , "putStrLn \"__QC_OUTPUT_END__\""
        , ":q"
        ]
      -- Target @all@ by default. The repl loads the library +
      -- its exposed modules, so :load against a src/ file brings
      -- the user's definitions into scope without needing to
      -- know the module name.
      cp = (Proc.proc "cabal"
             [ "v2-repl", "all"
             , "--build-depends=QuickCheck"
             , "-v0"
             ])
             { Proc.cwd     = Just (unProjectDir pd)
             , Proc.std_in  = Proc.CreatePipe
             , Proc.std_out = Proc.CreatePipe
             , Proc.std_err = Proc.CreatePipe
             }
  (ec, outStr, errStr) <- Proc.readCreateProcessWithExitCode cp input
  let errText   = T.pack errStr
      stdoutT   = T.pack outStr
      qcSlice   = extractQcOutput stdoutT
  case ec of
    ExitSuccess    -> pure (qcSlice, errText)
    ExitFailure _c ->
      -- Even on a non-zero exit, the repl may have emitted the
      -- sentinels (e.g. the 'let qcArgs = ...' line failed after
      -- QC already ran). Prefer the sliced output when present;
      -- otherwise hand back the full stdout alongside stderr.
      pure (if T.null qcSlice then stdoutT else qcSlice, errText)

-- | Slice the chatty output between our sentinel markers. The
-- rest of cabal's chatter (ghci prompt, module-load lines,
-- "Leaving GHCi", …) is discarded; only the QuickCheck formatter's
-- text reaches the parser.
extractQcOutput :: Text -> Text
extractQcOutput full =
  let (_, afterStart) = T.breakOn "__QC_OUTPUT_START__" full
      body            = T.drop (T.length "__QC_OUTPUT_START__") afterStart
      (captured, _)   = T.breakOn "__QC_OUTPUT_END__" body
  in T.strip captured

--------------------------------------------------------------------------------
-- Issue #78 — labels-aware repl harness
--------------------------------------------------------------------------------

-- | Like 'runQuickCheckViaCabalRepl' but additionally emits the
-- structured @Result.labels@ map between sentinel markers so the
-- caller can recover QuickCheck's label histogram even when the
-- formatted output suppresses it (@chatty = False@ does that).
--
-- Returns @(qcOutput, labelsBlock, stderr)@ where:
--
--   * @qcOutput@ is the same slice 'extractQcOutput' would produce.
--   * @labelsBlock@ contains zero or more lines, each
--     @"<label-tab><count>"@.
--   * @stderr@ is cabal's combined stderr (loader noise + diags).
--
-- Used by 'ghc_witness' (#65) so its size-bucket distribution
-- doesn't depend on the chatty stream that 'chatty=False' kills.
runQuickCheckWithLabelsViaCabalRepl
  :: ProjectDir -> Maybe Text -> Text -> IO (Text, Text, Text)
runQuickCheckWithLabelsViaCabalRepl pd mModule safeProp = do
  libMods <- libraryExposedModules pd
  let loadDirective = case mModule of
        Just modPath | not (T.null modPath) ->
          [":load " <> T.unpack modPath]
        _ -> []
      moduleImport
        | null libMods = []
        | otherwise    = [":m + " <> unwords (map T.unpack libMods)]
      input = unlines $
        loadDirective <>
        moduleImport <>
        [ "import Test.QuickCheck"
        -- Use 'Data.Map' (from 'containers'). The cabal v2-repl
        -- invocation below pins '--build-depends=containers' so
        -- this import resolves regardless of what the project's
        -- own build-depends list looks like.
        , "import qualified Data.Map as M"
        , "import Data.List (intercalate)"
        , "let qcArgs = stdArgs { chatty = False }"
        , "r <- quickCheckWithResult qcArgs (" <> T.unpack safeProp <> ")"
        , "putStrLn \"__QC_OUTPUT_START__\""
        , "putStr (output r)"
        , "putStrLn \"__QC_OUTPUT_END__\""
        , "putStrLn \"__QC_LABELS_START__\""
        -- Emit one line per label: "<label-set-joined-by-+><tab><count>".
        -- Most properties carry single-label sets so the join is a
        -- no-op; for 'classify'-heavy properties we keep grouping
        -- intact so the agent can still recover joint frequencies.
        --
        -- Strip QC's quote-wrapping: 'collect x' calls 'label (show x)',
        -- so a String value 'x = "size:1-5"' becomes the label
        -- '"size:1-5"' (with the literal quote characters). We
        -- unquote here so the structured labels block carries the
        -- raw bucket name the agent sent in.
        , "let unquote s = if length s >= 2 && head s == '\"' && last s == '\"' then init (tail s) else s"
        , "mapM_ (\\(ks, n) -> putStrLn (intercalate \"+\" (map unquote ks) ++ \"\\t\" ++ show n)) (M.toList (labels r))"
        , "putStrLn \"__QC_LABELS_END__\""
        , ":q"
        ]
      cp = (Proc.proc "cabal"
             [ "v2-repl", "all"
             , "--build-depends=QuickCheck"
             , "--build-depends=containers"
             , "-v0"
             ])
             { Proc.cwd     = Just (unProjectDir pd)
             , Proc.std_in  = Proc.CreatePipe
             , Proc.std_out = Proc.CreatePipe
             , Proc.std_err = Proc.CreatePipe
             }
  (ec, outStr, errStr) <- Proc.readCreateProcessWithExitCode cp input
  let errText     = T.pack errStr
      stdoutT     = T.pack outStr
      qcSlice     = extractQcOutput     stdoutT
      labelsSlice = extractLabelsBlock  stdoutT
  case ec of
    ExitSuccess    -> pure (qcSlice, labelsSlice, errText)
    ExitFailure _c ->
      -- Same defensive fallback as the regular runner: if the
      -- repl failed late but the sentinels were already emitted,
      -- prefer the captured slice over the full noisy stdout.
      pure
        ( if T.null qcSlice then stdoutT else qcSlice
        , labelsSlice
        , errText
        )

-- | Issue #78: slice the labels block emitted between
-- @__QC_LABELS_START__@ and @__QC_LABELS_END__@. Returns the
-- block's body (zero or more @"label\\tcount"@ lines, one per
-- distinct label set the property's collect/classify calls
-- emitted). 'parseLabelCounts' in the witness tool turns this
-- into a structured @[(label, count)]@.
extractLabelsBlock :: Text -> Text
extractLabelsBlock full =
  let (_, afterStart) = T.breakOn "__QC_LABELS_START__" full
      body            = T.drop (T.length "__QC_LABELS_START__") afterStart
      (captured, _)   = T.breakOn "__QC_LABELS_END__" body
  in T.strip captured


--------------------------------------------------------------------------------
-- store-module resolution
--------------------------------------------------------------------------------

-- | Pure selector: given the property text, the caller's hint, and
-- (optionally) the @:info@ output, pick which path to persist.
--
-- Wave-3 kept for unit-test compatibility; the Wave-3 'handle' uses
-- the caller hint verbatim — the @:info@ plumbing that sat on top of
-- the subprocess ghci isn't reintroduced here because the regression
-- store only uses the module to reload the right compile scope.
chooseStoreModule :: Text -> Maybe Text -> Maybe Text -> Maybe Text
chooseStoreModule _prop callerHint _mInfo = callerHint

-- | True iff @t@ parses as a single Haskell identifier (possibly
-- qualified with dots, e.g. @Spec.prop_x@).
isSimpleIdent :: Text -> Bool
isSimpleIdent t = case T.uncons t of
  Nothing      -> False
  Just (c, cs) ->
    (isAlpha c || c == '_')
      && T.all validRest cs
  where
    validRest c = isAlphaNum c || c == '_' || c == '\'' || c == '.'

--------------------------------------------------------------------------------
-- response shaping
--------------------------------------------------------------------------------

-- | Shape the tool response. The 'Maybe Text' is an optional
-- @hint@ that only appears on 'QcUnparsed' (compile failed
-- upstream of QC). It carries the condensed stderr from cabal
-- v2-repl so the agent can read the error message directly
-- instead of staring at an empty @raw@ field.
-- | Issue #90 Phase C: each QuickCheck outcome maps to a typed
-- envelope. The pre-envelope payload (state, property, passed,
-- shrinks, counterexample, raw) is preserved verbatim under
-- 'result' so consumers branch on the structured 'state' field
-- exactly as before.
--
--   * 'QcPassed'    → status='ok'.
--   * 'QcFailed'    → status='failed'        kind='validation'.
--   * 'QcException' → status='failed'        kind='subprocess_error'
--                     (the property bombed mid-shrink). Special
--                     case: the timeout path passes a fixed string
--                     prefix; we surface it as 'inner_timeout'.
--   * 'QcGaveUp'    → status='no_match'      kind='not_in_scope'
--                     (semantically, no input satisfied the
--                     precondition; consumers branch on this
--                     same as on a missing fixture).
--   * 'QcUnparsed'  → status='failed'        kind='subprocess_error'
--                     (cabal repl printed something we can't
--                     parse — usually a load error; the optional
--                     hint surfaces the cleaned stderr).
renderResult :: QuickCheckResult -> Maybe Text -> ToolResult
renderResult qr mHint = case qr of
  QcPassed p n ->
    Env.toolResponseToResult (Env.mkOk (object
      [ "state"    .= ("passed" :: Text)
      , "property" .= p
      , "passed"   .= n
      ]))
  QcFailed p n shr cex ->
    let payload = object
          [ "state"          .= ("failed" :: Text)
          , "property"       .= p
          , "passed"         .= n
          , "shrinks"        .= shr
          , "counterexample" .= cex
          ]
        envErr   = Env.mkErrorEnvelope Env.Validation
                     ("Property failed at counterexample (after "
                       <> T.pack (show n) <> " passes, "
                       <> T.pack (show shr) <> " shrinks)")
        response = (Env.mkFailed envErr) { Env.reResult = Just payload }
    in Env.toolResponseToResult response
  QcException p err ->
    let payload = object
          [ "state"    .= ("exception" :: Text)
          , "property" .= p
          , "error"    .= err
          ]
        kind | "timeout:" `T.isPrefixOf` err = Env.InnerTimeout
             | otherwise                     = Env.SubprocessError
        envErr   = Env.mkErrorEnvelope kind err
        response = (Env.mkFailed envErr) { Env.reResult = Just payload }
    in Env.toolResponseToResult response
  QcGaveUp p n disc ->
    let payload = object
          [ "state"     .= ("gave_up" :: Text)
          , "property"  .= p
          , "passed"    .= n
          , "discarded" .= disc
          , "hint"      .= ( "Too many inputs rejected by precondition (==>). \
                             \Consider relaxing the precondition or writing a \
                             \custom generator." :: Text)
          ]
        envErr   = Env.mkErrorEnvelope Env.NotInScope
                     ("QuickCheck gave up after "
                       <> T.pack (show disc) <> " discards / "
                       <> T.pack (show n) <> " passes")
        response = (Env.mkNoMatch payload) { Env.reError = Just envErr }
    in Env.toolResponseToResult response
  QcUnparsed p raw ->
    let payload = object $
          [ "state"    .= ("unparsed" :: Text)
          , "property" .= p
          , "raw"      .= raw
          ] <> maybeHintPair mHint
        envErr   = Env.mkErrorEnvelope Env.SubprocessError
                     ( "Could not parse cabal repl output for property '"
                       <> p <> "'." )
        response = (Env.mkFailed envErr) { Env.reResult = Just payload }
    in Env.toolResponseToResult response
  where
    -- Attach the 'hint' key ONLY when the stderr actually carried
    -- a diagnostic; empty or whitespace-only stderr is worse than
    -- nothing (suggests we have an explanation when we don't).
    maybeHintPair (Just h) | not (T.null (T.strip h)) = [ "hint" .= h ]
    maybeHintPair _                                    = []

-- | Read the project's @.cabal@ file and return every module name
-- listed under the library's @exposed-modules@. Used to widen the
-- cabal-repl interactive context via @:m +@ so a property that
-- references library functions can compile even when the user
-- loaded only a test-helper module. Returns @[]@ on any parse or
-- I/O failure — the caller falls back to whatever scope @:load@
-- already provided.
libraryExposedModules :: ProjectDir -> IO [Text]
libraryExposedModules pd = do
  let root = unProjectDir pd
  ents <- try (listDirectory root) :: IO (Either SomeException [FilePath])
  case ents of
    Left _ -> pure []
    Right es ->
      case [root </> e | e <- es, takeExtension e == ".cabal"] of
        []    -> pure []
        (f:_) -> do
          eBody <- try (TIO.readFile f) :: IO (Either SomeException Text)
          case eBody of
            Left _     -> pure []
            Right body -> pure (scanLibraryExposedModules body)

-- | Pure parser: given a full @.cabal@ body, return library
-- exposed-module names. Scoped to the @library@ stanza via
-- 'Deps.sliceStanza'; returns @[]@ when the project has no
-- library (executable-only projects / benchmark-only projects).
--
-- A line-oriented parser lives here in-line — using the richer
-- 'HaskellFlows.Tool.CheckProject.parseExposedModules' would
-- introduce a module-graph cycle (CheckProject → CheckModule →
-- Regression → QuickCheck).
scanLibraryExposedModules :: Text -> [Text]
scanLibraryExposedModules body =
  case Deps.sliceStanza ("library", Nothing) (T.lines body) of
    Nothing             -> []
    Just (_, libLns, _) -> extractExposedModules libLns

-- | Given the lines of a SINGLE @library@ stanza, return every
-- module listed under @exposed-modules:@ — both inline on the
-- header and on continuation lines. Stops at the next cabal
-- field or stanza header.
extractExposedModules :: [Text] -> [Text]
extractExposedModules = go False
  where
    go _ [] = []
    go inside (ln : rest)
      | isExposedHeader ln =
          let inlineTail = T.strip (T.dropWhile (/= ':') ln)
              inlineNow  = T.strip (T.drop 1 inlineTail)
              nameHere   = [ inlineNow | not (T.null inlineNow) ]
          in nameHere <> go True rest
      | inside && isContinuation ln =
          let nm = T.strip ln
              newField = ':' `T.elem` nm
          in if newField
               then go False rest
               else [ nm | not (T.null nm) ] <> go True rest
      | otherwise = go False rest

    isExposedHeader ln =
      "exposed-modules:" `T.isPrefixOf` T.toLower (T.stripStart ln)

    -- A continuation is an indented line; blank lines also end the block.
    isContinuation ln =
      not (T.null (T.takeWhile (== ' ') ln)) && not (T.null (T.strip ln))

-- | Compress the v2-repl stderr into the useful bits: drop
-- cabal's own banner lines ("Warning: …", "[build-profile]",
-- …) and cap the payload so the tool response stays JSON-RPC-
-- friendly. Agents get the first GHC error plus at most a few
-- lines of context.
summariseStderr :: Text -> Text
summariseStderr raw =
  let ls           = T.lines raw
      informative  = filter isInformative ls
      kept         = take 20 informative
      joined       = T.intercalate "\n" kept
      capped       = T.strip joined
  in if T.length capped > 1600
       then T.take 1600 capped <> "\n…(truncated)"
       else capped
  where
    isInformative ln =
      let l = T.toLower (T.strip ln)
      in not (T.null l)
         && not ("warning:" `T.isPrefixOf` l
                 && " -w" `T.isInfixOf` l)  -- cabal's own "-W" banner
         && not ("resolving dependencies" `T.isPrefixOf` l)
         && not ("build profile" `T.isPrefixOf` l)


