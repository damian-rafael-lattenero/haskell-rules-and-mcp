-- | @ghc_workflow@ — meta-tool that summarises the state of the server
-- and suggests the next action.
--
-- The TS port has a stateful workflow engine that tracks per-module
-- progress, which functions have tests, which gates are pending, etc.
-- This Haskell Phase-5 port ships the /observable/ subset: actions that
-- can be answered purely from the session's current state (is a GHCi
-- child alive? is the project dir set? etc.). Per-module workflow
-- state will grow in Phase 6 once the property-store is ported and we
-- can persist per-function facts.
--
-- This is intentionally read-only. It never spawns GHCi, never mutates
-- the session — safe to call at any time, including from an agent that
-- just errored and wants to know what's reachable.
module HaskellFlows.Tool.Workflow
  ( descriptor
  , handle
  , WorkflowArgs (..)
  , Action (..)
    -- * Exposed for unit tests
  , pickModuleLine
  , render
  , discoverRanked
  , postMortemPayload
  , planPayload
  ) where

import Control.Concurrent.MVar (MVar, readMVar)
import Control.Exception (SomeException, try)
import Data.Aeson
import qualified Data.Aeson.Key as Key
import Data.Aeson.Types (parseEither)
import Data.IORef (IORef, readIORef)
import Data.List (find, sortBy)
import Data.Char (isAlphaNum, isUpper)
import Data.Maybe (fromMaybe, isNothing, listToMaybe, mapMaybe)
import Data.Ord (Down (..), comparing)
import Data.Time.Clock (UTCTime, diffUTCTime, getCurrentTime)
import qualified Data.Set as Set
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.IO as TIO
import qualified System.Directory
import System.Directory (findExecutable)
import System.FilePath ((</>))

import qualified HaskellFlows.Data.Scratchpad as SP
import qualified HaskellFlows.Mcp.Envelope as Env
import HaskellFlows.Ghc.ApiSession (GhcSession)
import HaskellFlows.Mcp.Protocol
import HaskellFlows.Mcp.ToolName
  ( ToolName (..)
  , ToolCategory (..)
  , allToolNames
  , toolCategory
  , toolCategoryText
  , toolNameText
  )
import HaskellFlows.Mcp.Staleness (StalenessReport (..))
import HaskellFlows.Mcp.WorkflowState
  ( SessionPhase (..)
  , WorkflowState (..)
  , classifyPhase
  , renderHelp
  , renderPhaseHint
  , sessionMissedOpportunities
  )
import qualified HaskellFlows.Tool.ToolchainStatus as TC
import HaskellFlows.Types (ProjectDir, unProjectDir)

descriptor :: ToolDescriptor
descriptor =
  ToolDescriptor
    { tdName        = toolNameText GhcWorkflow
    , tdDescription =
        "PURPOSE: Report MCP / session state and the context-aware next "
          <> "action; read-only. "
          <> "WHEN: session-start handshake (action='status'); when unsure "
          <> "what to do next (action='help'); action='discover' ranks the "
          <> "tools you have NOT used this session by relevance to the "
          <> "current phase; action='post-mortem' gives a session retro "
          <> "(counts + missed opportunities); action='plan' (goal=...) "
          <> "turns a one-line goal into a ghc_batch-ready chain. "
          <> "WHEN NOT: ghc_toolchain to probe external binaries; the "
          <> "per-response nextStep already covers most next-step moments. "
          <> "PREREQUISITES: none — never spawns or mutates a GHCi session. "
          <> "OUTPUT: per-action view — status {projectDir, phase, "
          <> "toolsActive, staleness, session_activity}; help {steps, "
          <> "phaseHint}; discover {unused:[{tool, category, why_now}]}; "
          <> "post-mortem {session_duration_ms, tools_called, "
          <> "missed_opportunities, ...}; plan {matched_template, chain, "
          <> "confidence, alternative_templates}. "
          <> "SEE ALSO: ghc_toolchain, ghc_check_project."
    , tdInputSchema =
        object
          [ "type"       .= ("object" :: Text)
          , "properties" .= object
              [ "action" .= object
                  [ "type"        .= ("string" :: Text)
                  , "enum"        .= (["status", "help", "discover", "post-mortem", "plan"] :: [Text])
                  , "description" .=
                      ("Which view to return. Default: 'status'." :: Text)
                  ]
              , "goal" .= object
                  [ "type"        .= ("string" :: Text)
                  , "description" .=
                      ("For action='plan': a one-line goal to turn into a \
                       \ghc_batch-ready chain, e.g. 'set up Expr.Foo with a \
                       \QC roundtrip'." :: Text)
                  ]
              ]
          , "additionalProperties" .= False
          ]
    }

data Action = ActStatus | ActHelp | ActDiscover | ActPostMortem | ActPlan Text
  deriving stock (Eq, Show)

newtype WorkflowArgs = WorkflowArgs
  { waAction :: Action
  }
  deriving stock (Show)

instance FromJSON WorkflowArgs where
  parseJSON = withObject "WorkflowArgs" $ \o -> do
    mAct <- o .:? "action"
    a    <- case mAct :: Maybe Text of
      Nothing        -> pure ActStatus
      Just "status"  -> pure ActStatus
      Just "help"     -> pure ActHelp
      Just "discover" -> pure ActDiscover
      Just "post-mortem" -> pure ActPostMortem
      Just "plan"     -> ActPlan <$> o .:? "goal" .!= ""
      Just other     -> fail ("unknown action: " <> T.unpack other)
    pure (WorkflowArgs a)

-- | The @toolNames@ argument is the canonical tool list provided by
-- 'HaskellFlows.Mcp.Server' — the same list that feeds @tools/list@.
-- Passing it in keeps this module free of a dependency on Server
-- (no import cycle) while guaranteeing the @toolsActive@ view can
-- never drift from the @tools/list@ surface again.
--
-- BUG-07: 'StalenessReport' surfaces as the @staleness@ field of
-- the status / help views so the agent (and the user, via chat)
-- sees the "rebuild vs running binary" gap without hunting for it.
-- BUG-08 + BUG-24: the raw 'WorkflowState' is passed through so
-- this module can call 'renderHelp' and 'renderPhaseHint'
-- directly, avoiding the previous "Server pre-renders + passes
-- as flat [Text]" indirection that hid information the help view
-- could use.
handle
  :: IORef ProjectDir
  -> MVar (Maybe GhcSession)
  -> [Text]
  -> WorkflowState
  -> StalenessReport
  -> Bool                  -- ^ PR-4 Phase 1: is the active project the MCP itself?
  -> IORef SP.Store        -- ^ #253 Phase 5: scratchpad store for status section
  -> Value
  -> IO ToolResult
handle pdRef sessMVar toolNames ws staleness isSelfProject scratchRef rawArgs =
  case parseEither parseJSON rawArgs of
    Left err ->
      pure (Env.toolResponseToResult (Env.mkFailed
        ((Env.mkErrorEnvelope (parseErrorKind err)
            (T.pack ("Invalid arguments: " <> err)))
              { Env.eeCause = Just (T.pack err) })))
    Right (WorkflowArgs ActPostMortem) ->
      -- #266: post-mortem needs wall-clock 'now' for the session
      -- duration, so it is handled here (IO) rather than in the pure
      -- 'render'. Reads only the WorkflowState the Server threads in.
      Env.toolResponseToResult . Env.mkOk . postMortemPayload ws
        <$> getCurrentTime
    Right (WorkflowArgs a) -> do
      pd        <- readIORef pdRef
      sessAlive <- isAlive sessMVar
      -- One @findExecutable@ per optional binary — cheap (PATH stat,
      -- microseconds per name) and gives the boolean availability the
      -- nudge needs without paying for the @--version@ probe that
      -- 'ghc_toolchain status' does. See 'TC.optionalBinaryNames' for
      -- the canonical input list.
      missing   <- probeMissingOptionals
      -- F-02: for the help view, resolve the entry module so step 1
      -- carries a concrete suggestion rather than "your entry module".
      entryMod  <- case a of
        ActHelp -> suggestEntryModule pd
        _       -> pure Nothing
      -- #253 Phase 5: compute scratchpad summary only for status view.
      -- loadAll is read-only and cheap (one file stat + JSON decode).
      scratchSection <- case a of
        ActStatus -> do
          scratch  <- readIORef scratchRef
          entries  <- SP.loadAll scratch
          pure (Just (scratchpadSection entries))
        _ -> pure Nothing
      pure (render a pd sessAlive toolNames ws staleness missing entryMod isSelfProject scratchSection)

probeMissingOptionals :: IO [Text]
probeMissingOptionals =
  map fst . filter (isNothing . snd)
    <$> mapM probe TC.optionalBinaryNames
  where
    probe :: Text -> IO (Text, Maybe FilePath)
    probe name = do
      mp <- findExecutable (T.unpack name)
      pure (name, mp)

-- | Discriminate the FromJSON failure shape so the envelope's
-- error.kind reflects what actually went wrong: an unknown
-- 'action' value lands as 'Validation' (the value is structurally
-- valid JSON, just outside the enum); a missing required field
-- lands as 'MissingArg'; everything else falls back to
-- 'TypeMismatch'. Substring-detection is fragile but the
-- alternative (custom Aeson runner) is heavier than this surface
-- needs.
parseErrorKind :: String -> Env.ErrorKind
parseErrorKind err
  | "unknown action" `isInfixOfStr` err = Env.Validation
  | "key" `isInfixOfStr` err            = Env.MissingArg
  | otherwise                           = Env.TypeMismatch
  where
    isInfixOfStr needle haystack =
      let n = length needle
      in any (\i -> take n (drop i haystack) == needle)
             [0 .. length haystack - n]

isAlive :: MVar (Maybe GhcSession) -> IO Bool
isAlive sessMVar = do
  m <- readMVar sessMVar
  pure (case m of Nothing -> False; Just _ -> True)

--------------------------------------------------------------------------------
-- response shaping
--------------------------------------------------------------------------------

-- | Render the workflow view. The three branches share the same
-- top-level shape so agents can treat the tool's output polymorphically.
-- | Render the workflow view via the unified envelope. Issue #90
-- Phase B: 'ghc_workflow' is read-only — every successful call is
-- 'Env.StatusOk' carrying the requested view inside 'result'.
-- The legacy 'success: true' is auto-derived; the per-action
-- payloads stay shape-stable so consumers that read e.g.
-- 'projectDir' / 'toolsActive' / 'phase' need no client-side
-- changes.
render
  :: Action
  -> ProjectDir
  -> Bool
  -> [Text]
  -> WorkflowState
  -> StalenessReport
  -> [Text]
  -> Maybe Text       -- ^ F-02: suggested entry module (help view only)
  -> Bool             -- ^ PR-4 Phase 1: is the active project the MCP itself?
  -> Maybe Value      -- ^ #253 Phase 5: pre-computed scratchpad section (status only)
  -> ToolResult
render a pd alive toolNames ws staleness missingOpt mEntry isSelfProject mScratch =
  let phase      = classifyPhase ws
      stateHints = renderHelp ws
      -- #257: per-session activity surfaced for #263 (discover) and
      -- #266 (post-mortem). 'unused_count' is the registry size minus
      -- the cumulative ever-called set.
      sessionActivity = object
        [ "tools_called"  .= wsToolCalls ws
        , "unique_called" .= Set.size (wsEverCalled ws)
        , "unused_count"  .= max 0 (length toolNames - Set.size (wsEverCalled ws))
        , "error_streak"  .= wsErrorStreak ws
        , "started_at"    .= wsStarted ws
        ]
      payload    = case a of
        ActStatus   -> statusPayload pd alive toolNames staleness phase missingOpt isSelfProject mScratch sessionActivity
        ActHelp     -> helpPayload pd alive stateHints staleness phase mEntry
        ActDiscover -> discoverPayload ws phase
        ActPlan goal -> planPayload goal
  in Env.toolResponseToResult (Env.mkOk payload)

-- | #263: the ranked top-5 unused tools for the current phase. Pure +
-- exported so the ranking heuristic is unit-testable without going
-- through the JSON envelope. Excludes every tool already called this
-- session ('wsEverCalled', #257).
discoverRanked :: WorkflowState -> SessionPhase -> [ToolName]
discoverRanked ws phase =
  take 5
    (sortBy (comparing (Down . scoreTool phase))
       [ t | t <- allToolNames, t `Set.notMember` wsEverCalled ws ])

-- | Relevance score: a per-phase boost for the tools that matter in
-- that phase, plus a small category tiebreak (primitive > composite >
-- gate > control-plane).
scoreTool :: SessionPhase -> ToolName -> Int
scoreTool phase t = phaseScore + catScore
  where
    catScore = case toolCategory t of
      CatPrimitive    -> 3
      CatComposite    -> 2
      CatGate         -> 1
      CatControlPlane -> 0
    boost xs = if t `elem` xs then 10 else 0
    phaseScore = case phase of
      PhasePreScaffold -> boost [GhcProject, GhcLoad, GhcToolchain]
      PhaseBootstrap   -> boost [GhcDeps, GhcModules, GhcAddImport, GhcLoad]
      PhaseDeveloping  -> boost [GhcScratch, GhcHole, GhcSuggest, GhcType, GhcInfo, GhcComplete]
      PhaseTestingLaws -> boost [GhcSuggest, GhcQuickCheck, GhcWitness, GhcLab, GhcArbitrary]
      PhaseReadyToPush -> boost [GhcGate, GhcCoverage, GhcPropertyStore, GhcCheckProject, GhcLint, GhcPerf]

-- | A short "why this matters now" line per tool, with a
-- category-derived fallback. Kept compact — the goal is to nudge.
whyNow :: ToolName -> Text
whyNow t = case t of
  GhcScratch      -> "Type-check a hypothesis before editing source — faster and reversible."
  GhcLab          -> "Discover + run QuickCheck laws for every binding in a module in one call."
  GhcSuggest      -> "Derive candidate QuickCheck laws from a function's type signature."
  GhcWitness      -> "Sanity-check a property's input distribution — catch trivial-input bias."
  GhcPerf         -> "Wall-clock baseline for an expression; compare later to catch regressions."
  GhcComplete     -> "Prefix-complete in-scope identifiers when you half-remember a name."
  GhcExplainError -> "Decode a confusing type error and verify a candidate patch."
  GhcCoverage     -> "Find untested code paths via HPC before pushing."
  GhcGate         -> "One-shot pre-push gate: regression + cabal test + cabal build."
  GhcHole         -> "List a stub's typed holes with expected types + in-scope fits."
  _               -> "Unused this session — a "
                       <> toolCategoryText (toolCategory t) <> " tool worth a look."

-- | #263: render the discover view — top-5 unused tools, ranked, each
-- with a category + a why_now nudge, plus the total unused count.
discoverPayload :: WorkflowState -> SessionPhase -> Value
discoverPayload ws phase =
  let ranked  = discoverRanked ws phase
      total   = length [ t | t <- allToolNames, t `Set.notMember` wsEverCalled ws ]
      entry t = object
        [ "tool"     .= toolNameText t
        , "category" .= toolCategoryText (toolCategory t)
        , "why_now"  .= whyNow t
        ]
  in object
       [ "view"         .= ("discover" :: Text)
       , "phase"        .= T.pack (show phase)
       , "unused"       .= map entry ranked
       , "unused_total" .= total
       ]

-- | #266: render the post-mortem retro. Pure (given 'now') + exported
-- for unit tests. Cumulative counts come from the #257 fields; the
-- missed-opportunity list reuses 'sessionMissedOpportunities'. Finer
-- sequence patterns + cross-session persistence are deferred.
postMortemPayload :: WorkflowState -> UTCTime -> Value
postMortemPayload ws now =
  let durationMs =
        round (realToFrac (diffUTCTime now (wsStarted ws)) * 1000 :: Double) :: Int
      unique = Set.size (wsEverCalled ws)
  in object
       [ "view"                 .= ("post-mortem" :: Text)
       , "session_duration_ms"  .= max 0 durationMs
       , "tools_called"         .= wsToolCalls ws
       , "tools_unique"         .= unique
       , "tools_unused"         .= max 0 (length allToolNames - unique)
       , "passed_properties"    .= wsPassedProperties ws
       , "error_streak"         .= wsErrorStreak ws
       , "phase"                .= T.pack (show (classifyPhase ws))
       , "recent_tools"         .= map toolNameText (wsToolHistory ws)
       , "missed_opportunities" .= sessionMissedOpportunities ws
       ]

--------------------------------------------------------------------------------
-- #264 — ghc_workflow(action="plan"): NL goal -> ghc_batch-ready chain
--------------------------------------------------------------------------------

-- | A curated plan template: a name, the lowercase trigger keywords the
-- matcher scores a goal against, and a builder that turns an optional
-- module hint (parsed off the goal) into a concrete ghc_batch chain.
-- Inlined here (not a separate Templates module) to avoid a .cabal edit.
data PlanTemplate = PlanTemplate
  { ptName     :: !Text
  , ptKeywords :: ![Text]
  , ptBuild    :: Maybe Text -> [Value]
  }

-- | One chain step in the @ghc_batch@ shape: @{tool, args}@.
planStep :: ToolName -> Value -> Value
planStep t args = object [ "tool" .= toolNameText t, "args" .= args ]

planModName :: Maybe Text -> Text
planModName = fromMaybe "Your.Module"

planModPath :: Maybe Text -> Text
planModPath mh = "src/" <> T.replace "." "/" planMod <> ".hs"
  where planMod = planModName mh

-- | The curated catalog (15 templates over the common flows).
planTemplates :: [PlanTemplate]
planTemplates =
  [ PlanTemplate "module-with-qc-property"
      ["quickcheck", "qc", "property", "roundtrip", "law"]
      (\mh -> [ planStep GhcModules (object ["action" .= ("add" :: Text), "modules" .= [planModName mh]])
              , planStep GhcQuickCheck (object ["property" .= ("\\x -> f x === g x" :: Text), "module_path" .= planModPath mh])
              ])
  , PlanTemplate "module-only"
      ["new module", "add module", "exposed-module", "scaffold module"]
      (\mh -> [ planStep GhcModules (object ["action" .= ("add" :: Text), "modules" .= [planModName mh]]) ])
  , PlanTemplate "add-dep-then-import"
      ["dependency", "add package", "build-depends", "add dep"]
      (const [ planStep GhcDeps (object ["action" .= ("add" :: Text), "package" .= ("<pkg>" :: Text), "stanza" .= ("library" :: Text)])
             , planStep GhcAddImport (object ["name" .= ("<Module.To.Import>" :: Text)])
             ])
  , PlanTemplate "refactor-then-verify"
      ["refactor", "extract"]
      (\mh -> [ planStep GhcRefactor (object ["action" .= ("rename_local" :: Text), "module_path" .= planModPath mh, "old_name" .= ("<old>" :: Text), "new_name" .= ("<new>" :: Text), "scope_line_start" .= (1 :: Int), "scope_line_end" .= (1 :: Int)])
              , planStep GhcCheckModule (object ["module_path" .= planModPath mh])
              ])
  , PlanTemplate "property-discovery"
      ["discover laws", "lab", "audit module", "properties for"]
      (\mh -> [ planStep GhcLab (object ["module_path" .= planModPath mh])
              , planStep GhcPropertyStore (object ["action" .= ("run" :: Text)])
              ])
  , PlanTemplate "coverage-report"
      ["coverage", "hpc", "untested"]
      (const [ planStep GhcCoverage (object [])
             , planStep GhcPropertyStore (object ["action" .= ("export" :: Text)])
             ])
  , PlanTemplate "perf-baseline"
      ["perf", "benchmark", "baseline", "performance", "profile"]
      (const [ planStep GhcPerf (object ["expression" .= ("<expr>" :: Text), "save_baseline" .= True]) ])
  , PlanTemplate "bootstrap-project"
      ["new project", "create project", "bootstrap", "from scratch"]
      (\mh -> [ planStep GhcProject (object ["action" .= ("create" :: Text), "name" .= ("<pkg-name>" :: Text)])
              , planStep GhcDeps (object ["action" .= ("add" :: Text), "package" .= ("QuickCheck" :: Text), "stanza" .= ("test-suite" :: Text)])
              , planStep GhcLoad (object ["module_path" .= planModPath mh])
              ])
  , PlanTemplate "rename-local"
      ["rename local", "rename binding", "rename variable", "rename"]
      (\mh -> [ planStep GhcRefactor (object ["action" .= ("rename_local" :: Text), "module_path" .= planModPath mh, "old_name" .= ("<old>" :: Text), "new_name" .= ("<new>" :: Text), "scope_line_start" .= (1 :: Int), "scope_line_end" .= (1 :: Int)])
              , planStep GhcCheckModule (object ["module_path" .= planModPath mh])
              ])
  , PlanTemplate "move-symbol"
      ["move symbol", "move function", "relocate", "move to"]
      (const [ planStep GhcRefactor (object ["action" .= ("move_symbol" :: Text), "symbol" .= ("<name>" :: Text), "from" .= ("src/From.hs" :: Text), "to" .= ("src/To.hs" :: Text)])
             , planStep GhcCheckProject (object [])
             ])
  , PlanTemplate "fix-warning-loop"
      ["fix warning", "warnings", "clean warnings"]
      (\mh -> [ planStep GhcFixWarning (object ["module_path" .= planModPath mh])
              , planStep GhcLoad (object ["module_path" .= planModPath mh, "diagnostics" .= True])
              ])
  , PlanTemplate "audit-properties"
      ["audit", "contradiction", "consistency"]
      (const [ planStep GhcPropertyStore (object ["action" .= ("audit" :: Text)])
             , planStep GhcPropertyStore (object ["action" .= ("list" :: Text)])
             ])
  , PlanTemplate "export-test-suite"
      ["export", "materialise", "materialize", "spec.hs", "test suite"]
      (const [ planStep GhcPropertyStore (object ["action" .= ("export" :: Text)])
             , planStep GhcGate (object [])
             ])
  , PlanTemplate "find-via-hoogle"
      ["hoogle", "find function", "search for", "which function"]
      (const [ planStep HoogleSearch (object ["query" .= ("<type or name>" :: Text)])
             , planStep GhcAddImport (object ["name" .= ("<one of the hits>" :: Text)])
             ])
  , PlanTemplate "pre-push-gate"
      ["push", "gate", "ship", "finalize", "ready to push", "pre-push"]
      (const [ planStep GhcGate (object []) ])
  ]

-- | Keyword-score a goal against the catalog. Returns the best match
-- (when at least one keyword hit), a 0..1 confidence (2+ hits = full),
-- and up to 3 alternative template names.
matchTemplate :: Text -> (Maybe PlanTemplate, Double, [Text])
matchTemplate goal =
  let g      = T.toLower goal
      scored = sortBy (comparing (Down . snd))
                 [ (t, length (filter (`T.isInfixOf` g) (ptKeywords t)))
                 | t <- planTemplates ]
  in case scored of
       ((best, bs) : rest) | bs > 0 ->
         ( Just best
         , min 1.0 (fromIntegral bs / 2.0)
         , [ ptName t | (t, s) <- take 3 rest, s > 0 ] )
       _ -> (Nothing, 0.0, take 3 (map ptName planTemplates))

-- | Pull the first module-ish token (Capitalised, optionally dotted)
-- out of the goal, e.g. "set up Expr.Foo with QC" -> Just "Expr.Foo".
extractModule :: Text -> Maybe Text
extractModule goal = find isModuleToken (T.words goal)
  where
    isModuleToken w = case T.uncons w of
      Just (c, _) -> isUpper c && T.all (\x -> isAlphaNum x || x == '.') w
      Nothing     -> False

-- | #264: render the plan view — the matched template's concrete chain
-- (ghc_batch-ready), a confidence, and alternatives. Deterministic.
planPayload :: Text -> Value
planPayload goal =
  let mh                  = extractModule goal
      (mBest, conf, alts) = matchTemplate goal
  in object $
       [ "view"       .= ("plan" :: Text)
       , "goal"       .= goal
       , "confidence" .= conf
       ]
       <> case mBest of
            Just t | conf >= 0.5 ->
              [ "matched_template"      .= ptName t
              , "chain"                 .= ptBuild t mh
              , "alternative_templates" .= alts
              ]
            _ ->
              [ "matched_template"      .= Null
              , "chain"                 .= ([] :: [Value])
              , "alternative_templates" .= alts
              ]

statusPayload
  :: ProjectDir -> Bool -> [Text] -> StalenessReport -> SessionPhase
  -> [Text] -> Bool -> Maybe Value -> Value -> Value
statusPayload pd alive toolNames staleness phase missingOpt isSelfProject mScratch sessionActivity =
  object $
    [ "view"        .= ("status" :: Text)
    , "projectDir"  .= T.pack (unProjectDir pd)
    , "ghciAlive"   .= alive
    , "toolsActive" .= toolNames
    , "phase"       .= T.pack (show phase)
      -- BUG-07: full 'StalenessReport' body. Agents that care
      -- about "is my binary stale?" get the @stale@ bool + a
      -- human-readable @message@ without a second tool call.
    , "staleness"   .= staleness
      -- PR-4 Phase 1: cabal-name heuristic. True iff 'projectDir'
      -- points at a haskell-flows MCP source tree. Agents working
      -- on the MCP itself (`/Users/.../mcp-server-haskell` or the
      -- repo root) can use this to switch into the dogfood-fix
      -- flow without checking paths themselves.
    , "selfProject" .= isSelfProject
      -- #253 Phase 5: scratchpad summary — always present in status.
    , "scratchpad"  .= mScratch
      -- #257: per-session activity (tools_called / unique_called /
      -- unused_count / error_streak / started_at) for discover + post-mortem.
    , "session_activity" .= sessionActivity
    ]
    -- Only emit the 'optionalBinaries' field when something is
    -- missing.  Happy path stays clean — the nudge only appears
    -- when there's something to nudge about.
    <> [ "optionalBinaries" .= optionalBinariesPayload missingOpt
       | not (null missingOpt)
       ]

-- | #253 Phase 5: compact scratchpad summary for the status view.
scratchpadSection :: [SP.ScratchEntry] -> Value
scratchpadSection entries =
  let total    = length entries
      open     = length (filter ((== SP.ScratchOpen)      . SP.seStatus) entries)
      verified = length (filter ((== SP.ScratchVerified)  . SP.seStatus) entries)
      promoted = length (filter ((== SP.ScratchPromoted)  . SP.seStatus) entries)
      hint :: Text
      hint
        | total == 0 =
            "Use ghc_scratch(action='write') to record a Haskell hypothesis \
            \you want to type-check before touching source."
        | open > 0  =
            T.pack (show open) <> " open entr" <> (if open == 1 then "y" else "ies")
            <> " — run ghc_scratch(action='check', id='<id>') to type-check."
        | otherwise =
            "All entries verified or promoted. Use action='clear' with \
            \confirm=true to reset the scratchpad."
  in object
       [ "entries"  .= total
       , "open"     .= open
       , "verified" .= verified
       , "promoted" .= promoted
       , "hint"     .= hint
       ]

-- | Render the missing-optional-binaries payload — used by the agent
-- (and by the user, via chat) to decide whether to install the
-- skipped binaries before going further.  Shape:
--
-- @
--   { "missing":       ["fourmolu", "ormolu", ...]
--   , "install_hints": { "fourmolu": "cabal install fourmolu", ... }
--   , "summary":       "4 optional binaries missing — your MCP works
--                       but ghc_format / hoogle_search will return
--                       status='unavailable'."
--   }
-- @
optionalBinariesPayload :: [Text] -> Value
optionalBinariesPayload missing =
  object
    [ "missing"       .= missing
    , "install_hints" .= object
        [ Key.fromText name .= TC.installHintFor name | name <- missing ]
    , "summary"       .=
        ( T.pack (show (length missing))
       <> " optional binaries missing — your MCP works but tools that"
       <> " delegate to them will return status='unavailable'. Run:\n  "
       <> T.intercalate "\n  " (map TC.installHintFor missing) )
    ]

helpPayload
  :: ProjectDir -> Bool -> [Text] -> StalenessReport -> SessionPhase
  -> Maybe Text -> Value
helpPayload _pd alive stateHints staleness phase mEntry =
  object $
    [ "view"       .= ("help" :: Text)
    , "ghciAlive"  .= alive
    , "phase"      .= T.pack (show phase)
    , "phaseHint"  .= renderPhaseHint phase
    , "steps"      .= steps
    , "reasoning"  .= reasoning
    , "staleness"  .= staleness
    ]
    <> [ "stateHints"    .= stateHints | not (null stateHints) ]
    <> [ "entry_module"  .= m          | Just m <- [mEntry] ]
  where
    -- F-02: step 1 includes the concrete module path when we could
    -- find it in the cabal file, so the agent has an actionable hint
    -- rather than a generic "your entry module" placeholder.
    loadStep = case mEntry of
      Nothing -> "1. Call ghc_load with your entry module to boot GHCi."
      Just m  -> "1. Call ghc_load(module_path=\"" <> m
                   <> "\") to boot GHCi."

    steps :: [Text]
    steps
      | not alive =
          [ loadStep
          , "2. For data types you'll test: ghc_arbitrary (type_name=...)."
          , "3. For stubs with _ holes: ghc_hole (module_path=...)."
          , "4. For properties: ghc_quickcheck (property=...)."
          ]
      | otherwise =
          [ "1. ghc_load (diagnostics=true) to catch holes + errors."
          , "2. ghc_hole if holes surfaced."
          , "3. ghc_type to confirm subexpressions compose."
          , "4. ghc_quickcheck once a law is testable."
          , "5. hoogle_search when stuck on which library function fits."
          ]

    reasoning :: Text
    reasoning =
      if alive
        then "GHCi is alive, so the property-first loop is open: keep \
             \the compile/type/quickcheck triangle tight before touching \
             \external tools."
        else "No active GHCi session. Start by loading the module you \
             \want to work on — every other tool will auto-boot on first \
             \use anyway, but ghc_load gives you the cleanest error \
             \surface."

--------------------------------------------------------------------------------
-- F-02: entry-module suggestion
--------------------------------------------------------------------------------

-- | Look for the first @.cabal@ file in @pd@ and return the first
-- @exposed-modules@ or @main-is@ value as a @src/<module>.hs@ hint.
-- Returns 'Nothing' on any failure; the caller degrades gracefully.
suggestEntryModule :: ProjectDir -> IO (Maybe Text)
suggestEntryModule pd = do
  eFiles <- try (System.Directory.listDirectory (unProjectDir pd))
              :: IO (Either SomeException [FilePath])
  case eFiles of
    Left  _     -> pure Nothing
    Right files ->
      case listToMaybe [ unProjectDir pd </> f
                       | f <- files
                       , ".cabal" `T.isSuffixOf` T.pack f ] of
        Nothing   -> pure Nothing
        Just path -> do
          res <- try (TIO.readFile path) :: IO (Either SomeException Text)
          case res of
            Left _     -> pure Nothing
            Right body -> pure (pickModuleLine body)

-- | Extract the first useful module name from a cabal file body.
-- Looks for @exposed-modules:@ or @main-is:@; maps module names to
-- the canonical src/ path so the agent gets a copy-pasteable path.
pickModuleLine :: Text -> Maybe Text
pickModuleLine body =
  listToMaybe (mapMaybe pick (T.lines body))
  where
    pick ln =
      let s = T.stripStart ln
      in if "exposed-modules:" `T.isPrefixOf` T.toLower s
           then let rest = T.drop 1 (T.dropWhile (/= ':') s)
                    m    = T.strip (T.takeWhile (/= ',') rest)
                in if T.null m then Nothing
                   else Just ("src/" <> T.replace "." "/" m <> ".hs")
         else if "main-is:" `T.isPrefixOf` T.toLower s
           then let rest = T.strip (T.drop 1 (T.dropWhile (/= ':') s))
                in if T.null rest then Nothing
                   else Just ("app/" <> rest)
         else Nothing
