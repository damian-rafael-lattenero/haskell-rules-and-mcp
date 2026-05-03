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
  ) where

import Control.Concurrent.MVar (MVar, readMVar)
import Control.Exception (SomeException, try)
import Data.Aeson
import qualified Data.Aeson.Key as Key
import Data.Aeson.Types (parseEither)
import Data.IORef (IORef, readIORef)
import Data.Maybe (isNothing, listToMaybe, mapMaybe)
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.IO as TIO
import qualified System.Directory
import System.Directory (findExecutable)
import System.FilePath ((</>))

import qualified HaskellFlows.Mcp.Envelope as Env
import HaskellFlows.Ghc.ApiSession (GhcSession)
import HaskellFlows.Mcp.Protocol
import HaskellFlows.Mcp.ToolName (ToolName (..), toolNameText)
import HaskellFlows.Mcp.Staleness (StalenessReport (..))
import HaskellFlows.Mcp.WorkflowState
  ( SessionPhase
  , WorkflowState (..)
  , classifyPhase
  , renderHelp
  , renderPhaseHint
  )
import qualified HaskellFlows.Tool.ToolchainStatus as TC
import HaskellFlows.Types (ProjectDir, unProjectDir)

descriptor :: ToolDescriptor
descriptor =
  ToolDescriptor
    { tdName        = toolNameText GhcWorkflow
    , tdDescription =
        "Query the server's workflow state. Actions: 'status' (server "
          <> "inventory), 'help' (what to do next, context-aware), 'next' "
          <> "(single most likely next tool call). Read-only; never spawns "
          <> "or mutates a GHCi session."
    , tdInputSchema =
        object
          [ "type"       .= ("object" :: Text)
          , "properties" .= object
              [ "action" .= object
                  [ "type"        .= ("string" :: Text)
                  , "enum"        .= (["status", "help", "next"] :: [Text])
                  , "description" .=
                      ("Which view to return. Default: 'status'." :: Text)
                  ]
              ]
          , "additionalProperties" .= False
          ]
    }

data Action = ActStatus | ActHelp | ActNext
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
      Just "help"    -> pure ActHelp
      Just "next"    -> pure ActNext
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
  -> Value
  -> IO ToolResult
handle pdRef sessMVar toolNames ws staleness isSelfProject rawArgs =
  case parseEither parseJSON rawArgs of
    Left err ->
      pure (Env.toolResponseToResult (Env.mkFailed
        ((Env.mkErrorEnvelope (parseErrorKind err)
            (T.pack ("Invalid arguments: " <> err)))
              { Env.eeCause = Just (T.pack err) })))
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
      pure (render a pd sessAlive toolNames ws staleness missing entryMod isSelfProject)

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
  -> ToolResult
render a pd alive toolNames ws staleness missingOpt mEntry isSelfProject =
  let phase      = classifyPhase ws
      stateHints = renderHelp ws
      payload    = case a of
        ActStatus -> statusPayload pd alive toolNames staleness phase missingOpt isSelfProject
        ActHelp   -> helpPayload pd alive stateHints staleness phase mEntry
        ActNext   -> nextPayload pd alive ws
  in Env.toolResponseToResult (Env.mkOk payload)

statusPayload
  :: ProjectDir -> Bool -> [Text] -> StalenessReport -> SessionPhase
  -> [Text] -> Bool -> Value
statusPayload pd alive toolNames staleness phase missingOpt isSelfProject =
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
    ]
    -- Only emit the 'optionalBinaries' field when something is
    -- missing.  Happy path stays clean — the nudge only appears
    -- when there's something to nudge about.
    <> [ "optionalBinaries" .= optionalBinariesPayload missingOpt
       | not (null missingOpt)
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

-- | F-03: 'nextPayload' is now history-aware. If the most recent
-- tool call was already 'ghc_load', recommending 'ghc_load' again
-- is a static no-op. Look at the sliding history and suggest a
-- more useful next step.
nextPayload :: ProjectDir -> Bool -> WorkflowState -> Value
nextPayload _pd alive ws =
  object
    [ "view"   .= ("next" :: Text)
    , "tool"   .= tool
    , "why"    .= why
    ]
  where
    hist = wsToolHistory ws
    (tool :: Text, why :: Text) =
      case hist of
        -- Just called ghc_load successfully → suggest diagnostics pass.
        (GhcLoad : _) | alive ->
          ( "ghc_load"
          , "Run with diagnostics=true to surface typed holes and \
            \deferred warnings alongside regular errors." )
        -- Just called ghc_type / ghc_info → suggest a property.
        (GhcType : _) | alive ->
          ( "ghc_quickcheck"
          , "You typed an expression — if it has a useful law, check \
            \it now before moving on." )
        -- Just called ghc_suggest → feed result to quickcheck.
        (GhcSuggest : _) ->
          ( "ghc_quickcheck"
          , "You generated property candidates from ghc_suggest — pick \
            \the highest-confidence one and check it." )
        -- Already ran quickcheck → export to Spec.hs.
        (GhcQuickCheck : _) | wsPassedProperties ws >= 3 ->
          ( "ghc_property_store"
          , "You have " <> T.pack (show (wsPassedProperties ws))
            <> " passing properties — run action=\"export\" to materialise \
               \them as test/Spec.hs." )
        _ ->
          if alive
            then ( "ghc_load"
                 , "Re-check with diagnostics=true to surface any new holes \
                   \or warnings introduced by the last edit." )
            else ( "ghc_load"
                 , "No active session. ghc_load boots GHCi and is the \
                   \cheapest way to learn the project's current compile state." )

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
