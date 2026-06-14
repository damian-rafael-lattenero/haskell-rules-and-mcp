-- | @ghc_deps@ — add / remove / list entries in the project's @.cabal@
-- file without the agent having to edit it by hand.
--
-- We deliberately keep the cabal-file parser line-oriented rather than
-- pulling @Cabal@'s full parser as a dependency: it adds ~15 MB of
-- transitive closure, and we only care about one field
-- (@build-depends@) of the @library@ stanza. A focused string parser
-- covers the common comma-leading and comma-trailing shapes that
-- @cabal init@ / @cabal-fmt@ produce.
--
-- Security posture:
--
-- * The target @.cabal@ is located by scanning 'ProjectDir' only; we
--   never accept a path from the agent. Traversal is impossible.
-- * The package name goes through a strict identifier check — Hackage
--   names are @[A-Za-z0-9-]+@ and we refuse anything else. No shell
--   metacharacter can leak into the edit.
-- * The version constraint is validated with a minimal parser that
--   only accepts the operator + literal shape cabal actually uses
--   (@>=@, @<@, @^>=@, @&&@, numeric versions, spaces). Anything else
--   is rejected.
module HaskellFlows.Tool.Deps
  ( descriptor
  , handle
  , DepsArgs (..)
  , Action (..)
  , validatePackageName
  , validateVersionConstraint
  , parseStanzaSelector
  , sliceStanza
  , renderSelector
  , addDep
  , removeDep
    -- * #48 — post-edit verification of dep resolvability
  , verifyResolvable
  , extractErrorSummary
    -- * F-08 — all-stanzas listing (exported for unit tests)
  , allStanzaDeps
    -- * #119 — idempotent-result helper (exported for unit tests)
  , unchangedResult
    -- * #244 — common stanza hint (exported for unit tests)
  , findCommonStanzaWithPkg
  , unchangedResult'
    -- * #260 — cross-stanza dep hint (exported for unit tests)
  , importsMatchingPackage
  ) where

import Control.Concurrent.MVar (MVar, newMVar, withMVar)
import Control.Exception (SomeException, bracket, try)
import Data.Aeson
import Data.Aeson.Types (parseEither)
import Data.Char (isAlphaNum)
import Data.List (nub)
import Data.Maybe (fromMaybe)
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.IO as TIO
import GHC.IO.Handle.Lock (hLock, hUnlock, LockMode (..))
import System.Directory (doesDirectoryExist, listDirectory)
import System.Exit (ExitCode (..))
import System.FilePath (takeDirectory, takeExtension, (</>))
import System.IO (IOMode (..), openFile, hClose)
import System.IO.Unsafe (unsafePerformIO)
import qualified System.Process as Proc

import qualified Data.Aeson.Key as AesonKey
import qualified Data.Aeson.KeyMap as KeyMap

import qualified HaskellFlows.Mcp.Envelope as Env
import qualified HaskellFlows.Mcp.Schema as Schema
import HaskellFlows.Mcp.Protocol
import HaskellFlows.Mcp.ToolName (ToolName (..), toolNameText)
import qualified HaskellFlows.Tool.DepsExplain as DepsExplain
import HaskellFlows.Types (ProjectDir, unProjectDir)
import HaskellFlows.Tool.Env (ToolEnv (..))
import HaskellFlows.Tool.Deps.Validate
  ( validatePackageName
  , validateVersionConstraint
  , parseStanzaSelector
  , renderSelector
  )
import HaskellFlows.Tool.Deps.Cabal
  ( findCabalFile
  , parseBuildDepends
  , allStanzaDeps
  , addDep
  , removeDep
  , sliceStanza
  , applyWithinStanza
  , resolveStanza
  , editAgreesWithVerb
  )

-- | Serialise concurrent .cabal edits across every call originating
-- in THIS process. 'hLock' below serialises across processes; the
-- MVar covers the two-in-process-Server case that 'FlowConcurrentClients'
-- exercises (hLock with two FDs inside one process behaves
-- unpredictably on some POSIX implementations).
--
-- NOINLINE + unsafePerformIO is the canonical top-level-MVar pattern.
-- It's a per-program singleton: the MVar has one state across the
-- whole server lifetime, which is exactly what we want.
{-# NOINLINE inProcessCabalLock #-}
inProcessCabalLock :: MVar ()
inProcessCabalLock = unsafePerformIO (newMVar ())

-- | Exclusive read-modify-write guard around any .cabal mutation.
-- Holds the in-process MVar AND an exclusive flock on a dedicated
-- @.lock@ sidecar file.
--
-- We do NOT flock the .cabal itself: on some POSIX configurations
-- holding an exclusive flock on a file blocks subsequent 'writeFile'
-- attempts on the same path with "resource busy (file is locked)",
-- which defeats the purpose. The sidecar lockfile is independent
-- of the read/write path so both ends can proceed freely while
-- the lock does its job.
--
-- Found-by: 'Scenarios.FlowConcurrentClients' in the e2e suite
-- (two McpClients firing ghc_deps(add) concurrently dropped one
-- of the writes with "resource busy (file is locked)" because the
-- naive read + writeFile had no serialisation).
withCabalLock :: FilePath -> IO a -> IO a
withCabalLock cabalPath action =
  withMVar inProcessCabalLock $ \_ -> do
    let lockPath = cabalPath <> ".lock"
    bracket
      (do h <- openFile lockPath AppendMode  -- creates if missing
          hLock h ExclusiveLock
          pure h)
      (\h -> hUnlock h >> hClose h)
      (const action)

descriptor :: ToolDescriptor
descriptor =
  ToolDescriptor
    { tdName        = toolNameText GhcDeps
    , tdDescription =
        "PURPOSE: Manage build-depends in the project's .cabal "
          <> "(list / add / remove / explain). "
          <> "WHEN: action='add'/'remove' a dependency; action='list' the "
          <> "current set; action='explain' which stanzas + source imports "
          <> "use package=X. "
          <> "WHEN NOT: ghc_modules for exposed-modules / other-modules; "
          <> "ghc_add_import to add a source import line. "
          <> "PREREQUISITES: a .cabal in the active project; never hand-edit "
          <> "build-depends — the post-edit invariant guards this tool. "
          <> "OUTPUT: per-action {action, stanzas|added|removed|explanation}; "
          <> "the next ghc_load picks up the new package graph. "
          <> "SEE ALSO: ghc_modules, ghc_add_import."
    , tdInputSchema = schema
    }

-- | Issue #92 Phase B: per-action discriminated schema. The flat
-- 'required: [action]' shape pre-#92 lied about 'add' / 'remove'
-- (which both need 'package'); a host that respected it sent
-- 'add' without a package and the runtime emitted the
-- now-removed "'package' is required for add" message. Each
-- branch now declares its own required-field set.
schema :: Value
schema = Schema.discriminatedSchema "action"
  [ Schema.SchemaBranch
      { Schema.sbDiscriminantValue = "list"
      , Schema.sbDescription       =
          "List the current build-depends of the targeted stanza."
      , Schema.sbProperties        = [ ("stanza", stanzaField) ]
      , Schema.sbRequired          = []
      }
  , Schema.SchemaBranch
      { Schema.sbDiscriminantValue = "add"
      , Schema.sbDescription       =
          "Insert a package + optional version constraint into the \
          \build-depends of the targeted stanza."
      , Schema.sbProperties        =
          [ ("package", Schema.stringField "Hackage package name.")
          , ("version", Schema.stringField
              "Optional cabal version constraint (e.g. '>= 2.14', '^>= 1.4').")
          , ("stanza",  stanzaField)
          ]
      , Schema.sbRequired          = ["package"]
      }
  , Schema.SchemaBranch
      { Schema.sbDiscriminantValue = "remove"
      , Schema.sbDescription       =
          "Remove a package from the build-depends of the targeted stanza."
      , Schema.sbProperties        =
          [ ("package", Schema.stringField "Package name to remove.")
          , ("stanza",  stanzaField)
          ]
      , Schema.sbRequired          = ["package"]
      }
  , Schema.SchemaBranch
      { Schema.sbDiscriminantValue = "explain"
      , Schema.sbDescription       =
          "Two modes — pass exactly one of: (1) 'package' to show \
          \which stanzas declare that dependency and which source files \
          \import its modules; (2) 'cabal_output' (raw output from \
          \'cabal v2-build --dry-run' or a solver conflict dump) to \
          \extract the root-cause rejection and affected packages."
      , Schema.sbProperties        =
          [ ("package", Schema.stringField
              "Package name to explain (e.g. 'aeson', 'QuickCheck'). \
              \Searches build-depends across all stanzas and scans \
              \source files for matching imports. \
              \Mutually exclusive with cabal_output.")
          , ("cabal_output", Schema.stringField
              "Raw cabal solver / dry-run output. The tool extracts \
              \the root-cause conflict and affected packages. \
              \Mutually exclusive with package.")
          ]
      , Schema.sbRequired          = []
      }
  ]
  where
    stanzaField = Schema.typedField "string"
      "Optional stanza selector. \"library\" (main library), \
      \\"test-suite\" (first occurrence), \"test-suite:NAME\", \
      \\"executable\" / \"executable:NAME\", \"benchmark\" / \
      \\"benchmark:NAME\", \"foreign-library\" / \
      \\"foreign-library:NAME\". Omit to target the first build-depends \
      \block in the file (backwards-compatible default)."

data Action = ActList | ActAdd | ActRemove | ActExplain
  deriving stock (Eq, Show)

data DepsArgs = DepsArgs
  { daAction  :: !Action
  , daPackage :: !(Maybe Text)
  , daVersion :: !(Maybe Text)
  , daStanza  :: !(Maybe Text)
  }
  deriving stock (Show)

-- | Issue #92 Phase B: per-action validation at parse time. The
-- flat record stays for handler convenience, but the parser now
-- enforces the same required-field set the schema advertises —
-- 'add' and 'remove' both require 'package' at parse time so the
-- contract drift documented in #92 (schema lying about 'add'
-- requiring only 'action') goes away.
instance FromJSON DepsArgs where
  parseJSON = withObject "DepsArgs" $ \o -> do
    a <- o .:  "action"
    s <- o .:? "stanza"
    act <- case (a :: Text) of
      "list"    -> pure ActList
      "add"     -> pure ActAdd
      "remove"  -> pure ActRemove
      "explain" -> pure ActExplain
      other     -> fail ("unknown action: " <> T.unpack other)
    case act of
      ActList -> pure DepsArgs
        { daAction  = act
        , daPackage = Nothing
        , daVersion = Nothing
        , daStanza  = s
        }
      ActAdd -> do
        p <- o .:  "package"
        v <- o .:? "version"
        pure DepsArgs
          { daAction  = act
          , daPackage = Just p
          , daVersion = v
          , daStanza  = s
          }
      ActRemove -> do
        p <- o .:  "package"
        pure DepsArgs
          { daAction  = act
          , daPackage = Just p
          , daVersion = Nothing
          , daStanza  = s
          }
      ActExplain -> pure DepsArgs
        { daAction  = ActExplain
        , daPackage = Nothing
        , daVersion = Nothing
        , daStanza  = Nothing
        }

handle :: ToolEnv -> Value -> IO ToolResult
handle env rawArgs = do
  pd <- teProjectDir env
  r  <- runHandle pd rawArgs
  teInvalidateStanza env
  pure r

runHandle :: ProjectDir -> Value -> IO ToolResult
runHandle pd rawArgs = case parseEither parseJSON rawArgs of
  Left parseError ->
    pure (Env.toolResponseToResult (Env.mkFailed
      ((Env.mkErrorEnvelope (parseErrorKindD parseError)
          (T.pack ("Invalid arguments: " <> parseError)))
            { Env.eeCause = Just (T.pack parseError) })))
  Right args -> case daAction args of
    ActExplain -> DepsExplain.handle pd rawArgs
    _ -> do
      mCabal <- findCabalFile pd
      case mCabal of
        Nothing ->
          pure (Env.toolResponseToResult (Env.mkFailed
            ((Env.mkErrorEnvelope Env.ModulePathDoesNotExist
                "No .cabal file found in project root")
                  { Env.eeRemediation =
                      Just "Run ghc_project(action=\"create\") to scaffold a cabal package first." })))
        Just file -> handleAction file args

handleAction :: FilePath -> DepsArgs -> IO ToolResult
handleAction file args = case daAction args of
  ActList -> do
    res <- try (TIO.readFile file) :: IO (Either SomeException Text)
    case res of
      Left e -> pure (errorResult (T.pack ("Could not read cabal file: " <> show e)))
      Right body -> case daStanza args of
        -- F-08: no stanza selector → return all stanzas structured.
        Nothing    -> pure (listResultAll file (allStanzaDeps body))
        Just _     -> case resolveStanza (daStanza args) body of
          Left err    -> pure (errorResult err)
          Right scope -> pure (listResult file (parseBuildDepends scope))
  ActAdd -> case daPackage args of
    Nothing  -> pure (errorResult "'package' is required for add")
    Just pkg -> case validatePackageName pkg of
      Left err -> pure (errorResult err)
      Right safePkg -> case traverse validateVersionConstraint (daVersion args) of
        Left err -> pure (errorResult err)
        Right safeVer -> case traverse parseStanzaSelector (daStanza args) of
          Left err   -> pure (errorResult err)
          -- 'verifyAfter=True' on add: spawn `cabal v2-build --dry-run`
          -- after the edit to confirm the new dep set is solvable;
          -- rollback the .cabal if cabal refuses (#48).
          Right mSel -> do
            -- #260: warn if a sibling stanza's sources already import a
            -- module this package owns, so the agent adds it there too
            -- before a later gate fails on the missing dep.
            mHint <- crossStanzaHint file safePkg mSel
            runEdit mHint file safePkg mSel (addDep safeVer) "added" True
  ActRemove -> case daPackage args of
    Nothing  -> pure (errorResult "'package' is required for remove")
    Just pkg -> case validatePackageName pkg of
      Left err       -> pure (errorResult err)
      Right safePkg  -> case traverse parseStanzaSelector (daStanza args) of
        Left err   -> pure (errorResult err)
        -- Remove can never make the dep set unsolvable from the
        -- agent's perspective — if cabal fails after a remove, it's
        -- because some other module still imports the dropped
        -- package (a downstream issue surfaced by ghc_load, not by
        -- the resolver). Skip verify on remove to keep the call
        -- cheap.
        Right mSel -> runEdit Nothing file safePkg mSel removeDep "removed" False
  ActExplain ->
    pure (errorResult "internal: ActExplain must be dispatched in runHandle")


runEdit
  :: Maybe Value                             -- #260: cross-stanza hint (add only)
  -> FilePath
  -> Text                                    -- validated package name
  -> Maybe (Text, Maybe Text)                -- parsed stanza selector, if any
  -> (Text -> Text -> Text)                  -- (pkg -> body -> newBody)
  -> Text                                    -- verb for the success message
  -> Bool                                    -- verifyAfter: run cabal dry-run + rollback
  -> IO ToolResult
runEdit mHint file pkg mStanza f verb verifyAfter = withCabalLock file $ do
  res <- try (TIO.readFile file) :: IO (Either SomeException Text)
  case res of
    Left e -> pure (errorResult (T.pack ("Could not read cabal file: " <> show e)))
    Right body -> case applyWithinStanza mStanza (f pkg) body of
      Left err -> pure (errorResult err)
      Right newBody
        | newBody == body -> do
            -- Idempotent no-op: the edit is already reflected in the
            -- .cabal. Verb-specific message so the agent doesn't have
            -- to re-parse a remove-shaped error on an add path. Still
            -- a 'success=true' payload — the post-condition the caller
            -- asked for ("pkg is [not] listed in stanza") holds.
            --
            -- Issue #244: on 'remove', if the package was not found in
            -- the targeted stanza but IS present in a common stanza,
            -- add a hint so the agent knows where to look.
            commonHint <- case verb of
              "removed" -> case mStanza of
                Nothing  -> pure Nothing
                Just _   ->
                  -- Check if the package lives in a common stanza.
                  case findCommonStanzaWithPkg pkg body of
                    Nothing -> pure Nothing
                    Just cn ->
                      pure (Just ("'" <> pkg <> "' is not in the targeted stanza's "
                                  <> "own build-depends but appears in common stanza '"
                                  <> cn <> "'. To remove it, re-run with stanza=\"common:"
                                  <> cn <> "\"."))
              _ -> pure Nothing
            pure (unchangedResult' file pkg verb commonHint)
        | not (editAgreesWithVerb verb pkg mStanza newBody) ->
            -- Post-edit structural check: if the requested verb says
            -- \"added\" but the re-parsed body doesn't list the package
            -- in the targeted scope (or \"removed\" but it still is),
            -- the edit got confused — refuse to persist. Prevents
            -- regressing to the F-01 class of bugs where success=true
            -- was reported but the .cabal file ended up in a broken
            -- state that cabal could not parse.
            pure (errorResult ("Refusing to write: post-edit parse check \
                              \disagreed with the requested operation \
                              \for '" <> pkg <> "'. No changes written."))
        | otherwise -> do
            wres <- try (TIO.writeFile file newBody) :: IO (Either SomeException ())
            case wres of
              Left e  -> pure (errorResult (T.pack ("Could not write cabal file: " <> show e)))
              Right _
                | verifyAfter -> verifyAndCommit mHint file body pkg verb
                | otherwise   -> pure (editResult mHint file pkg verb)

-- | Post-write verification step (#48): run @cabal v2-build all
-- --dry-run --only-dependencies@ in the project root. If cabal
-- accepts the new dep set, return the success payload. If cabal
-- rejects it (e.g. the package doesn't exist on Hackage, version
-- bounds unsolvable), restore the @.cabal@ to its pre-edit body
-- and return a structured 'error_kind: \"unresolvable_dep\"'.
--
-- Held inside 'withCabalLock' (caller's responsibility) so the
-- subprocess sees the version we just wrote, and so a rollback
-- can't race with a concurrent add.
verifyAndCommit :: Maybe Value -> FilePath -> Text -> Text -> Text -> IO ToolResult
verifyAndCommit mHint file originalBody pkg verb = do
  verified <- verifyResolvable file pkg
  case verified of
    Right () -> pure (editResult mHint file pkg verb)
    Left err -> do
      -- Roll back the .cabal to its pre-edit state.
      rbres <- try (TIO.writeFile file originalBody)
                :: IO (Either SomeException ())
      case rbres of
        Right _ -> pure (verifyFailedResult file pkg err)
        Left rbErr ->
          -- Catastrophic: verify failed AND rollback failed. The
          -- .cabal is now in the post-edit state but cabal won't
          -- accept it. Surface BOTH errors so the agent can decide
          -- whether to manually restore from VCS.
          pure (errorResult
            ( "FATAL: cabal could not solve the dep set after adding '"
              <> pkg <> "', AND the rollback write failed. The .cabal "
              <> "is in an inconsistent state. Cabal error: " <> err
              <> " | Rollback error: " <> T.pack (show rbErr) ))

-- | Spawn @cabal v2-build all --dry-run --only-dependencies@ in
-- the project root. Argv-form, no shell. Returns 'Right ()' on
-- exit 0; 'Left summary' otherwise (exec failure or non-zero
-- exit).
--
-- We pick @v2-build all --dry-run --only-dependencies@ rather
-- than @v2-repl@ because:
--
--   * @--dry-run@ runs the solver without compiling, ~1–3 s.
--   * @--only-dependencies@ stops cabal from chasing the home
--     package's source tree (which may legitimately have errors
--     unrelated to the dep change we just made).
--   * @v2-build all@ exercises every stanza's deps (lib, test,
--     bench), catching cross-stanza solver conflicts the agent
--     wouldn't see otherwise.
verifyResolvable :: FilePath -> Text -> IO (Either Text ())
verifyResolvable cabalPath pkg = do
  let root = takeDirectory cabalPath
      cp = (Proc.proc "cabal"
              [ "v2-build", "all"
              , "--dry-run"
              , "--only-dependencies"
              ])
              { Proc.cwd      = Just root
              , Proc.std_in   = Proc.NoStream
              , Proc.std_out  = Proc.CreatePipe
              , Proc.std_err  = Proc.CreatePipe
              }
  result <- try (Proc.readCreateProcessWithExitCode cp "")
              :: IO (Either SomeException (ExitCode, String, String))
  pure $ case result of
    Left e ->
      Left ("could not invoke cabal for verification: " <> T.pack (show e))
    Right (ExitSuccess, _, _) ->
      Right ()
    Right (ExitFailure _, stdout, stderr) ->
      Left (extractErrorSummary pkg (T.pack (stderr <> "\n" <> stdout)))

-- | Pull the lines from cabal's failure output that mention the
-- package or look like a solver verdict. Falls back to a truncated
-- raw output if no relevant line is found. Pure (no IO) so it's
-- unit-testable in isolation.
extractErrorSummary :: Text -> Text -> Text
extractErrorSummary pkg combinedOutput =
  let lns      = T.lines combinedOutput
      pkgLower = T.toLower pkg
      relevant = filter
        (\l ->
          let lower = T.toLower l
          in pkgLower                  `T.isInfixOf` lower
             || "could not resolve"    `T.isInfixOf` lower
             || "unknown package"      `T.isInfixOf` lower
             || "rejecting"            `T.isInfixOf` lower
             || "no solution"          `T.isInfixOf` lower
             || "backjump"             `T.isInfixOf` lower
        )
        lns
      summary = case relevant of
        [] -> T.take 800 combinedOutput
        xs -> T.unlines (take 8 xs)
  in T.strip summary


--------------------------------------------------------------------------------
-- response shaping
--------------------------------------------------------------------------------


-- | F-08: list response that enumerates all stanzas. Shape is
-- @{ stanzas: { library: [...], "test-suite:NAME": [...] } }@.
listResultAll :: FilePath -> [(Text, [Text])] -> ToolResult
listResultAll file stanzas =
  let km = KeyMap.fromList
             [ (AesonKey.fromText k, toJSON deps)
             | (k, deps) <- stanzas
             ]
      payload = object
        [ "action"     .= ("list" :: Text)
        , "cabal_file" .= T.pack file
        , "stanzas"    .= Object km
        ]
  in Env.toolResponseToResult (Env.mkOk payload)

listResult :: FilePath -> [Text] -> ToolResult
listResult file deps =
  let payload = object
        [ "action"       .= ("list" :: Text)
        , "cabal_file"   .= T.pack file
        , "count"        .= length deps
        , "build_depends".= deps
        ]
  in Env.toolResponseToResult (Env.mkOk payload)

editResult :: Maybe Value -> FilePath -> Text -> Text -> ToolResult
editResult mHint file pkg verb =
  let payload = object $
        [ "action"     .= verb
        , "cabal_file" .= T.pack file
        , "package"    .= pkg
        , "hint"       .= ( "Dependency set changed. The next \
                            \ghc_load reloads GHCi with the new \
                            \package graph — no explicit session \
                            \restart tool is needed."
                          :: Text )
        ]
        <> maybe [] (\h -> [ "cross_stanza_hint" .= h ]) mHint
  in Env.toolResponseToResult (Env.mkOk payload)

-- | #48 + #90: cabal-rejected dep maps to status='failed' with
-- kind='unresolvable_dep'. The legacy 'rolled_back' flag stays
-- inside 'result' for back-compat.
verifyFailedResult :: FilePath -> Text -> Text -> ToolResult
verifyFailedResult file pkg err =
  let envErr = (Env.mkErrorEnvelope Env.UnresolvableDep
                 ("cabal could not solve the dep set after adding '"
                  <> pkg <> "'"))
                 { Env.eeCause = Just err
                 , Env.eeField = Just "package"
                 }
      payload = object
        [ "action"      .= ("rejected" :: Text)
        , "cabal_file"  .= T.pack file
        , "package"     .= pkg
        , "rolled_back" .= True
        ]
      response = (Env.mkFailed envErr) { Env.reResult = Just payload }
  in Env.toolResponseToResult response

unchangedResult :: FilePath -> Text -> Text -> ToolResult
unchangedResult file pkg verb = unchangedResult' file pkg verb Nothing

-- | Issue #244: extended version of 'unchangedResult' that accepts an
-- optional hint string surfaced when a package is absent from the
-- targeted stanza but found in a common stanza.
unchangedResult' :: FilePath -> Text -> Text -> Maybe Text -> ToolResult
unchangedResult' file pkg verb mHint =
  let note = case verb of
        "added"   -> "'" <> pkg <> "' already present in target stanza — no change written."
        "removed" -> "'" <> pkg <> "' not listed in target stanza — no change written."
        _         -> "no change written"
      -- #119: remove 'verb' — it contradicts 'action: "unchanged"' when
      -- the verb is "added" (implying something was added when nothing
      -- was written). The 'note' field already explains the outcome.
      payload = object $
        [ "action"     .= ("unchanged" :: Text)
        , "cabal_file" .= T.pack file
        , "package"    .= pkg
        , "note"       .= (note :: Text)
        ]
        <> case mHint of
             Nothing -> []
             Just h  -> [ "hint" .= h ]
  in Env.toolResponseToResult (Env.mkOk payload)

-- | Issue #244: scan the whole cabal body for @common@ stanzas and
-- return the first common stanza name whose build-depends contains
-- @pkg@. Used to produce an actionable hint when a remove is a no-op
-- because the package is inherited rather than direct.
--------------------------------------------------------------------------------
-- #260: cross-stanza dependency hint. After adding a package to one
-- stanza, warn when a sibling stanza's sources import a module the
-- package owns (curated map + conventional-layout scan).
--------------------------------------------------------------------------------

-- | Curated package -> module-prefix map: the common packages whose
-- modules a sibling stanza frequently also imports.
packageModulePrefixes :: [(Text, [Text])]
packageModulePrefixes =
  [ ("containers",           ["Data.Map", "Data.Set", "Data.IntMap", "Data.IntSet", "Data.Sequence", "Data.Tree", "Data.Graph"])
  , ("unordered-containers", ["Data.HashMap", "Data.HashSet"])
  , ("text",                 ["Data.Text"])
  , ("bytestring",           ["Data.ByteString"])
  , ("aeson",                ["Data.Aeson"])
  , ("vector",               ["Data.Vector"])
  , ("mtl",                  ["Control.Monad.State", "Control.Monad.Reader", "Control.Monad.Writer", "Control.Monad.Except", "Control.Monad.RWS"])
  , ("QuickCheck",           ["Test.QuickCheck"])
  , ("hspec",                ["Test.Hspec"])
  , ("time",                 ["Data.Time"])
  ]

-- | Import lines in a source body that pull a module owned by @pkg@
-- (per the curated map). Empty when @pkg@ isn't mapped. Pure + tested.
importsMatchingPackage :: Text -> Text -> [Text]
importsMatchingPackage pkg body =
  case lookup pkg packageModulePrefixes of
    Nothing       -> []
    Just prefixes ->
      [ T.strip ln
      | ln <- T.lines body
      , "import " `T.isPrefixOf` T.stripStart ln
      , let modTok = importedModule ln
      , any (\p -> p == modTok || (p <> ".") `T.isPrefixOf` modTok) prefixes
      ]
  where
    importedModule ln =
      let afterImp  = T.stripStart (T.drop 6 (T.stripStart ln))   -- drop "import"
          afterQual = fromMaybe afterImp (T.stripPrefix "qualified " afterImp)
      in T.takeWhile (\c -> isAlphaNum c || c == '.' || c == '_') (T.stripStart afterQual)

-- | Conventional stanza -> source dir mapping (custom hs-source-dirs are
-- not parsed; this is a best-effort layout heuristic).
conventionalStanzaDirs :: [(Text, FilePath)]
conventionalStanzaDirs =
  [ ("library", "src"), ("test-suite", "test")
  , ("executable", "app"), ("benchmark", "benchmarks") ]

-- | Recursively list @.hs@ files under a directory (empty if absent).
enumerateHsFiles :: FilePath -> IO [FilePath]
enumerateHsFiles dir = do
  exists <- doesDirectoryExist dir
  if not exists
    then pure []
    else do
      entries <- listDirectory dir
      concat <$> mapM (\e -> do
        let p = dir </> e
        isDir <- doesDirectoryExist p
        if isDir
          then enumerateHsFiles p
          else pure [ p | takeExtension p == ".hs" ]) entries

-- | #260: build the cross-stanza hint Value, or Nothing when the added
-- package isn't curated or no sibling stanza imports a module it owns.
crossStanzaHint :: FilePath -> Text -> Maybe (Text, Maybe Text) -> IO (Maybe Value)
crossStanzaHint cabalFile pkg mSel
  | pkg `notElem` map fst packageModulePrefixes = pure Nothing
  | otherwise = do
      let added  = maybe "library" fst mSel
          root   = takeDirectory cabalFile
          others = [ sd | sd@(s, _) <- conventionalStanzaDirs, s /= added ]
      evidence <- concat <$> mapM (scanStanza root) others
      pure $ case evidence of
        [] -> Nothing
        _  ->
          let stanzas = nub [ s | (s, _, _) <- evidence ]
          in Just $ object
               [ "package"        .= pkg
               , "also_needed_in" .= stanzas
               , "evidence"       .= [ object [ "file" .= f, "import" .= imp ]
                                     | (_, f, imp) <- take 8 evidence ]
               , "suggested_call" .= object
                   [ "action"  .= ("add" :: Text)
                   , "package" .= pkg
                   , "stanza"  .= headOr "test-suite" stanzas ]
               ]
  where
    headOr d xs = case xs of { (x : _) -> x; [] -> d }
    scanStanza root (stanza, d) = do
      files <- enumerateHsFiles (root </> d)
      concat <$> mapM (\f -> do
        eBody <- try (TIO.readFile f) :: IO (Either SomeException Text)
        pure $ case eBody of
          Left _     -> []
          Right body -> [ (stanza, T.pack f, imp)
                        | imp <- importsMatchingPackage pkg body ]) files

findCommonStanzaWithPkg :: Text -> Text -> Maybe Text
findCommonStanzaWithPkg pkg body =
  let lns = T.lines body
      -- Collect all (stanzaName, stanzaLines) pairs for common stanzas.
      commonStanzas = go lns
  in case filter (pkgInStanza pkg) commonStanzas of
       ((name, _) : _) -> Just name
       []               -> Nothing
  where
    pkgInStanza p (_, stanzaLns) = p `elem` parseBuildDepends (T.unlines stanzaLns)

    go [] = []
    go (ln : rest) =
      case T.strip ln of
        t | "common " `T.isPrefixOf` t || "common\t" `T.isPrefixOf` t ->
              let name = T.strip (T.drop 6 t)   -- drop "common"
                  (body_, after) = break isTopLevelStanzaHeader rest
              in (name, body_) : go after
          | otherwise -> go rest

-- | Issue #90 Phase C: route the legacy 'errorResult' through the
-- envelope. Most call sites pass a free-form 'Text' that maps to
-- kind='validation' (the input was structurally fine but failed a
-- domain check).
errorResult :: Text -> ToolResult
errorResult msg =
  Env.toolResponseToResult (Env.mkFailed
    (Env.mkErrorEnvelope Env.Validation msg))

parseErrorKindD :: String -> Env.ErrorKind
parseErrorKindD err
  | "key" `isInfixOfStrD` err = Env.MissingArg
  | otherwise                 = Env.TypeMismatch
  where
    isInfixOfStrD needle haystack =
      let n = length needle
      in any (\i -> take n (drop i haystack) == needle)
             [0 .. length haystack - n]
