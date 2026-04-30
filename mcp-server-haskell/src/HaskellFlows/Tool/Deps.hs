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
  ) where

import Control.Concurrent.MVar (MVar, newMVar, withMVar)
import Control.Exception (SomeException, bracket, try)
import Data.Aeson
import Data.Aeson.Types (parseEither)
import Data.Char (isAlphaNum, isDigit, isSpace)
import Data.List (sortOn)
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

import qualified HaskellFlows.Mcp.Envelope as Env
import qualified HaskellFlows.Mcp.Schema as Schema
import HaskellFlows.Mcp.Protocol
import HaskellFlows.Mcp.ToolName (ToolName (..), toolNameText)
import HaskellFlows.Types (ProjectDir, unProjectDir)

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
        "Manage build-depends in the project's .cabal file. Actions: "
          <> "'list' (current deps), 'add' (insert pkg + optional "
          <> "version constraint), 'remove' (delete by name). After "
          <> "add/remove, the next ghc_load picks up the new "
          <> "package graph — no explicit session restart needed."
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
  ]
  where
    stanzaField = Schema.typedField "string"
      "Optional stanza selector. \"library\" (main library), \
      \\"test-suite\" (first occurrence), \"test-suite:NAME\", \
      \\"executable\" / \"executable:NAME\", \"benchmark\" / \
      \\"benchmark:NAME\", \"foreign-library\" / \
      \\"foreign-library:NAME\". Omit to target the first build-depends \
      \block in the file (backwards-compatible default)."

data Action = ActList | ActAdd | ActRemove
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
      "list"   -> pure ActList
      "add"    -> pure ActAdd
      "remove" -> pure ActRemove
      other    -> fail ("unknown action: " <> T.unpack other)
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

handle :: ProjectDir -> Value -> IO ToolResult
handle pd rawArgs = case parseEither parseJSON rawArgs of
  Left parseError ->
    pure (Env.toolResponseToResult (Env.mkFailed
      ((Env.mkErrorEnvelope (parseErrorKindD parseError)
          (T.pack ("Invalid arguments: " <> parseError)))
            { Env.eeCause = Just (T.pack parseError) })))
  Right args -> do
    mCabal <- findCabalFile pd
    case mCabal of
      Nothing ->
        pure (Env.toolResponseToResult (Env.mkFailed
          ((Env.mkErrorEnvelope Env.ModulePathDoesNotExist
              "No .cabal file found in project root")
                { Env.eeRemediation =
                    Just "Run ghc_create_project to scaffold a cabal package first." })))
      Just file -> handleAction file args

handleAction :: FilePath -> DepsArgs -> IO ToolResult
handleAction file args = case daAction args of
  ActList -> do
    res <- try (TIO.readFile file) :: IO (Either SomeException Text)
    case res of
      Left e     -> pure (errorResult (T.pack ("Could not read cabal file: " <> show e)))
      Right body -> case resolveStanza (daStanza args) body of
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
          Right mSel -> runEdit file safePkg mSel (addDep safeVer) "added" True
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
        Right mSel -> runEdit file safePkg mSel removeDep "removed" False

-- | Resolve a stanza selector at @list@ time by scoping the body to
-- that stanza's lines. Errors (unknown selector / stanza not found)
-- surface as structured error results to the agent.
resolveStanza :: Maybe Text -> Text -> Either Text Text
resolveStanza Nothing body    = Right body
resolveStanza (Just raw) body = do
  sel <- parseStanzaSelector raw
  case sliceStanza sel (T.lines body) of
    Nothing -> Left ("stanza not found: " <> raw)
    Just (_, stanzaLns, _) -> Right (T.unlines stanzaLns)

runEdit
  :: FilePath
  -> Text                                    -- validated package name
  -> Maybe (Text, Maybe Text)                -- parsed stanza selector, if any
  -> (Text -> Text -> Text)                  -- (pkg -> body -> newBody)
  -> Text                                    -- verb for the success message
  -> Bool                                    -- verifyAfter: run cabal dry-run + rollback
  -> IO ToolResult
runEdit file pkg mStanza f verb verifyAfter = withCabalLock file $ do
  res <- try (TIO.readFile file) :: IO (Either SomeException Text)
  case res of
    Left e -> pure (errorResult (T.pack ("Could not read cabal file: " <> show e)))
    Right body -> case applyWithinStanza mStanza (f pkg) body of
      Left err -> pure (errorResult err)
      Right newBody
        | newBody == body ->
            -- Idempotent no-op: the edit is already reflected in the
            -- .cabal. Verb-specific message so the agent doesn't have
            -- to re-parse a remove-shaped error on an add path. Still
            -- a 'success=true' payload — the post-condition the caller
            -- asked for ("pkg is [not] listed in stanza") holds.
            pure (unchangedResult file pkg verb)
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
                | verifyAfter -> verifyAndCommit file body pkg verb
                | otherwise   -> pure (editResult file pkg verb)

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
verifyAndCommit :: FilePath -> Text -> Text -> Text -> IO ToolResult
verifyAndCommit file originalBody pkg verb = do
  verified <- verifyResolvable file pkg
  case verified of
    Right () -> pure (editResult file pkg verb)
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

-- | Structural self-check run on the in-memory newBody before it hits
-- disk. Uses the same line-oriented parser the tool ships with — if
-- its own parser can't agree with the verb, the edit is refused.
editAgreesWithVerb
  :: Text                           -- verb (\"added\" / \"removed\")
  -> Text                           -- package name
  -> Maybe (Text, Maybe Text)       -- stanza selector
  -> Text                           -- newBody
  -> Bool
editAgreesWithVerb verb pkg mStanza newBody =
  let scope = case mStanza of
        Nothing  -> newBody
        Just sel -> case sliceStanza sel (T.lines newBody) of
          Nothing            -> newBody   -- can't slice ⇒ fall back to \"no regression on unknown\"
          Just (_, lns, _)   -> T.unlines lns
      present = pkg `elem` parseBuildDepends scope
  in case verb of
       "added"   -> present
       "removed" -> not present
       _         -> True

--------------------------------------------------------------------------------
-- cabal file discovery
--------------------------------------------------------------------------------

-- | Find the single @.cabal@ file in the project root. Returns
-- 'Nothing' if zero or multiple are present (the latter is unusual and
-- the agent should resolve it by hand).
findCabalFile :: ProjectDir -> IO (Maybe FilePath)
findCabalFile pd = do
  let root = unProjectDir pd
  exists <- doesDirectoryExist root
  if not exists
    then pure Nothing
    else do
      entries <- listDirectory root
      let cabalFiles = [ root </> e | e <- entries, takeExtension e == ".cabal" ]
      case cabalFiles of
        [one] -> pure (Just one)
        _     -> pure Nothing

--------------------------------------------------------------------------------
-- boundary validation
--------------------------------------------------------------------------------

-- | A Hackage package name is @[A-Za-z][A-Za-z0-9-]*@. Any other
-- character (including whitespace and meta-characters) is rejected.
validatePackageName :: Text -> Either Text Text
validatePackageName raw
  | T.null raw                = Left "package name is empty"
  | not (T.all okChar raw)    = Left ("invalid character in package name: " <> raw)
  | not (firstIsLetter raw)   = Left "package name must start with a letter"
  | otherwise                 = Right raw
  where
    okChar c = isAlphaNum c || c == '-'
    firstIsLetter t = case T.uncons t of
      Just (c, _) -> isAlphaNum c && not (isDigit c)
      Nothing     -> False

-- | Cabal version constraints are a tiny language: @>=@, @<=@, @<@,
-- @>@, @==@, @^>=@, conjunctions via @&&@, numeric @X.Y.Z@ versions,
-- and whitespace. We accept only those characters — no shell
-- metacharacters, no identifiers.
validateVersionConstraint :: Text -> Either Text Text
validateVersionConstraint raw
  | T.null stripped          = Left "version constraint is empty"
  | T.any (not . okChar) raw = Left ("invalid character in version constraint: " <> raw)
  | otherwise                = Right stripped
  where
    stripped = T.strip raw
    okChar c = isDigit c
            || isSpace c
            || c `elem` (".<>=^&" :: String)

--------------------------------------------------------------------------------
-- cabal file edits
--------------------------------------------------------------------------------

-- | Extract the names currently present in the library's
-- @build-depends@. Robust to:
--
-- * @build-depends:    base, text, aeson@              (comma-trailing)
-- * @build-depends:    base@, newline, @, text@         (comma-leading)
-- * @build-depends:@, newline, indented continuation
parseBuildDepends :: Text -> [Text]
parseBuildDepends body =
  let raw  = extractBuildDependsBody body
      toks = map T.strip (splitOnCommas raw)
  in sortOn T.toLower
       [ packageHead t
       | t <- toks
       , not (T.null t)
       ]
  where
    packageHead t =
      let (name, _) = T.break (\c -> isSpace c || c == '<' || c == '>' || c == '=' || c == '^' || c == '&')
                               (T.strip t)
      in name

-- | Pull everything that belongs to the first @build-depends:@ block
-- in the @library@ stanza. Stops at the next top-level keyword or at
-- end-of-file. Conservative: if we don't recognise the structure, we
-- return an empty string (the caller reports no deps).
extractBuildDependsBody :: Text -> Text
extractBuildDependsBody body =
  let lns = T.lines body
      -- Find the line that starts a build-depends block.
      rest = dropWhile (not . isBuildDependsHeader) lns
  in case rest of
       []     -> ""
       (h:ls) ->
         let headTail = T.drop (T.length "build-depends:") (T.strip h)
             cont     = takeWhile isContinuation ls
             all_     = headTail : map T.strip cont
         in T.unwords all_

isBuildDependsHeader :: Text -> Bool
isBuildDependsHeader ln =
  let s = T.toLower (T.stripStart ln)
  in "build-depends:" `T.isPrefixOf` s

-- | A continuation of @build-depends:@ is any line that isn't a new
-- top-level stanza header or a new field name at column 0.
isContinuation :: Text -> Bool
isContinuation ln
  | T.null stripped              = False
  | isBuildDependsHeader ln      = False
  | looksLikeFieldHeader stripped = False
  | not (T.null (T.takeWhile isSpace ln)) = True   -- indented continuation
  | otherwise                    = T.take 1 stripped == "," -- leading comma
  where
    stripped = T.strip ln

looksLikeFieldHeader :: Text -> Bool
looksLikeFieldHeader t =
  case T.breakOn ":" t of
    (name, rest)
      | T.null rest      -> False
      | T.any isSpace name -> False
      | otherwise        -> True

-- | Comma-split at the top level, ignoring commas inside parentheses.
splitOnCommas :: Text -> [Text]
splitOnCommas = go 0 []
  where
    go :: Int -> String -> Text -> [Text]
    go _ acc t | T.null t    = [T.pack (reverse acc)]
    go d acc t =
      case T.uncons t of
        Just ('(', r) -> go (d + 1)       ('(':acc) r
        Just (')', r) -> go (max 0 (d-1)) (')':acc) r
        Just (',', r)
          | d == 0    -> T.pack (reverse acc) : go 0 [] r
        Just (c,   r) -> go d (c:acc) r
        Nothing       -> [T.pack (reverse acc)]

-- | Insert a new @, pkg [version]@ entry after the last dep of the
-- library's @build-depends:@ block. If the dep already exists we
-- return the original body untouched (caller reports \"no change\").
addDep :: Maybe Text -> Text -> Text -> Text
addDep mVer pkg body
  | pkg `elem` parseBuildDepends body = body
  | otherwise                          =
      let entry = case mVer of
            Nothing -> pkg
            Just v  -> pkg <> " " <> v
      in insertAfterBuildDepends entry body

-- | Remove the dep by deleting its comma-prefixed entry from every
-- continuation line and the header line. If the dep isn't present,
-- returns the original body unchanged.
removeDep :: Text -> Text -> Text
removeDep pkg body
  | pkg `notElem` parseBuildDepends body = body
  | otherwise = T.unlines (map dropFromLine (T.lines body))
  where
    dropFromLine ln =
      let toks     = map T.strip (splitOnCommas ln)
          filtered = filter (not . depMatches) toks
          rebuilt  = T.intercalate ", " filtered
          -- Preserve leading "," or "build-depends:" framing when
          -- there's no dep on the line. Conservative: if nothing was
          -- left, drop the line wholesale by returning empty.
      in if filtered == toks then ln else rebuilt

    depMatches tok =
      let (name, _) = T.break (\c -> isSpace c || c == '<' || c == '>' || c == '=' || c == '^' || c == '&') tok
      in T.strip name == pkg

-- | Append @, <entry>@ to the end of the library's build-depends
-- continuation. If no existing block is found we append a new line.
--
-- Indent derivation (fix for F-01/F-02):
--
-- * If @last pre@ is the @build-depends:@ header itself (no prior
--   continuation), align the new @, @ so the dep starts at the same
--   column as the value that is already on the header line. Using the
--   header's plain leading-whitespace (old behaviour) produced a
--   continuation at the same column as the field name, which cabal
--   3.0 rejects as @unexpected operator ","@.
-- * If @last pre@ is already a continuation line (a previous dep),
--   reuse its leading whitespace verbatim so the style is consistent
--   with what the author — or a previous call — put there.
insertAfterBuildDepends :: Text -> Text -> Text
insertAfterBuildDepends entry body =
  let lns = T.lines body
  in case splitAtBuildDependsEnd lns of
       Just (pre, post) ->
         T.unlines (pre <> [computeContinuationIndent (last pre) <> ", " <> entry] <> post)
       Nothing -> body <> "\n-- build-depends: " <> entry <> "\n"

-- | Compute the indent prefix for a new continuation line.
--
-- * Header line (contains @build-depends:@): indent so the inserted
--   dependency aligns with the value already on the header. Concretely:
--   leading-ws + len(\"build-depends:\") + spaces-to-value - 2 (the
--   @\", \"@ prefix we add later).
--
--   We also guarantee the result strictly exceeds the header's leading
--   whitespace (cabal 3.0 treats @col <= fieldCol@ as a new field).
--
-- * Continuation line (previous dep): reuse its leading whitespace
--   verbatim so a block stays visually consistent.
computeContinuationIndent :: Text -> Text
computeContinuationIndent ln
  | isBuildDependsHeader ln =
      let leading    = T.takeWhile isSpace ln
          afterLead  = T.drop (T.length leading) ln
          afterField = T.drop (T.length ("build-depends:" :: Text)) afterLead
          spacesBeforeValue = T.takeWhile isSpace afterField
          prefixCols =
            T.length leading
            + T.length ("build-depends:" :: Text)
            + T.length spacesBeforeValue
            - T.length (", " :: Text)
          -- cabal 3.0: continuation column must strictly exceed field's
          -- leading-ws column. Enforce that invariant as a lower bound.
          safeCols   = max prefixCols (T.length leading + 4)
      in T.replicate safeCols " "
  | otherwise = T.takeWhile isSpace ln

--------------------------------------------------------------------------------
-- stanza scoping (F-03 fix)
--------------------------------------------------------------------------------

-- | Parse a stanza selector from the agent into @(kind, maybe-name)@.
--
-- Accepted shapes:
--
-- * @library@
-- * @test-suite@            — first occurrence
-- * @test-suite:NAME@
-- * @executable@ / @executable:NAME@
-- * @benchmark@ / @benchmark:NAME@
-- * @foreign-library@ / @foreign-library:NAME@
--
-- Validation is strict: only alphanumerics, @-@, @_@, @:@ pass. Shell
-- metacharacters, path separators, whitespace are all rejected — the
-- string never reaches a shell (no spawns here) but defence in depth
-- is cheap.
parseStanzaSelector :: Text -> Either Text (Text, Maybe Text)
parseStanzaSelector raw
  | T.null stripped            = Left "stanza is empty"
  | T.any (not . okChar) stripped =
      Left ("invalid character in stanza selector: " <> raw)
  | otherwise = case T.splitOn ":" stripped of
      [kind]
        | kind `elem` allowedKinds -> Right (kind, Nothing)
        | otherwise                -> Left ("unknown stanza kind: " <> kind)
      [kind, name]
        | kind `elem` allowedKinds
        , not (T.null name)
        , T.all isIdChar name
        , isIdFirst (T.head name)
            -> Right (kind, Just name)
        | otherwise -> Left ("invalid stanza: " <> raw)
      _   -> Left ("invalid stanza format: " <> raw)
  where
    stripped     = T.strip raw
    allowedKinds =
      [ "library", "test-suite", "executable"
      , "benchmark", "foreign-library"
      ]
    okChar c   = isAlphaNum c || c == '-' || c == '_' || c == ':'
    isIdChar c = isAlphaNum c || c == '-' || c == '_'
    isIdFirst c = isAlphaNum c && not (isDigit c)

-- | Slice a list of lines into @(before, stanzaBody, after)@ based on
-- the selector. Returns 'Nothing' if no matching stanza header is
-- found.
sliceStanza
  :: (Text, Maybe Text)
  -> [Text]
  -> Maybe ([Text], [Text], [Text])
sliceStanza (kind, mName) lns =
  case break (matchesHeader kind mName) lns of
    (_,   [])     -> Nothing
    (pre, h : tl) ->
      let (body, post) = break isTopLevelStanzaHeader tl
      in Just (pre, h : body, post)

-- | Does @ln@ open the stanza described by @(kind, mName)@?
matchesHeader :: Text -> Maybe Text -> Text -> Bool
matchesHeader kind mName ln
  | not (T.null leading) = False   -- must be at column 0
  | otherwise =
      firstW == kind
      && case mName of
           Nothing
             | kind == "library" -> T.null rest   -- main library, no name
             | otherwise         -> True          -- first occurrence of kind
           Just name -> rest == name
  where
    leading  = T.takeWhile isSpace ln
    stripped = T.strip ln
    firstW   = T.takeWhile (not . isSpace) stripped
    rest     = T.strip (T.dropWhile (not . isSpace) stripped)

-- | True when @ln@ opens a new top-level stanza (library / executable
-- / test-suite / benchmark / foreign-library / common / flag /
-- source-repository). Used to determine where the previous stanza
-- body ends.
isTopLevelStanzaHeader :: Text -> Bool
isTopLevelStanzaHeader ln
  | not (T.null leading) = False
  | T.null stripped      = False
  | ":" `T.isInfixOf` stripped = False   -- top-level field, not stanza
  | otherwise = firstW `elem` kinds
  where
    leading  = T.takeWhile isSpace ln
    stripped = T.strip ln
    firstW   = T.takeWhile (not . isSpace) stripped
    kinds =
      [ "library", "executable", "test-suite", "benchmark"
      , "foreign-library", "common", "flag", "source-repository"
      ]

-- | Run a body-editor inside the selected stanza's slice and splice
-- the result back. When no selector is given, the editor runs on the
-- whole body (preserves legacy behaviour).
applyWithinStanza
  :: Maybe (Text, Maybe Text)
  -> (Text -> Text)
  -> Text
  -> Either Text Text
applyWithinStanza Nothing f body = Right (f body)
applyWithinStanza (Just sel) f body =
  let lns = T.lines body
  in case sliceStanza sel lns of
       Nothing -> Left ("stanza not found: " <> renderSelector sel)
       Just (pre, stanzaLns, post) ->
         let stanzaBody    = T.unlines stanzaLns
             newStanzaBody = f stanzaBody
             newStanzaLns  = T.lines newStanzaBody
         in Right (T.unlines (pre <> newStanzaLns <> post))

renderSelector :: (Text, Maybe Text) -> Text
renderSelector (k, Nothing)   = k
renderSelector (k, Just name) = k <> ":" <> name

-- | Split the source at the boundary between the end of the
-- build-depends block and whatever follows. Returns @Nothing@ if no
-- block was found.
splitAtBuildDependsEnd :: [Text] -> Maybe ([Text], [Text])
splitAtBuildDependsEnd ls =
  case break isBuildDependsHeader ls of
    (_, [])          -> Nothing
    (pre, h : rest)  ->
      let contLines = takeWhile isContinuation rest
          tailLines = drop (length contLines) rest
      in Just (pre <> (h : contLines), tailLines)

--------------------------------------------------------------------------------
-- response shaping
--------------------------------------------------------------------------------

listResult :: FilePath -> [Text] -> ToolResult
listResult file deps =
  let payload = object
        [ "action"       .= ("list" :: Text)
        , "cabal_file"   .= T.pack file
        , "count"        .= length deps
        , "build_depends".= deps
        ]
  in Env.toolResponseToResult (Env.mkOk payload)

editResult :: FilePath -> Text -> Text -> ToolResult
editResult file pkg verb =
  let payload = object
        [ "action"     .= verb
        , "cabal_file" .= T.pack file
        , "package"    .= pkg
        , "hint"       .= ( "Dependency set changed. The next \
                            \ghc_load reloads GHCi with the new \
                            \package graph — no explicit session \
                            \restart tool is needed."
                          :: Text )
        ]
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
unchangedResult file pkg verb =
  let note = case verb of
        "added"   -> "'" <> pkg <> "' already present in target stanza — no change written."
        "removed" -> "'" <> pkg <> "' not listed in target stanza — no change written."
        _         -> "no change written"
      payload = object
        [ "action"     .= ("unchanged" :: Text)
        , "verb"       .= verb
        , "cabal_file" .= T.pack file
        , "package"    .= pkg
        , "note"       .= (note :: Text)
        ]
  in Env.toolResponseToResult (Env.mkOk payload)

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
