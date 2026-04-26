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
import qualified Data.Text.Lazy as TL
import qualified Data.Text.Lazy.Encoding as TLE
import GHC.IO.Handle.Lock (hLock, hUnlock, LockMode (..))
import System.Directory (doesDirectoryExist, listDirectory)
import System.FilePath (takeExtension, (</>))
import System.IO (IOMode (..), openFile, hClose)
import System.IO.Unsafe (unsafePerformIO)

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
    , tdInputSchema =
        object
          [ "type"       .= ("object" :: Text)
          , "properties" .= object
              [ "action"  .= object
                  [ "type" .= ("string" :: Text)
                  , "enum" .= (["list", "add", "remove"] :: [Text])
                  ]
              , "package" .= object
                  [ "type"        .= ("string" :: Text)
                  , "description" .=
                      ("Hackage package name. Required for add/remove."
                       :: Text)
                  ]
              , "version" .= object
                  [ "type"        .= ("string" :: Text)
                  , "description" .=
                      ("Optional cabal version constraint. Example: \
                       \\">= 2.14\", \"^>= 1.4\". Only used on 'add'."
                       :: Text)
                  ]
              , "stanza" .= object
                  [ "type"        .= ("string" :: Text)
                  , "description" .=
                      ("Optional stanza selector. Restrict the edit to a \
                       \specific stanza of the .cabal file. Accepted: \
                       \\"library\" (main library), \"test-suite\" (first \
                       \occurrence), \"test-suite:NAME\", \"executable\" / \
                       \\"executable:NAME\", \"benchmark\" / \
                       \\"benchmark:NAME\", \"foreign-library\" / \
                       \\"foreign-library:NAME\". Omit to target the first \
                       \build-depends block in the file \
                       \(backwards-compatible default)."
                       :: Text)
                  ]
              ]
          , "required"             .= ["action" :: Text]
          , "additionalProperties" .= False
          ]
    }

data Action = ActList | ActAdd | ActRemove
  deriving stock (Eq, Show)

data DepsArgs = DepsArgs
  { daAction  :: !Action
  , daPackage :: !(Maybe Text)
  , daVersion :: !(Maybe Text)
  , daStanza  :: !(Maybe Text)
  }
  deriving stock (Show)

instance FromJSON DepsArgs where
  parseJSON = withObject "DepsArgs" $ \o -> do
    a <- o .:  "action"
    p <- o .:? "package"
    v <- o .:? "version"
    s <- o .:? "stanza"
    act <- case (a :: Text) of
      "list"   -> pure ActList
      "add"    -> pure ActAdd
      "remove" -> pure ActRemove
      other    -> fail ("unknown action: " <> T.unpack other)
    pure DepsArgs
      { daAction  = act
      , daPackage = p
      , daVersion = v
      , daStanza  = s
      }

handle :: ProjectDir -> Value -> IO ToolResult
handle pd rawArgs = case parseEither parseJSON rawArgs of
  Left parseError ->
    pure (errorResult (T.pack ("Invalid arguments: " <> parseError)))
  Right args -> do
    mCabal <- findCabalFile pd
    case mCabal of
      Nothing ->
        pure (errorResult "No .cabal file found in project root")
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
          Right mSel -> runEdit file safePkg mSel (addDep safeVer) "added"
  ActRemove -> case daPackage args of
    Nothing  -> pure (errorResult "'package' is required for remove")
    Just pkg -> case validatePackageName pkg of
      Left err       -> pure (errorResult err)
      Right safePkg  -> case traverse parseStanzaSelector (daStanza args) of
        Left err   -> pure (errorResult err)
        Right mSel -> runEdit file safePkg mSel removeDep "removed"

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
  -> IO ToolResult
runEdit file pkg mStanza f verb = withCabalLock file $ do
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
              Right _ -> pure (editResult file pkg verb)

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
  let payload =
        object
          [ "success"      .= True
          , "action"       .= ("list" :: Text)
          , "cabal_file"   .= T.pack file
          , "count"        .= length deps
          , "build_depends".= deps
          ]
  in ToolResult
       { trContent = [ TextContent (encodeUtf8Text payload) ]
       , trIsError = False
       }

editResult :: FilePath -> Text -> Text -> ToolResult
editResult file pkg verb =
  let payload =
        object
          [ "success"    .= True
          , "action"     .= verb
          , "cabal_file" .= T.pack file
          , "package"    .= pkg
          , "hint"       .= ( "Dependency set changed. The next \
                             \ghc_load reloads GHCi with the new \
                             \package graph — no explicit session \
                             \restart tool is needed."
                            :: Text )
          ]
  in ToolResult
       { trContent = [ TextContent (encodeUtf8Text payload) ]
       , trIsError = False
       }

-- | Idempotent no-op payload. Shape parallels 'editResult' but marks
-- the action as \"unchanged\" with a verb-aware @note@ field so the
-- agent gets a clear signal that the post-condition is already met
-- (on @add@: package is present; on @remove@: package is absent)
-- instead of the earlier remove-shaped \"not found or already at
-- desired state\" error that fired on every idempotent add too.
unchangedResult :: FilePath -> Text -> Text -> ToolResult
unchangedResult file pkg verb =
  let note = case verb of
        "added"   -> "'" <> pkg <> "' already present in target stanza — no change written."
        "removed" -> "'" <> pkg <> "' not listed in target stanza — no change written."
        _         -> "no change written"
      payload =
        object
          [ "success"    .= True
          , "action"     .= ("unchanged" :: Text)
          , "verb"       .= verb
          , "cabal_file" .= T.pack file
          , "package"    .= pkg
          , "note"       .= (note :: Text)
          ]
  in ToolResult
       { trContent = [ TextContent (encodeUtf8Text payload) ]
       , trIsError = False
       }

errorResult :: Text -> ToolResult
errorResult msg =
  ToolResult
    { trContent = [ TextContent (encodeUtf8Text (object
        [ "success" .= False
        , "error"   .= msg
        ]))
      ]
    , trIsError = True
    }

encodeUtf8Text :: Value -> Text
encodeUtf8Text = TL.toStrict . TLE.decodeUtf8 . encode
