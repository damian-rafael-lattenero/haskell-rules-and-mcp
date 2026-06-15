-- | Internal handler for the @explain@ branch of @ghc_deps@ (#63 + #94).
--
-- #156: rewritten to report package *usage locations* — which stanzas
-- declare the package and which source files import its modules — rather
-- than analysing cabal solver conflicts (the original MVP implementation
-- was addressing the wrong use-case per the rules matrix).
--
-- Algorithm:
--
--   1. Find the @.cabal@ file in @projectDir@.
--   2. Parse it line-by-line to identify stanzas and their
--      @build-depends@ lists; collect stanza names that mention
--      @package@.
--   3. For each matching stanza, collect @hs-source-dirs@ (defaulting
--      to @.@) and enumerate @.hs@ files under them.
--   4. Scan those files for @import@ lines whose module name contains
--      any capitalised component of @package@ (e.g. @aeson@ →
--      @\"Aeson\"@, @data-default@ → @[\"Data\",\"Default\"]@).
--   5. Return @{package, stanzas, import_sites, summary}@.
--
-- Issue #94 Phase C retired the @ghc_deps_explain@ wire surface;
-- 'HaskellFlows.Tool.Deps' is the single externally-advertised tool.
-- This module's 'handle' is the implementation 'Deps.handle'
-- forwards to when @action="explain"@.
module HaskellFlows.Tool.DepsExplain
  ( handle
  , DepsExplainArgs (..)
    -- * Exported for unit tests
  , cabalComponentsMatchingPkg
  , importMatchesPkg
  , pkgSearchTokens
    -- * Legacy solver-conflict helpers (kept for existing tests)
  , Conflict (..)
  , Rejection (..)
  , parseSolverOutput
  , parseRejections
  , identifyRootCause
  , extractPackages
  ) where

import Control.Exception (SomeException, try)
import Data.Aeson
import Data.Aeson.Types (parseEither)
import Data.Char (isAsciiLower, isDigit, toUpper)
import Data.List (nub)
import Data.Maybe (catMaybes, fromMaybe)
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.IO as TIO
import System.Directory (listDirectory)
import System.FilePath ((</>), takeExtension)

import HaskellFlows.Mcp.Envelope (ToolResponse)
import qualified HaskellFlows.Mcp.Envelope as Env
import HaskellFlows.Mcp.Protocol ()
import HaskellFlows.Types (ProjectDir, unProjectDir)


-- ---------------------------------------------------------------------------
-- Args
-- ---------------------------------------------------------------------------

-- | Two modes for the @explain@ action:
--
-- * 'DepsExplainPkg' — @package=X@: show which stanzas declare X and
--   which source files import it (#156 new behaviour).
-- * 'DepsExplainDump' — @cabal_output=X@: parse a solver dump and
--   extract the root-cause conflict (#63 original behaviour, kept so
--   that E2E tests and existing callers that pass a pre-fetched dump
--   continue to work).
data DepsExplainArgs
  = DepsExplainPkg  !Text   -- ^ package=X
  | DepsExplainDump !Text   -- ^ cabal_output=X
  deriving stock (Show)

instance FromJSON DepsExplainArgs where
  parseJSON = withObject "DepsExplainArgs" $ \o -> do
    mPkg    <- o .:? "package"
    mOutput <- o .:? "cabal_output"
    case (mPkg, mOutput) of
      (Just p,  _       ) -> pure (DepsExplainPkg p)
      (Nothing, Just out) -> pure (DepsExplainDump out)
      (Nothing, Nothing ) ->
        fail "Either 'package' or 'cabal_output' is required for explain"

-- ---------------------------------------------------------------------------
-- Main handler
-- ---------------------------------------------------------------------------

handle :: ProjectDir -> Value -> IO ToolResponse
handle pd rawArgs = case parseEither parseJSON rawArgs of
  Left err ->
    pure (Env.mkFailed
      ((Env.mkErrorEnvelope Env.MissingArg
          (T.pack ("Invalid arguments: " <> err)))
            { Env.eeCause = Just (T.pack err) }))
  Right (DepsExplainPkg  pkg)  -> handlePkgUsage pd pkg
  Right (DepsExplainDump dump) -> pure (handleSolverConflict dump)

-- | #156: show which stanzas declare @pkg@ and which source files import it.
handlePkgUsage :: ProjectDir -> Text -> IO ToolResponse
handlePkgUsage pd pkg = do
    let root = unProjectDir pd
    -- Step 1: find the cabal file
    mCabal <- findCabalFile root
    case mCabal of
      Nothing ->
        pure (Env.mkFailed
          (Env.mkErrorEnvelope Env.SubprocessError
            "No .cabal file found in the project root."))
      Just cabalPath -> do
        cabalText <- TIO.readFile cabalPath
        -- Step 2: collect stanzas + source dirs
        let (stanzas, sourceDirsByStanza) =
              cabalComponentsMatchingPkg pkg cabalText
        -- Step 3: gather .hs files under the collected source dirs
        let allSrcDirs = nub (concatMap snd sourceDirsByStanza)
        hsFiles <- gatherHsFiles root allSrcDirs
        -- Step 4: scan for matching imports
        importSites <- findImportSites pkg hsFiles
        -- Step 5: shape the response
        let n        = length importSites
            k        = length stanzas
            summary  = "'" <> pkg <> "' is declared in "
                     <> T.pack (show k) <> " stanza"
                     <> (if k == 1 then "" else "s")
                     <> " and imported in "
                     <> T.pack (show n) <> " source file"
                     <> (if n == 1 then "" else "s") <> "."
            payload  = object
              [ "package"      .= pkg
              , "stanzas"      .= stanzas
              , "import_sites" .= importSites
              , "summary"      .= summary
              ]
        pure $ if null stanzas
            then Env.mkNoMatch (object
                   [ "package" .= pkg
                   , "hint"    .= ("Package not found in any build-depends. \
                                   \Check the spelling or use ghc_deps \
                                   \action='list' to see declared deps." :: Text)
                   ])
            else Env.mkOk payload

-- | #63: parse a cabal solver dump and surface the root-cause conflict.
-- Returns @status=ok@ with a @conflict@ object when rejections are found,
-- or @status=no_match@ with @conflict=null@ for a clean (conflict-free) dump.
handleSolverConflict :: Text -> ToolResponse
handleSolverConflict dump =
  case parseSolverOutput dump of
    Nothing ->
      Env.mkNoMatch (object ["conflict" .= Null])
    Just c  ->
      let root    = cRoot c
          payload = object
            [ "conflict" .= object
                [ "root_cause" .= object
                    [ "package" .= rPackage root
                    , "depth"   .= rDepth root
                    , "reason"  .= rReason root
                    ]
                , "packages"  .= cPackages c
                , "backjumps" .= cBackjumps c
                ]
            ]
      in Env.mkOk payload

-- ---------------------------------------------------------------------------
-- Cabal file parsing
-- ---------------------------------------------------------------------------

-- | Find the first @.cabal@ file in the project root directory.
findCabalFile :: FilePath -> IO (Maybe FilePath)
findCabalFile root = do
  entries <- try (listDirectory root) :: IO (Either SomeException [FilePath])
  case entries of
    Left _  -> pure Nothing
    Right fs ->
      let cabal = filter (\f -> takeExtension f == ".cabal") fs
      in case cabal of
           (f : _) -> pure (Just (root </> f))
           []      -> pure Nothing

-- | Parse the cabal file text and return:
--   * @stanzas@: names of stanzas that list @pkg@ in @build-depends@
--   * @sourceDirsByStanza@: @[(stanzaName, [srcDir])]@ for matching stanzas
--
-- Uses a simple line-by-line state machine — not a full cabal AST.
-- Recognises the common stanza headers and @build-depends@ / @hs-source-dirs@.
cabalComponentsMatchingPkg
  :: Text                          -- ^ package name (lowercase, hyphenated)
  -> Text                          -- ^ cabal file contents
  -> ([Text], [(Text, [Text])])    -- ^ (matchingStanzas, [(stanza, srcDirs)])
cabalComponentsMatchingPkg pkg cabalText =
  let ls  = T.lines cabalText
      sects = parseSections ls
      matching = filter (stanzaDeclarsPkg pkg) sects
      stanzaNames = map sName matching
      srcDirMap   = [ (sName s, if null (sSrcDirs s) then ["."] else sSrcDirs s)
                    | s <- matching ]
  in (stanzaNames, srcDirMap)

data Section = Section
  { sName    :: !Text
  , sDeps    :: ![Text]   -- ^ build-depends tokens
  , sSrcDirs :: ![Text]   -- ^ hs-source-dirs tokens
  }
  deriving stock (Show)

-- | Check if a section's deps mention @pkg@ (ignoring version bounds).
stanzaDeclarsPkg :: Text -> Section -> Bool
stanzaDeclarsPkg pkg s =
  any (depMatchesPkg pkg) (sDeps s)

-- | Check if a single dep token matches the package name.
-- Strips any version constraint suffix before comparing.
depMatchesPkg :: Text -> Text -> Bool
depMatchesPkg pkg dep =
  let bare = T.takeWhile (\c -> c /= ' ' && c /= '>' && c /= '<'
                             && c /= '=' && c /= ',') (T.strip dep)
  in T.toLower bare == T.toLower pkg

-- | Simple state-machine parser: collect sections from cabal lines.
--
-- Handles multi-line @build-depends:@ blocks — continuation lines
-- starting with @,@ or plain indented dependency tokens after the
-- initial @build-depends:@ heading are collected until the next
-- unindented field or stanza header.
parseSections :: [Text] -> [Section]
parseSections = go Nothing False []
  where
    -- inDeps: True while we're collecting continuation lines for build-depends
    go mCurrent _inDeps acc [] =
      reverse (maybe acc (: acc) mCurrent)
    go mCurrent inDeps acc (ln : rest)
      -- Blank lines don't exit build-depends continuation
      | T.null (T.strip ln) =
          go mCurrent inDeps acc rest
      -- New stanza header ends any field continuation
      | Just name <- detectHeader ln =
          let acc' = maybe acc (: acc) mCurrent
          in go (Just (Section name [] [])) False acc' rest
      -- Field inside a stanza
      | Just cur <- mCurrent =
          let stripped = T.strip ln
              -- Indented (non-top-level) lines can continue build-depends.
              -- A line is a continuation if it starts with a comma or if
              -- we're in inDeps mode and the line doesn't introduce a new field.
              isContinuation =
                inDeps && not ("build-depends:" `T.isPrefixOf` stripped)
                       && not ("hs-source-dirs:" `T.isPrefixOf` stripped)
                       && isIndented ln
          in case T.stripPrefix "build-depends:" stripped of
               Just deps ->
                 let tokens = splitDepLine deps
                 in go (Just cur { sDeps = sDeps cur ++ tokens }) True acc rest
               Nothing
                 | isContinuation ->
                     -- Continuation dep line: e.g. "    , aeson ^>= 2.0"
                     let stripped2 = T.stripStart (T.dropWhile (== ',') stripped)
                         tokens    = splitDepLine stripped2
                     in go (Just cur { sDeps = sDeps cur ++ tokens }) True acc rest
                 | otherwise ->
                     case T.stripPrefix "hs-source-dirs:" stripped of
                       Just dirs ->
                         let ds = map T.strip (T.splitOn "," dirs)
                         in go (Just cur { sSrcDirs = sSrcDirs cur ++ ds })
                                False acc rest
                       Nothing ->
                         go mCurrent False acc rest
      | otherwise =
          go mCurrent False acc rest

    isIndented ln = case T.uncons ln of
      Just (c, _) -> c == ' ' || c == '\t'
      Nothing     -> False

    detectHeader ln
      | "library" `T.isPrefixOf` T.stripStart ln
        && (T.strip ln == "library"
            || "library " `T.isPrefixOf` T.strip ln) =
          let r    = T.drop (T.length "library") (T.strip ln)
              name = T.strip r
          in Just ("library" <> if T.null name then "" else " " <> name)
      | "test-suite" `T.isPrefixOf` T.stripStart ln =
          Just (T.strip (T.drop (T.length "test-suite") (T.stripStart ln)))
      | "executable" `T.isPrefixOf` T.stripStart ln =
          Just (T.strip (T.drop (T.length "executable") (T.stripStart ln)))
      | "benchmark" `T.isPrefixOf` T.stripStart ln =
          Just (T.strip (T.drop (T.length "benchmark") (T.stripStart ln)))
      | otherwise = Nothing

    -- Split a deps fragment on comma; trim each token.
    splitDepLine txt =
      filter (not . T.null) (map T.strip (T.splitOn "," txt))

-- ---------------------------------------------------------------------------
-- Import site scanning
-- ---------------------------------------------------------------------------

-- | Collect all @.hs@ files under the given source directories.
gatherHsFiles :: FilePath -> [Text] -> IO [FilePath]
gatherHsFiles root srcDirs = do
  let dirs = map (\d -> root </> T.unpack d) srcDirs
  fmap (nub . concat) (traverse (recHsFiles []) dirs)
  where
    recHsFiles acc dir = do
      eEntries <- try (listDirectory dir) :: IO (Either SomeException [FilePath])
      case eEntries of
        Left _ -> pure acc
        Right entries -> do
          let fullPaths = map (dir </>) entries
          concat <$> traverse (classify acc) fullPaths
    classify acc p
      | takeExtension p == ".hs" = pure (p : acc)
      | otherwise = do
          eExists <- try (listDirectory p) :: IO (Either SomeException [FilePath])
          case eExists of
            Right _ -> recHsFiles acc p   -- it's a directory
            Left  _ -> pure acc

-- | Scan files for @import@ lines that match the package.
-- Returns relative paths (relative to project root) of files that
-- contain at least one matching import.
findImportSites :: Text -> [FilePath] -> IO [Text]
findImportSites pkg files = do
  hits <- traverse (fileHasMatchingImport pkg) files
  pure (catMaybes hits)

fileHasMatchingImport :: Text -> FilePath -> IO (Maybe Text)
fileHasMatchingImport pkg fp = do
  eLines <- try (TIO.readFile fp) :: IO (Either SomeException Text)
  case eLines of
    Left _  -> pure Nothing
    Right body ->
      let ls = T.lines body
          hasHit = any (importMatchesPkg pkg) ls
      in pure (if hasHit then Just (T.pack fp) else Nothing)

-- | Check whether a single line looks like an @import@ of a module from
-- @pkg@. Heuristic: the module name must contain at least one capitalised
-- token derived from the hyphen-components of the package name.
--
-- Examples:
--   @importMatchesPkg "aeson" "import Data.Aeson"@        → True
--   @importMatchesPkg "aeson" "import qualified Data.Aeson.Key"@ → True
--   @importMatchesPkg "base"  "import Data.Maybe"@         → False (too generic)
--   @importMatchesPkg "QuickCheck" "import Test.QuickCheck"@ → True
importMatchesPkg :: Text -> Text -> Bool
importMatchesPkg pkg ln =
  let stripped = T.stripStart ln
  in case T.stripPrefix "import" stripped of
       Nothing -> False
       Just afterImport ->
         -- Module name is the first non-whitespace token after
         -- 'import' or 'import qualified'
         let withoutQual = case T.stripPrefix " qualified " afterImport of
               Just s  -> s
               Nothing ->
                 fromMaybe afterImport (T.stripPrefix " qualified\t" afterImport)
             modName = T.takeWhile (\c -> c /= ' ' && c /= '\t'
                                       && c /= '(' && c /= '\n')
                                   (T.stripStart withoutQual)
             tokens = pkgSearchTokens pkg
         in any (`T.isInfixOf` modName) tokens

-- | Derive capitalised search tokens from a hyphenated package name.
-- @\"data-default\"@ → @[\"DataDefault\",\"Default\",\"Data\"]@
-- @\"aeson\"@        → @[\"Aeson\"]@
-- @\"QuickCheck\"@  → @[\"Quickcheck\",\"QuickCheck\",\"Quick\",\"Check\"]@
pkgSearchTokens :: Text -> [Text]
pkgSearchTokens pkg =
  let parts = T.splitOn "-" (T.toLower pkg)
      capd  = map capitalise parts
      joined = T.concat capd
  in nub (filter (not . T.null) (joined : capd))
  where
    capitalise t = case T.uncons t of
      Just (c, rest) -> T.cons (toUpper c) rest
      Nothing        -> ""

-- ---------------------------------------------------------------------------
-- Legacy solver-conflict helpers (kept because DepsExplain unit tests
-- still import and test them directly).
-- ---------------------------------------------------------------------------

-- | One @rejecting: pkg-version (conflict: …)@ entry from the solver.
data Rejection = Rejection
  { rDepth   :: !Int   -- ^ indent depth from the @[__N]@ marker
  , rPackage :: !Text  -- ^ "pkg-version", e.g. "aeson-2.2.3.0"
  , rReason  :: !Text  -- ^ what's after \"conflict:\", trimmed
  }
  deriving stock (Eq, Show)

-- | The structured report the tool returns when it finds a
-- conflict in the input.
data Conflict = Conflict
  { cRoot      :: !Rejection
  , cPackages  :: ![Text]    -- ^ unique package names involved
  , cBackjumps :: !(Maybe Int)  -- ^ backjump limit if reported
  , cAll       :: ![Rejection]  -- ^ every rejection seen, in order
  }
  deriving stock (Eq, Show)

-- | Parse a solver dump. Returns 'Nothing' when the input contains
-- no @rejecting:@ lines.
parseSolverOutput :: Text -> Maybe Conflict
parseSolverOutput txt =
  let rejections = concatMap parseRejections (T.lines txt)
  in case rejections of
       [] -> Nothing
       _  -> Just Conflict
               { cRoot      = identifyRootCause rejections
               , cPackages  = extractPackages rejections
               , cBackjumps = parseBackjumps txt
               , cAll       = rejections
               }

-- | One line → zero or more rejections.
parseRejections :: Text -> [Rejection]
parseRejections raw =
  let stripped = T.stripStart raw
  in case T.stripPrefix "[__" stripped of
       Nothing -> []
       Just afterMarker ->
         let (depthTxt, rest) = T.breakOn "]" afterMarker
         in case T.unpack (T.strip depthTxt) of
              ds | all isDigit ds, not (null ds) ->
                let depth   = read ds
                    afterRb = T.stripStart (T.drop 1 rest)
                in case T.stripPrefix "rejecting:" afterRb of
                     Nothing -> []
                     Just rj ->
                       let trimmed = T.stripStart rj
                           (pkgsRaw, parenAndAfter) =
                             T.breakOn " (conflict:" trimmed
                           reason = case T.stripPrefix " (conflict:"
                                           parenAndAfter of
                             Just inside -> stripCloseParen (T.stripStart inside)
                             Nothing     -> ""
                           pkgs = filter (not . T.null)
                                    (map T.strip (T.splitOn "," pkgsRaw))
                       in [ Rejection { rDepth = depth, rPackage = p, rReason = reason }
                          | p <- pkgs ]
              _  -> []

stripCloseParen :: Text -> Text
stripCloseParen t =
  case T.unsnoc (T.stripEnd t) of
    Just (rest, ')') -> T.stripEnd rest
    _                -> t

-- | The root cause is the rejection at the greatest depth.
identifyRootCause :: [Rejection] -> Rejection
identifyRootCause [r] = r
identifyRootCause (r0 : rs) = foldr deepest r0 rs
  where
    deepest r acc = if rDepth r > rDepth acc then r else acc
identifyRootCause [] =
  Rejection { rDepth = 0, rPackage = "", rReason = "" }

-- | Best-effort extraction of package names from the rejection list.
extractPackages :: [Rejection] -> [Text]
extractPackages rs =
  nub $ concatMap (\r -> [stripVersion (rPackage r)] <> hostNames (rReason r))
                  rs
  where
    stripVersion pkg =
      case reverse (T.splitOn "-" pkg) of
        (verLast : nameRev)
          | not (T.null verLast)
          , isVersionLike verLast
          -> T.intercalate "-" (reverse nameRev)
        _ -> pkg
    isVersionLike t = case T.uncons t of
      Just (c, _) -> isDigit c
      Nothing     -> False
    hostNames reason =
      take 2 [ tok | tok <- T.words reason
                   , isPkgIdent tok
                   ]
    isPkgIdent t = case T.uncons t of
      Just (c, rest) -> isAsciiLower c
                     && T.all (\ch -> isAsciiLower ch
                                   || isDigit ch
                                   || ch == '-' || ch == '_') rest
                     && T.length t >= 2
      Nothing -> False

parseBackjumps :: Text -> Maybe Int
parseBackjumps txt =
  case T.breakOn "backjump limit reached (currently " txt of
    (_, after) | not (T.null after) ->
      let payload = T.drop (T.length "backjump limit reached (currently ") after
          numTxt  = T.takeWhile isDigit payload
      in if T.null numTxt then Nothing
         else case reads (T.unpack numTxt) of
                ((n, _) : _) -> Just n
                _            -> Nothing
    _ -> Nothing
