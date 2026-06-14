-- | Cabal-file parsing and stanza model for @ghc_deps@.
--
-- All functions except 'findCabalFile' are pure. No subprocess spawns.
-- Importers that only need the parsing primitives get a module with a
-- minimal import footprint.
module HaskellFlows.Tool.Deps.Cabal
  ( -- * File discovery
    findCabalFile
    -- * Build-depends parsing
  , parseBuildDepends
  , extractBuildDependsBody
  , isBuildDependsHeader
  , isContinuation
  , looksLikeFieldHeader
  , splitOnCommas
    -- * Pure dep edits
  , addDep
  , removeDep
  , insertAfterBuildDepends
  , computeContinuationIndent
  , splitAtBuildDependsEnd
    -- * Stanza model
  , sliceStanza
  , applyWithinStanza
  , resolveStanza
  , allStanzaDeps
  , matchesHeader
  , isTopLevelStanzaHeader
    -- * Post-edit structural check
  , editAgreesWithVerb
  ) where

import Data.Char (isSpace)
import Data.List (sortOn)
import Data.Text (Text)
import qualified Data.Text as T
import System.Directory (doesDirectoryExist, listDirectory)
import System.FilePath (takeExtension, (</>))

import HaskellFlows.Tool.Deps.Validate (parseStanzaSelector, renderSelector)
import HaskellFlows.Types (ProjectDir, unProjectDir)

--------------------------------------------------------------------------------
-- file discovery
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
-- build-depends parsing
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

--------------------------------------------------------------------------------
-- pure dep edits
--------------------------------------------------------------------------------

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
--
-- Issue #118: when a continuation line carries *only* the package being
-- removed, rebuilding it produces an empty string. The old @map
-- dropFromLine@ path kept these empty strings and @T.unlines@ rendered
-- them as blank lines in the @.cabal@ file. The new @concatMap@ path
-- drops the line entirely instead, keeping the dep block tidy.
removeDep :: Text -> Text -> Text
removeDep pkg body
  | pkg `notElem` parseBuildDepends body = body
  | otherwise = T.unlines (concatMap dropFromLine (T.lines body))
  where
    dropFromLine ln =
      let toks     = map T.strip (splitOnCommas ln)
          filtered = filter (not . depMatches) toks
      in if filtered == toks
           then [ln]           -- nothing removed on this line — keep as-is
           else let rebuilt = T.intercalate ", " filtered
                in [rebuilt | not (T.null (T.strip rebuilt))]

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
-- stanza model
--------------------------------------------------------------------------------

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

-- | F-08: enumerate all top-level stanzas and their deps. Returns
-- one entry per stanza in cabal file order. The stanza key is
-- @\"kind\"@ for name-less stanzas (e.g. @library@) and
-- @\"kind:name\"@ for named ones (e.g. @test-suite:mytest@).
allStanzaDeps :: Text -> [(Text, [Text])]
allStanzaDeps body = go (T.lines body)
  where
    go [] = []
    go (ln : rest)
      | isTopLevelStanzaHeader ln =
          let (bodyLns, after) = break isTopLevelStanzaHeader rest
              key  = stanzaKey ln
              deps = parseBuildDepends (T.unlines (ln : bodyLns))
          in (key, deps) : go after
      | otherwise = go rest

    stanzaKey ln =
      let s      = T.strip ln
          firstW = T.takeWhile (not . isSpace) s
          nm     = T.strip (T.drop (T.length firstW) s)
      in if T.null nm then firstW else firstW <> ":" <> nm

--------------------------------------------------------------------------------
-- post-edit structural check
--------------------------------------------------------------------------------

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
