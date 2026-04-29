-- | @ghc_quickcheck_export@ — materialize a runnable Haskell test
-- file from the persisted QuickCheck property store.
--
-- `ghc_quickcheck` auto-saves passing properties to
-- `.haskell-flows/properties.json`. That file is internal state —
-- not runnable by `cabal test` directly. This tool closes the loop:
-- reads the store, emits a `test/Spec.hs`-shaped Main module with
-- one `runProp` line per property, and writes it to disk at a
-- caller-chosen path.
--
-- Once exported + committed, the project's standard `cabal test`
-- replays every persisted property the same way `ghc_regression`
-- does from inside GHCi — but from CI, without the MCP in the
-- loop.
--
-- Security:
--
-- * Output path routed through 'mkModulePath' so a path-traversal
--   attempt is rejected by the existing boundary validator
--   (CWE-22 defence).
-- * Property text is NOT re-sanitised on write — every stored
--   property passed through 'sanitizeExpression' at capture time,
--   so embedding verbatim is safe.
-- * Generated code is a plain `module Main where`; it never
--   injects cabal / ghc flags the user didn't opt in to.
module HaskellFlows.Tool.QuickCheckExport
  ( descriptor
  , handle
  , ExportArgs (..)
  , renderTestFile
  , renderTestFileWith
  , sanitizeLabel
  , modulePathToModule
  ) where

import Control.Exception (SomeException, try)
import Data.Aeson
import Data.Aeson.Types (parseEither)
import Data.Char (isAlphaNum, isAsciiLower, isAsciiUpper, isDigit)
import Data.List (nub, sort)
import Data.Maybe (mapMaybe)
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.IO as TIO
import System.Directory (createDirectoryIfMissing)
import System.FilePath (takeDirectory)

import HaskellFlows.Data.PropertyStore (Store, StoredProperty (..), loadAll)
import qualified HaskellFlows.Mcp.Envelope as Env
import HaskellFlows.Mcp.ParseError (formatParseError)
import HaskellFlows.Mcp.Protocol
import HaskellFlows.Mcp.ToolName (ToolName (..), toolNameText)
import qualified HaskellFlows.Tool.QuickCheck as Qc
import HaskellFlows.Types (ProjectDir, mkModulePath, unModulePath, unProjectDir)

descriptor :: ToolDescriptor
descriptor =
  ToolDescriptor
    { tdName        = toolNameText GhcQuickCheckExport
    , tdDescription =
        "Emit a runnable test/Spec.hs materialising every property "
          <> "persisted to .haskell-flows/properties.json. After a commit "
          <> "'cabal test' replays the set the same way ghc_regression "
          <> "does from inside GHCi, but from CI with no MCP in the loop."
    , tdInputSchema =
        object
          [ "type"       .= ("object" :: Text)
          , "properties" .= object
              [ "output_path" .= object
                  [ "type"        .= ("string" :: Text)
                  , "description" .=
                      ("Relative path where the test file is written. "
                       <> "Default: \"test/Spec.hs\"." :: Text)
                  ]
              , "module" .= object
                  [ "type"        .= ("string" :: Text)
                  , "description" .=
                      ("Optional filter: export only properties whose "
                       <> "stored module matches this value." :: Text)
                  ]
              ]
          , "additionalProperties" .= False
          ]
    }

data ExportArgs = ExportArgs
  { eaOutputPath :: !(Maybe Text)
  , eaModule     :: !(Maybe Text)
  }
  deriving stock (Show)

instance FromJSON ExportArgs where
  parseJSON = withObject "ExportArgs" $ \o ->
    ExportArgs
      <$> o .:? "output_path"
      <*> o .:? "module"

--------------------------------------------------------------------------------
-- handle
--------------------------------------------------------------------------------

handle :: Store -> ProjectDir -> Value -> IO ToolResult
handle store pd rawArgs = case parseEither parseJSON rawArgs of
  Left err -> pure (formatParseError err)
  Right args -> do
    props <- loadAll store
    let filtered = case eaModule args of
          Nothing -> props
          Just m  -> [ p | p <- props, spModule p == Just m ]
        outRel = maybe "test/Spec.hs" T.unpack (eaOutputPath args)
    case mkModulePath pd outRel of
      Left err -> pure (pathTraversalResult
                          (T.pack ("Invalid output path: " <> show err)))
      Right mp -> do
        -- Resolve the project's library exposed-modules so a property
        -- whose 'spModule' was a test scope (e.g. 'test/Spec.hs') can
        -- still reference library symbols (issue #40). 'libraryExposedModules'
        -- returns @[]@ on any I/O / parse failure so a corrupt or
        -- absent .cabal degrades to the historical behaviour.
        libMods <- Qc.libraryExposedModules pd
        -- Identify the would-be module name for the output file. Any
        -- inferred import that matches it is the file importing
        -- itself — circular and invalid (#40).
        let selfHint = modulePathToModule (T.pack outRel)
            full     = unModulePath mp
            body     = renderTestFileWith selfHint libMods filtered
        eRes <- try (do
          createDirectoryIfMissing True (takeDirectory full)
          TIO.writeFile full body) :: IO (Either SomeException ())
        case eRes of
          Left ex -> pure (subprocessResult
                              (T.pack ("Could not write: " <> show ex)))
          Right _ -> pure (successResult full (length filtered) (length props))
  where
    _ = unProjectDir pd  -- silence unused when path validation is the only use

--------------------------------------------------------------------------------
-- rendering
--------------------------------------------------------------------------------

-- | Emit the full test file contents from a list of stored
-- properties. Imports are derived from the unique set of
-- 'spModule' values that look like source paths
-- (@src/Foo/Bar.hs@) — those are mapped to module names
-- (@Foo.Bar@) and imported.
--
-- Each property becomes one numbered @prop_N@ binding + one line
-- in @main@'s list.
--
-- Compatibility wrapper. Equivalent to
-- @renderTestFileWith Nothing []@. Live callers go through
-- 'renderTestFileWith' so they can pass the project's library
-- exposed-modules + output self-module hint (issue #40).
renderTestFile :: [StoredProperty] -> Text
renderTestFile = renderTestFileWith Nothing []

-- | Issue #40: extended renderer that takes the project's library
-- exposed-modules ('libMods') and the output file's would-be
-- module name ('selfHint').
--
--   * 'libMods' are unioned with the property-derived modules so
--     properties authored in test scope (whose 'spModule' is e.g.
--     @"test/Spec.hs"@) still see every library symbol they
--     reference at compile time.
--   * 'selfHint' (when 'Just') is filtered out of the import list
--     so the generated file never tries to @import Spec@ when it
--     IS @test/Spec.hs@ — that produced the broken circular
--     header reported in the bug.
--
-- 'libMods' empty + 'selfHint' 'Nothing' reproduces the legacy
-- behaviour byte-for-byte; preserved by 'renderTestFile' above.
renderTestFileWith :: Maybe Text -> [Text] -> [StoredProperty] -> Text
renderTestFileWith selfHint libMods props =
  let propMods    = mapMaybe (spModule >=> modulePathToModule) props
      combined    = nub . sort $ libMods <> propMods
      withoutSelf = case selfHint of
        Just sm -> filter (/= sm) combined
        Nothing -> combined
      importLines =
        [ "import " <> m | m <- withoutSelf ]
      propLines =
        zipWith renderPropBinding [1 :: Int ..] props
      runLines =
        zipWith renderRunLine [1 :: Int ..] props
  in T.unlines $
       [ "-- Generated by ghc_quickcheck_export. Do not edit by hand;"
       , "-- regenerate whenever .haskell-flows/properties.json changes."
       , "module Main where"
       , ""
       , "import Test.QuickCheck"
       , "import System.Exit (exitFailure, exitSuccess)"
       ]
       <> importLines
       <> [ ""
          , "main :: IO ()"
          , "main = do"
          , "  rs <- sequence"
          ]
       <> zipWith (\i ln -> "    " <> (if i == 0 then "[ " else ", ") <> ln)
                  [0 :: Int ..] runLines
       <> [ "    ]"
          , "  if and rs then exitSuccess else exitFailure"
          , ""
          , "runProp :: Testable p => String -> p -> IO Bool"
          , "runProp name p = do"
          , "  res <- quickCheckWithResult stdArgs { chatty = False, maxSuccess = 200 } p"
          , "  let ok = case res of Success {} -> True; _ -> False"
          , "  putStrLn ((if ok then \"PASS  \" else \"FAIL  \") <> name)"
          , "  pure ok"
          , ""
          ]
       <> propLines

renderPropBinding :: Int -> StoredProperty -> Text
renderPropBinding i sp =
  let label = "prop_" <> T.pack (show i)
      expr  = spExpression sp
  in label <> " = " <> expr

renderRunLine :: Int -> StoredProperty -> Text
renderRunLine i sp =
  let label = "prop_" <> T.pack (show i)
      displayName = case spModule sp of
        Just m | not (T.null m) -> sanitizeLabel (m <> "_" <> label)
        _                       -> label
  in "runProp \"" <> displayName <> "\" " <> label

-- | Sanitise a label for safe embedding in a Haskell string literal.
-- Rules:
--
-- * CR / LF → space (a newline would terminate the string literal).
-- * Collapse whitespace into single underscore.
-- * Keep alnum + underscore + hyphen; replace everything else with
--   underscore.
-- * Strip leading/trailing underscores.
-- * Fallback to @property@ if the result is empty.
sanitizeLabel :: Text -> Text
sanitizeLabel raw =
  let noLines   = T.map (\c -> if c == '\n' || c == '\r' then ' ' else c) raw
      collapsed = T.intercalate "_" (T.words noLines)
      safe      = T.map (\c -> if isAlphaNum c || c == '_' || c == '-' then c else '_') collapsed
      trimmed   = T.dropAround (\c -> c == '_' || c == '-') safe
  in if T.null trimmed then "property" else trimmed

-- | Map a source path like @src/Foo/Bar.hs@ to a module name
-- @Foo.Bar@. Returns 'Nothing' when the path does not match the
-- convention (we cannot know the right import line without it).
--
-- BUG-02 fix: historically this function only stripped @src@ /
-- @lib@ as leading directories. Test-only helper modules live
-- under @test/@ (e.g. @test/Gen.hs@ containing @module Gen@) and
-- were mis-mapped to @test.Gen@ — a lowercase first segment that
-- is not a valid Haskell module name, so the generated Spec.hs
-- failed to compile. Strip @test@ too, plus every segment must
-- start with an uppercase letter; otherwise return 'Nothing' so
-- the renderer simply omits the bad import rather than emit
-- broken Haskell.
modulePathToModule :: Text -> Maybe Text
modulePathToModule raw
  | not (".hs" `T.isSuffixOf` raw) = Nothing
  | otherwise =
      let noExt  = T.dropEnd 3 raw
          parts  = T.splitOn "/" noExt
          -- drop a leading sources-dir name. 'src'/'lib' are the
          -- library conventions; 'test' covers test-suite helpers.
          -- We also drop 'app' for executable mains, though those
          -- are typically 'module Main' and do not import cleanly.
          core   = case parts of
            (p:rest) | p `elem` ["src", "lib", "test", "app"] -> rest
            _                                                  -> parts
      in if null core || not (all segmentIsUppercaseHead core)
           then Nothing
           else let dotted = T.intercalate "." core
                in if T.null dotted then Nothing else Just dotted

-- | A Haskell module-name segment must start with an uppercase
-- letter and contain only identifier-safe characters. Anything
-- else (spaces, kebab-case, lowercase dirs like @support@) is
-- rejected.
segmentIsUppercaseHead :: Text -> Bool
segmentIsUppercaseHead seg = case T.uncons seg of
  Just (c, rest) -> isAsciiUpper c && T.all idChar rest
  Nothing        -> False
  where
    idChar ch =
         isAsciiUpper ch
      || isAsciiLower ch
      || isDigit      ch
      || ch == '_' || ch == '\''

-- | Tiny helper to chain Maybe through pure functions.
(>=>) :: (a -> Maybe b) -> (b -> Maybe c) -> a -> Maybe c
f >=> g = \x -> f x >>= g
{-# INLINE (>=>) #-}

--------------------------------------------------------------------------------
-- response shaping
--------------------------------------------------------------------------------

-- | Issue #90 Phase C: rendered Spec.hs file → status='ok' with
-- the on-disk path + counts under 'result'.
successResult :: FilePath -> Int -> Int -> ToolResult
successResult path written total =
  Env.toolResponseToResult (Env.mkOk (object
    [ "output_path"        .= T.pack path
    , "properties_written" .= written
    , "total_persisted"    .= total
    , "hint"
        .= ( "Commit the generated file and add QuickCheck to the \
             \test-suite via ghc_deps. `cabal test` will replay \
             \every property as a regression gate." :: Text )
    ]))


-- | Issue #90 Phase C: 'mkModulePath' rejected the output path.
pathTraversalResult :: Text -> ToolResult
pathTraversalResult msg =
  Env.toolResponseToResult
    (Env.mkRefused (Env.mkErrorEnvelope Env.PathTraversal msg))

-- | Issue #90 Phase C: filesystem write failure.
subprocessResult :: Text -> ToolResult
subprocessResult msg =
  Env.toolResponseToResult
    (Env.mkFailed (Env.mkErrorEnvelope Env.SubprocessError msg))
