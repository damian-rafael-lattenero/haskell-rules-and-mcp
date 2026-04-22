-- | Wave-1 infrastructure for the "pure GHC API" migration.
--
-- Drives cabal via a @--with-compiler@ shim that captures the exact
-- flags cabal would pass to @ghc --interactive@. Those flags carry
-- everything the in-process GhcSession needs to compile the user's
-- project: package-dbs, package-ids, import paths, cabal-mangled
-- -this-unit-id, -fbuilding-cabal-package, etc.
--
-- For each cabal target (library, test-suite, executable,
-- benchmark) we invoke cabal once. The shim writes the @ghc@ argv
-- (null-separated so spaces in paths survive) to a unique file.
-- We parse the file and cache a 'StanzaFlags' per target.
--
-- The shim is a POSIX shell script, embedded as a Haskell string
-- literal and materialised to @.haskell-flows/shim/ghc-shim.sh@ on
-- first use. When called non-interactively (for dep compilation,
-- --numeric-version, --print-libdir, …) it delegates to @ghc@ on
-- PATH. Only the @--interactive@ call gets intercepted.
module HaskellFlows.Ghc.CabalBootstrap
  ( StanzaFlags (..)
  , Target (..)
  , bootstrapProject
  , shimScript
  ) where

import Control.Exception (SomeException, try)
import Data.Char (isAlphaNum)
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import Data.Maybe (mapMaybe)
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.IO as TIO
import System.Directory
  ( createDirectoryIfMissing
  , doesDirectoryExist
  , doesFileExist
  , listDirectory
  )
import System.Environment (getEnvironment)
import System.FilePath ((</>), takeExtension)
import qualified Data.ByteString as BS
import qualified Data.ByteString.Char8 as BS8
import qualified System.Process as Proc

import HaskellFlows.Types (ProjectDir, unProjectDir)

-- | A cabal stanza's target selector.
--
-- Serialised back to cabal CLI via 'renderTarget':
-- 'TargetLibrary'         → @lib:\<pkg\>@
-- 'TargetTestSuite n'     → @test:\<n\>@
-- 'TargetExecutable n'    → @exe:\<n\>@
-- 'TargetBenchmark n'     → @bench:\<n\>@
data Target
  = TargetLibrary
  | TargetTestSuite  !Text
  | TargetExecutable !Text
  | TargetBenchmark  !Text
  deriving stock (Eq, Ord, Show)

-- | The GHC argv cabal would have passed to @ghc --interactive@
-- for a given target.
data StanzaFlags = StanzaFlags
  { sfTarget :: !Target
  , sfArgs   :: ![String]
  }
  deriving stock (Eq, Show)

-- | POSIX shim script. Intercepts @--interactive@; delegates
-- everything else to the real ghc binary (discovered via
-- @command -v@ which searches PATH reliably under cabal's sub-env).
--
-- Output destination comes from the env var @HASKELL_FLOWS_SHIM_OUT@.
-- Each bootstrap call sets a unique value before spawning cabal so
-- per-target captures never overwrite each other.
shimScript :: String
shimScript = unlines
  [ "#!/bin/sh"
  , "# haskell-flows ghc shim — intercepts --interactive only."
  , "# Locate real ghc. 'command -v' searches PATH; 'which' fallback."
  , "REAL_GHC=\"$(command -v ghc 2>/dev/null)\""
  , "if [ -z \"$REAL_GHC\" ]; then"
  , "  REAL_GHC=\"$(which ghc 2>/dev/null)\""
  , "fi"
  , "if [ -z \"$REAL_GHC\" ]; then"
  , "  echo 'ghc-shim: real ghc not found on PATH' >&2"
  , "  exit 1"
  , "fi"
  , "for arg in \"$@\"; do"
  , "  if [ \"$arg\" = \"--interactive\" ]; then"
  , "    printf '%s\\0' \"$@\" > \"$HASKELL_FLOWS_SHIM_OUT\""
  , "    exit 0"
  , "  fi"
  , "done"
  , "exec \"$REAL_GHC\" \"$@\""
  ]

-- | Write the shim to @.haskell-flows/shim/ghc-shim.sh@ under the
-- project dir, chmod +x, return its absolute path. Idempotent.
installShim :: FilePath -> IO FilePath
installShim projectRoot = do
  let shimDir  = projectRoot </> ".haskell-flows" </> "shim"
      shimPath = shimDir </> "ghc-shim.sh"
  createDirectoryIfMissing True shimDir
  present <- doesFileExist shimPath
  if present
    then pure shimPath
    else do
      -- Write + chmod. Using writeFile + the shebang is enough on
      -- POSIX; file permissions default-u+r, so we also chmod +x.
      writeFile shimPath shimScript
      _ <- Proc.readCreateProcessWithExitCode
             (Proc.proc "chmod" ["+x", shimPath]) ""
      pure shimPath

-- | Detect every target in the project's .cabal file. Uses a tiny
-- line-based parser — no dependency on Cabal the library.
--
-- Recognises:
--   @library@                  → 'TargetLibrary'
--   @test-suite NAME@          → 'TargetTestSuite' NAME
--   @executable NAME@          → 'TargetExecutable' NAME
--   @benchmark NAME@           → 'TargetBenchmark' NAME
--
-- Non-stanza lines are ignored.
detectTargets :: FilePath -> IO [Target]
detectTargets projectRoot = do
  cabalFile <- findCabalFile projectRoot
  case cabalFile of
    Nothing -> pure []
    Just fp -> do
      body <- TIO.readFile fp
      pure (parseTargetsFromCabal body)

parseTargetsFromCabal :: Text -> [Target]
parseTargetsFromCabal = mapMaybe (classify . T.stripStart) . T.lines
  where
    classify ln
      | "library" == lower = Just TargetLibrary
      | "test-suite " `T.isPrefixOf` lower =
          Just (TargetTestSuite (firstWord (T.drop (T.length "test-suite ") lower)))
      | "executable " `T.isPrefixOf` lower =
          Just (TargetExecutable (firstWord (T.drop (T.length "executable ") lower)))
      | "benchmark " `T.isPrefixOf` lower =
          Just (TargetBenchmark (firstWord (T.drop (T.length "benchmark ") lower)))
      | otherwise = Nothing
      where
        lower = T.toLower (T.strip ln)
    firstWord = T.takeWhile (\c -> isAlphaNum c || c == '-' || c == '_')

findCabalFile :: FilePath -> IO (Maybe FilePath)
findCabalFile projectRoot = do
  exists <- doesDirectoryExist projectRoot
  if not exists then pure Nothing
  else do
    entries <- listDirectory projectRoot
    pure $ case [projectRoot </> e | e <- entries, takeExtension e == ".cabal"] of
      (f : _) -> Just f
      []      -> Nothing

-- | Serialise a 'Target' into the argv cabal accepts for v2-repl.
-- 'TargetLibrary' intentionally produces an EMPTY list — cabal's
-- v2-repl defaults to the library when no target is given, and
-- accepting "lib:all" fails for projects with a cabal.project that
-- doesn't explicitly name the package (@Our failure to do so is a
-- bug in cabal@ per issue #8684).
renderTarget :: Target -> [String]
renderTarget = \case
  TargetLibrary      -> []
  TargetTestSuite n  -> ["test:"  <> T.unpack n]
  TargetExecutable n -> ["exe:"   <> T.unpack n]
  TargetBenchmark n  -> ["bench:" <> T.unpack n]

-- | Bootstrap all targets in the project. Runs cabal once per
-- target with the shim as compiler. Targets that fail silently
-- (cabal fails, shim never gets the --interactive call) are dropped
-- — better to have a partial map than abort the whole bootstrap.
bootstrapProject :: ProjectDir -> IO (Map Target StanzaFlags)
bootstrapProject pd = do
  let root = unProjectDir pd
  targets <- detectTargets root
  shimPath <- installShim root
  pairs <- traverse (bootstrapOne root shimPath) targets
  pure (Map.fromList [(t, sf) | (t, Just sf) <- zip targets pairs])

bootstrapOne :: FilePath -> FilePath -> Target -> IO (Maybe StanzaFlags)
bootstrapOne root shimPath tgt = do
  let outDir  = root </> ".haskell-flows" </> "flags"
      outFile = outDir </> targetFileName tgt
  createDirectoryIfMissing True outDir
  -- Start with an empty file; a failed bootstrap leaves it empty
  -- and we skip that target. BS.writeFile is strict (no lingering
  -- handle) — critical on macOS where lazy readFile from a stale
  -- handle produces "resource busy" on the next open.
  BS.writeFile outFile BS.empty
  let cp = (Proc.proc "cabal"
             ( ["v2-repl"]
               <> renderTarget tgt
               <> ["--with-compiler=" <> shimPath]
             ))
             { Proc.cwd     = Just root
             , Proc.env     = Nothing  -- inherit parent env
             , Proc.std_in  = Proc.NoStream
             , Proc.std_out = Proc.CreatePipe
             , Proc.std_err = Proc.CreatePipe
             }
  -- Merge our env var into the inherited env so the shim sees it
  -- and PATH etc. stay intact for the delegation fallthrough.
  parentEnv <- getEnvironment
  let cpFinal = cp { Proc.env = Just (("HASKELL_FLOWS_SHIM_OUT", outFile) : parentEnv) }
  _ <- try @SomeException (Proc.readCreateProcess cpFinal "")
  -- Strict read so no handle outlives this call. Translate bytes
  -- back to String for the null-separated parser.
  contents <- try @SomeException (BS8.unpack <$> BS.readFile outFile)
  case contents :: Either SomeException String of
    Right bs | not (null bs) -> pure (Just (StanzaFlags tgt (parseNullSep bs)))
    _                        -> pure Nothing

-- | Split a null-separated string into its parts. Trailing nulls
-- produce an empty final element which we drop.
parseNullSep :: String -> [String]
parseNullSep s = case break (== '\0') s of
  (chunk, [])       -> [chunk | not (null chunk)]
  (chunk, _ : rest) -> chunk : parseNullSep rest

targetFileName :: Target -> FilePath
targetFileName = \case
  TargetLibrary      -> "lib.args"
  TargetTestSuite n  -> "test-" <> T.unpack n <> ".args"
  TargetExecutable n -> "exe-"  <> T.unpack n <> ".args"
  TargetBenchmark n  -> "bench-" <> T.unpack n <> ".args"
