-- | ghcide-backed GHC session — the F1 strangler backend.
--
-- Boots a ghcide 'IdeState' in-process (no LSP) through
-- 'Development.IDE.Main.defaultMain' with a @Custom@ 'IdeCommand',
-- forked so the MCP stdio transport keeps running. Tool handlers reach
-- the state via 'runAction' — the same embedding HLS plugins use.
--
-- Component resolution (library vs test-suite package envs) comes from
-- hie-bios via an explicit @hie.yaml@. F0 finding: without one, the
-- implicit cradle degrades to a base-only direct cradle with empty
-- cradle-error diagnostics. 'ensureHieYaml' synthesizes the file from
-- the project's .cabal when missing.
--
-- Verified API recipes (docs/ghcide-spike-F0.md):
--   * 'GhcSessionDeps' (not 'GhcSession') for interactive eval/type.
--   * 'evalGhcEnv' as the runner; 'setContext' needs an explicit
--     Prelude IIDecl.
--   * 'getDiagnostics' is STM.
module HaskellFlows.Ghc.IdeSession
  ( IdeSession (..)
  , BackendChoice (..)
  , backendFromEnv
  , bootIdeSession
  , ideTypecheckFile
  , ideEvalExprIn
  , ideTypeOfExprIn
  , anchorModuleIn
  , hieYamlFromCabal
  , ensureHieYaml
  ) where

import Control.Concurrent (forkIO, threadDelay)
import Control.Concurrent.MVar (MVar, newEmptyMVar, putMVar, takeMVar)
import Control.Concurrent.STM (atomically)
import Control.Exception (SomeException, evaluate, try)
import Control.Monad (forever, forM)
import Control.Monad.IO.Class (liftIO)
import Data.Aeson (Value, object, (.=))
import Data.Char (isSpace, toLower)
import Data.List (find, sort)
import Data.Maybe (fromMaybe, isNothing, listToMaybe, mapMaybe)
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.IO as TIO
import Development.IDE (runAction, use)
import Development.IDE.Core.RuleTypes (GhcSessionDeps (..), TypeCheck (..))
import Development.IDE.Core.Shake (IdeState, getDiagnostics)
import Development.IDE.GHC.Util (evalGhcEnv, printOutputable)
import Development.IDE.Main (Arguments (..), Command (..), IdeCommand (..), Log, defaultArguments, defaultMain)
import Development.IDE.Types.Diagnostics (DiagnosticSeverity (..), FileDiagnostic (..), _message, _severity)
import Development.IDE.Types.HscEnvEq (HscEnvEq (hscEnv))
import Development.IDE.Types.Location (toNormalizedFilePath')
import GHC
  ( InteractiveImport (IIDecl)
  , TcRnExprMode (TM_Inst)
  , compileExpr
  , exprType
  , mkModuleName
  , setContext
  , simpleImportDecl
  )
import Ide.Logger (Doc, Recorder, WithPriority, cmapWithPrio, makeDefaultStderrRecorder, pretty)
import Ide.Types (IdePlugins)
import System.Directory (doesDirectoryExist, doesFileExist, listDirectory, makeAbsolute)
import System.Environment (lookupEnv)
import System.FilePath ((</>), takeExtension)
import System.Posix.IO (dup, dupTo, stdOutput)
import System.Timeout (timeout)
import Unsafe.Coerce (unsafeCoerce)

import HaskellFlows.Types (ProjectDir, unProjectDir)

-- | Which GHC backend serves tool calls.
data BackendChoice
  = BackendGhcApi
    -- ^ Default — the existing in-process 'HaskellFlows.Ghc.ApiSession'.
  | BackendGhcide
    -- ^ Experimental F1 backend — ghcide 'IdeState' per project.
  deriving stock (Eq, Show)

-- | Read @HASKELL_FLOWS_BACKEND@ (@ghcide@ / @ghcapi@). Anything
-- unrecognized falls back to 'BackendGhcApi' — master must stay green
-- for every existing consumer.
backendFromEnv :: IO BackendChoice
backendFromEnv = do
  mv <- lookupEnv "HASKELL_FLOWS_BACKEND"
  pure $ case trim <$> mv of
    Just v | map toLower v == "ghcide" -> BackendGhcide
    _ -> BackendGhcApi
  where
    trim = f . f where f = dropWhile isSpace . reverse

-- | A live ghcide session anchored at a project directory.
data IdeSession = IdeSession
  { isState :: !IdeState
  , isRoot  :: !FilePath
  }

-- | Boot a ghcide IdeState for the project. Forks 'defaultMain' with a
-- blocking Custom command so the state stays alive; the boot itself is
-- budgeted (cradle discovery + first session setup are not free).
--
-- F1 finding: ghcide's 'defaultMain' hijacks the process stdout
-- (reserves fd 1 for the LSP protocol it thinks it owns and redirects
-- prior stdout to stderr). The MCP's stdio transport lives on fd 1, so
-- we duplicate it before the fork and restore it after the state is
-- handed over — otherwise every tool response after the first ghcide
-- boot silently lands on stderr and the client times out.
bootIdeSession :: ProjectDir -> IO IdeSession
bootIdeSession pd = do
  root <- makeAbsolute (unProjectDir pd)
  _ <- ensureHieYaml root
  stateVar <- newEmptyMVar
  recorderDoc <- makeDefaultStderrRecorder Nothing
  let recorder :: Recorder (WithPriority Log)
      recorder = cmapWithPrio (pretty :: Log -> Doc ()) recorderDoc
      plugins :: IdePlugins IdeState
      plugins = mempty
      args =
        (defaultArguments recorder root plugins)
          { argCommand = Custom (IdeCommand (\ide -> putMVar stateVar ide >> hang)) }
  savedStdout <- dup stdOutput
  _ <- forkIO $ do
    r <- try (defaultMain recorder args) :: IO (Either SomeException ())
    case r of
      Left e -> putStrLn ("[haskell-flows] ghcide defaultMain died: " ++ show e)
      _ -> pure ()
  mstate <- timeout 180_000_000 (takeMVar stateVar)
  case mstate of
    Nothing -> fail "ghcide IdeState boot timed out after 180s"
    Just st -> do
      _ <- dupTo savedStdout stdOutput
      pure (IdeSession st root)
  where
    hang = forever (threadDelay 3_600_000_000)

-- | Force a typecheck of one file and return the error-severity
-- diagnostics for it as envelope-friendly JSON objects.
ideTypecheckFile :: IdeSession -> FilePath -> IO [Value]
ideTypecheckFile s fp = do
  absF <- makeAbsolute fp
  let nfp = toNormalizedFilePath' absF
  _ <- runAction "mcp-ide-check" (isState s) (use TypeCheck nfp)
  diags <- atomically (getDiagnostics (isState s))
  pure
    [ object ["message" .= T.unpack (_message d)]
    | fd <- diags
    , fdFilePath fd == nfp
    , let d = fdLspDiagnostic fd
    , _severity d == Just DiagnosticSeverity_Error
    ]

-- | Compile and run a String-valued expression in the context of the
-- component owning the anchor file, with extra interactive imports
-- (Prelude is always added — F0 finding). 30s wall budget.
ideEvalExprIn :: IdeSession -> FilePath -> [Text] -> Text -> IO (Either Text Text)
ideEvalExprIn s anchor extraImports expr = do
  mEq <- sessionDepsFor s anchor
  case mEq of
    Nothing -> pure (Left "could not resolve GHC session for anchor module")
    Just eq -> fmap joinTimeout (timeout 30_000_000 (tryRun eq))
  where
    joinTimeout Nothing = Left "timeout after 30s"
    joinTimeout (Just (Left e)) = Left ("EXC: " <> T.pack (show e))
    joinTimeout (Just (Right v)) = Right v
    tryRun eq = do
      r <- try (evalGhcEnv (hscEnv eq) go) :: IO (Either SomeException String)
      pure (fmap T.pack r)
    go = do
      setContext
        (map (IIDecl . simpleImportDecl . mkModuleName . T.unpack)
             (extraImports ++ ["Prelude"]))
      hv <- compileExpr (T.unpack expr)
      -- F30: the String comes back as a thunk — without forcing it here
      -- the expression would only run when the envelope serializes it,
      -- outside the 30s budget. Force the full render inside the window.
      let s = unsafeCoerce hv :: String
      _ <- liftIO (evaluate (length s))
      pure s

-- | Type of an expression in the component context of the anchor file.
ideTypeOfExprIn :: IdeSession -> FilePath -> Text -> IO (Either Text Text)
ideTypeOfExprIn s anchor expr = do
  mEq <- sessionDepsFor s anchor
  case mEq of
    Nothing -> pure (Left "could not resolve GHC session for anchor module")
    Just eq -> fmap joinTimeout (timeout 30_000_000 (tryRun eq))
  where
    joinTimeout Nothing = Left "timeout after 30s"
    joinTimeout (Just (Left e)) = Left ("EXC: " <> T.pack (show e))
    joinTimeout (Just (Right v)) = Right v
    tryRun eq = do
      r <- try (evalGhcEnv (hscEnv eq) go) :: IO (Either SomeException Text)
      pure r
    go = do
      setContext [IIDecl (simpleImportDecl (mkModuleName "Prelude"))]
      ty <- exprType TM_Inst (T.unpack expr)
      pure (printOutputable ty)

-- | 'GhcSessionDeps' for the component owning the file (F0: GhcSession
-- alone dies with lookupFinderCache on interactive contexts).
sessionDepsFor :: IdeSession -> FilePath -> IO (Maybe HscEnvEq)
sessionDepsFor s fp = do
  absF <- makeAbsolute fp
  runAction "mcp-ide-session" (isState s)
    (use GhcSessionDeps (toNormalizedFilePath' absF))

-- | First .hs file under the project's src/ (fallback: test/) — the
-- default anchor when a tool call carries no module argument.
anchorModuleIn :: IdeSession -> IO FilePath
anchorModuleIn s = do
  found <- forM [root </> "src", root </> "test"] firstHsUnder
  pure (fromMaybe root (listToMaybe (mapMaybe id found)))
  where
    root = isRoot s
    firstHsUnder dir = do
      ok <- doesDirectoryExist dir
      if not ok
        then pure Nothing
        else do
          entries <- sort <$> listDirectory dir
          pure (listToMaybe [dir </> e | e <- entries, takeExtension e == ".hs"])

-- | A cabal stanza we can map to a hie.yaml component.
data Stanza = LibStanza | TestStanza !Text
  deriving stock (Eq, Show)

-- | Render a @hie.yaml@ from .cabal content: one @- path/component@
-- pair per library and test-suite stanza. Two passes: split the file
-- into stanzas, then extract each stanza's hs-source-dirs (or the
-- conventional default). Pure — unit-tested.
hieYamlFromCabal :: Text -> Either String Text
hieYamlFromCabal cabal = do
  pkg <- packageNameOf sigLines
  let stanzas = splitStanzas sigLines
  if null stanzas
    then Left "no library or test-suite stanza found"
    else
      Right $
        T.unlines $
          [ "cradle:"
          , "  cabal:"
          ]
            <> concatMap (renderEntry pkg) stanzas
  where
    sigLines =
      [ T.strip t
      | t <- T.lines cabal
      , not (T.null (T.strip t))
      , not ("--" `T.isPrefixOf` T.strip t)
      ]
    renderEntry pkg (LibStanza, body) =
      entry (sourceDirOf "src" body) ("lib:" <> pkg)
    renderEntry _ (TestStanza n, body) =
      entry (sourceDirOf "test" body) ("test:" <> n)
    entry dir comp =
      [ "    - path: \"./" <> dir <> "\""
      , "      component: \"" <> comp <> "\""
      ]

-- | Significant lines, split into (stanza, body-lines) pairs. Lines
-- before the first stanza header (name:, version:, …) are skipped.
splitStanzas :: [Text] -> [(Stanza, [Text])]
splitStanzas [] = []
splitStanzas (t : rest) = case stanzaHeaderOf t of
  Just st ->
    let (body, rest') = span (isNothing . stanzaHeaderOf) rest
    in (st, body) : splitStanzas rest'
  Nothing -> splitStanzas rest

-- | Top-level (column-0) stanza headers only.
stanzaHeaderOf :: Text -> Maybe Stanza
stanzaHeaderOf t
  | not (T.null t)
  , not (isSpace (T.head t)) =
      if t == "library"
        then Just LibStanza
        else case T.stripPrefix "test-suite " t of
          Just rest
            | not (T.null (T.strip rest)) ->
                Just (TestStanza (T.takeWhile (/= ' ') (T.strip rest)))
          _ -> Nothing
  | otherwise = Nothing

packageNameOf :: [Text] -> Either String Text
packageNameOf ls =
  case mapMaybe pick ls of
    (n : _) -> Right n
    [] -> Left "no 'name:' field in .cabal"
  where
    pick t =
      let (k, v) = fieldSplit t
      in if T.toLower (T.strip k) == "name" && not (T.null v)
           then Just v
           else Nothing

-- | Break a @field: value@ line, dropping the separator colon and
-- trimming both sides.
fieldSplit :: Text -> (Text, Text)
fieldSplit t =
  let (k, v) = T.break (== ':') t
  in (T.strip k, T.strip (T.drop 1 v))

-- | First comma-separated value of the stanza's hs-source-dirs, or the
-- conventional default when the stanza omits the field.
sourceDirOf :: Text -> [Text] -> Text
sourceDirOf def body =
  case mapMaybe pick body of
    (d : _) -> d
    [] -> def
  where
    pick t =
      let (k, v) = fieldSplit t
      in if T.toLower k == "hs-source-dirs"
           then let first = T.takeWhile (/= ',') v
                in if T.null first then Nothing else Just first
           else Nothing

-- | Write a synthesized @hie.yaml@ when the project has none. Returns
-- True when the file was written.
ensureHieYaml :: FilePath -> IO Bool
ensureHieYaml root = do
  let hiePath = root </> "hie.yaml"
  exists <- doesFileExist hiePath
  if exists
    then pure False
    else do
      entries <- listDirectory root
      case find ((== ".cabal") . takeExtension) (sort entries) of
        Nothing -> pure False
        Just cabalFile -> do
          content <- TIO.readFile (root </> cabalFile)
          case hieYamlFromCabal content of
            Left _ -> pure False
            Right yaml -> do
              TIO.writeFile hiePath yaml
              pure True
