-- | ghcide-backed GHC session — the F1 strangler backend.
--
-- Boots a ghcide 'IdeState' in-process (no LSP) through
-- 'Development.IDE.Main.defaultMain' with a @Custom@ 'IdeCommand',
-- forked so the MCP stdio transport keeps running. Tool handlers reach
-- the state via 'runAction' — the same embedding HLS plugins use.
--
-- Component resolution comes from hie-bios implicit cabal discovery
-- (no hie.yaml): the session runs with cwd = project root and all
-- paths canonicalized, matching how the editor launches HLS. F3
-- finding: generated CabalMulti cradles break multi-component
-- targets ("No cradle target found") — never synthesize one.
--
-- Verified API recipes (docs/ghcide-spike-F0.md):
--   * 'GhcSessionDeps' (not 'GhcSession') for interactive eval/type.
--   * 'evalGhcEnv' as the runner; 'setContext' needs an explicit
--     Prelude IIDecl.
--   * 'getDiagnostics' is STM.
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE TypeFamilies #-}

module HaskellFlows.Ghc.IdeSession
  ( IdeSession (..)
  , BackendChoice (..)
  , backendFromEnv
  , bootIdeSession
  , ideTypecheckFile
  , ideEvalExprIn
  , ideTypeOfExprIn
  , anchorModuleIn
    -- * F3: project-wide diagnostics + module inventory
  , EvalArgs (..)
  , ideDiagnosticsFor
  , ideModuleNameOf
  , ideProjectDiagnostics
  , projectModuleFilesFromCabal
  , ideInteractiveEnvFor
  ) where

import Control.Concurrent (forkIO, threadDelay)
import Control.Concurrent.MVar (MVar, newEmptyMVar, newMVar, putMVar, takeMVar, withMVar)
import Control.Concurrent.STM (atomically)
import Control.Exception (SomeException, evaluate, try)
import Control.Monad (forever, forM, void, when)
import Control.Monad.IO.Class (liftIO)
import Data.Aeson (Value, object, (.=))
import Data.Char (isSpace, toLower)
import Data.List (find, sort)
import Data.Maybe (fromMaybe, isNothing, listToMaybe, mapMaybe)
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.IO as TIO
import Control.DeepSeq (NFData (rnf))
import Data.HashSet (HashSet)
import Data.Typeable (Typeable)
import GHC.Generics (Generic)
import qualified Data.HashSet as Set
import Data.IORef (IORef, atomicModifyIORef', newIORef, readIORef)
import qualified Data.ByteString as BS
import Development.IDE (LinkableType (BCOLinkable)
  , NeedsCompilation (NeedsCompilation)
  , NormalizedFilePath
  , RuleBody (RuleNoDiagnostics, RuleWithCustomNewnessCheck)
  , Rules
  , defineEarlyCutoff
  , encodeLinkableType
  , runAction, use, use_, useNoFile_, uses_
  )
import Development.IDE.Core.FileStore (setSomethingModified)
import Development.IDE.Core.OfInterest (FileOfInterestStatus (OnDisk), addFileOfInterest)
import Development.IDE.Core.Shake (VFSModified (VFSUnmodified))
import Development.IDE.Types.Shake (toKey)

import Development.IDE.Core.Shake
  ( IsIdeGlobal, addIdeGlobal, getIdeGlobalAction, getIdeGlobalState )
import qualified Development.IDE.Core.Rules as NS (needsCompilationRule)
import Data.Hashable (Hashable (hashWithSalt))
import Development.IDE.Graph (RuleResult)
import Development.IDE.Graph (alwaysRerun)
import Development.IDE.Core.RuleTypes
  ( pattern GetModificationTime
  , GetLinkable (GetLinkable)
  , GetCoreFileHash (GetCoreFileHash)
  , IsFileOfInterest (IsFileOfInterest)
  , IsFileOfInterestResult (..)
  , GetModSummary (GetModSummary)
  , GetModuleGraph (GetModuleGraph)
  , GhcSessionDeps (GhcSessionDeps)
  , LinkableResult (linkableHomeMod)
  , ModSummaryResult (msrModSummary)
  , TypeCheck (TypeCheck)
  , tmrTypechecked
  )
import Development.IDE.Import.DependencyInformation
  ( transitiveDeps, transitiveModuleDeps )
import Development.IDE.Core.RuleTypes (GhcSessionDeps (..), TypeCheck (..))
import Development.IDE.Core.Shake (IdeState, getDiagnostics)
import Development.IDE.GHC.Util (evalGhcEnv, modifyDynFlags, printOutputable)


import Data.Default (Default (def))
-- (data-default via ghcide deps)
import Development.IDE.Main (Arguments (..), Command (..), IdeCommand (..), Log (LogShake), defaultArguments, defaultMain)
import Development.IDE.Plugin (Plugin (..))
import Ide.Plugin.Config (Config)
import Control.Exception (displayException)
import System.IO (stderr)
import Development.IDE.Types.Diagnostics (DiagnosticSeverity (..), FileDiagnostic (..), _message, _range, _severity)
import Development.IDE.Types.HscEnvEq (HscEnvEq (hscEnv))
import Development.IDE.Types.HscEnvEq (HscEnvEq (hscEnv))
import Development.IDE.Types.Location (Position (..), Range (..), toNormalizedFilePath')
import GHC
  ( DynFlags (..)
  , Ghc
  , HscEnv
  , InteractiveImport (IIModule, IIDecl)
  , TcRnExprMode (TM_Inst)
  , compileExpr
  , exprType
  , getSession
  , getSessionDynFlags
  , moduleName
  , ms_hspp_opts
  , ms_mod
  , mkModuleName
  , setContext
  , setSessionDynFlags
  , simpleImportDecl
  )
import Development.IDE.GHC.Compat
  ( Extension (ExtendedDefaultRules, MonomorphismRestriction)
  , loadModulesHome
  )
import GHC.Driver.Backend (interpreterBackend)
import GHC.Iface.Syntax
  ( ImpIfaceList (ImpIfaceAll, ImpIfaceExplicit, ImpIfaceEverythingBut)
  , IfaceImport (IfaceImport)
  )
import GHC.Tc.Types
  ( ImportUserSpec (ImpUserSpec)
  , ImpUserList (ImpUserAll, ImpUserExplicit, ImpUserEverythingBut)
  , tcg_import_decls
  , tcg_rdr_env
  )
import GHC.Types.Name.Reader (forceGlobalRdrEnv, globalRdrEnvLocal)
import GHC.Unit.Home.ModInfo (HomeModInfo (hm_iface))
import GHC.Unit.Module.ModIface
  ( IfaceTopEnv (IfaceTopEnv)
  , ModIface_ (mi_module)
  , set_mi_top_env
  )
import GHC.Data.Bool (OverridingBool (Never))
import qualified Development.IDE.GHC.Compat.Units as CUnits (unitState)
import GHC.Data.FastString (mkFastString)
import GHC.Unit.Types (GenUnit (RealUnit), UnitId, Definite (Definite))
import GHC.Unit.Info (PackageName (PackageName))
import GHC.Unit.State (lookupPackageName)
import Language.Haskell.Syntax.Module.Name (moduleNameString)
import qualified GHC.Data.EnumSet as EnumSet
import GHC.Driver.DynFlags
  ( ModRenaming (ModRenaming)
  , PackageArg (PackageArg, UnitIdArg)
  , PackageFlag (ExposePackage)
  )

import Ide.Logger (Doc, Recorder, WithPriority, cmapWithPrio, makeDefaultStderrRecorder, pretty)
import Ide.Types (IdePlugins)
import System.Directory (doesDirectoryExist, doesFileExist, listDirectory, makeAbsolute, setCurrentDirectory)
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

-- | Read @HASKELL_FLOWS_BACKEND@ (@ghcide@ default / @ghcapi@
-- legacy escape hatch). The ghcide engine is the default since the
-- F3 flip: legacy stays selectable for A/B debugging during the
--ApiSession decommission, not for production use.
backendFromEnv :: IO BackendChoice
backendFromEnv = do
  mv <- lookupEnv "HASKELL_FLOWS_BACKEND"
  pure $ case trim <$> mv of
    Just v | map toLower v == "ghcapi" -> BackendGhcApi
    _ -> BackendGhcide
  where
    trim = f . f where f = dropWhile isSpace . reverse

-- | A live ghcide session anchored at a project directory.
data IdeSession = IdeSession
  { isState    :: !IdeState
  , isRoot     :: !FilePath
  , isEvalLock :: !(MVar ())
    -- ^ eval/type/property run serialized against the shared HscEnvEq:
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
  -- Official HLS behavior: implicit cradle discovery (no hie.yaml)
  -- anchors at the PROCESS cwd — the editor launches HLS from the
  -- workspace root. We must do the same, or hie-bios resolves the
  -- multi cradle against the server's install dir and every file
  -- lands in "No prefixes matched".
  setCurrentDirectory root
  stateVar <- newEmptyMVar
  recorderDoc <- makeDefaultStderrRecorder Nothing
  let recorder :: Recorder (WithPriority Log)
      recorder = cmapWithPrio (pretty :: Log -> Doc ()) recorderDoc
      plugins :: IdePlugins IdeState
      plugins = mempty
      -- defaultMain composes rules as: argsRules >> kick >>
      -- pluginRules plugins, where plugins = hlsPlugin <> argsGhcidePlugin
      -- (Main.hs). hls-graph's addRule is Map.insert: LAST wins. So
      -- plugin-channel rules override argsRules ones — exactly how
      -- the hls-eval-plugin's NeedsCompilation redefinition wins in
      -- real HLS. Our override rides the (deprecated but final)
      -- argsGhcidePlugin channel.
      args =
        (defaultArguments recorder root plugins)
          { argCommand = Custom (IdeCommand (\ide -> putMVar stateVar ide >> hang))
          , argsGhcidePlugin =
              (def :: Plugin Config)
                { pluginRules = evalQueueRules recorder }
          }
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
      lock <- newMVar ()
      pure (IdeSession st root lock)
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

-- | F3: every diagnostic (errors AND warnings) for one file, as
-- structured values with the GHC code split out of the message and
-- the range flattened. Same rule run as 'ideTypecheckFile' — the
-- severity filter is the caller's business.
-- | The TRUE module name of a source file, from its ModSummary —
-- path-derived guesses ("src.Expr") do not resolve in IIDecls.
ideModuleNameOf :: IdeSession -> FilePath -> IO (Maybe String)
ideModuleNameOf s fp =
  withMVar (isEvalLock s) $ \_ -> do
    absF <- makeAbsolute fp
    runAction "mcp-ide-modname" (isState s)
      (fmap moduleNameString
         . fmap moduleName
         . fmap ms_mod
         . fmap msrModSummary
         <$> use GetModSummary (toNormalizedFilePath' absF))

-- | didSave-equivalent for headless runs: the client edits files on
-- disk between tool calls, and without an LSP client nothing tells
-- the engine to re-stat them. Records the mtime key as dirty and
-- restarts the session — exactly what Notifications.hs does on
-- TextDocumentDidSave (setFileModified). Must run under isEvalLock.
rescanForDiskChanges :: IdeSession -> NormalizedFilePath -> IO ()
rescanForDiskChanges s nfp =
  setSomethingModified VFSUnmodified (isState s) "mcp-external-edit" $
    pure [toKey GetModificationTime nfp]

ideDiagnosticsFor :: IdeSession -> FilePath -> IO [Value]
ideDiagnosticsFor s fp = withMVar (isEvalLock s) $ \_ -> do
  absF <- makeAbsolute fp
  rescanForDiskChanges s (toNormalizedFilePath' absF)
  ideDiagnosticsFor' s fp

ideDiagnosticsFor' :: IdeSession -> FilePath -> IO [Value]
ideDiagnosticsFor' s fp = do
  absF <- makeAbsolute fp
  let nfp = toNormalizedFilePath' absF
  _ <- runAction "mcp-ide-check" (isState s) (use TypeCheck nfp)
  diags <- atomically (getDiagnostics (isState s))
  pure
    [ object
        [ "severity" .= (if _severity d == Just DiagnosticSeverity_Error
                           then ("error" :: Text) else ("warning" :: Text))
        , "code"     .= diagCode (_message d)
        , "message"  .= T.unpack (_message d)
        , "file"     .= fp
        , "line"     .= maybe 0 fst (rangeToLineCol (_range d))
        , "column"   .= maybe 0 snd (rangeToLineCol (_range d))
        ]
    | fd <- diags
    , fdFilePath fd == nfp
    , let d = fdLspDiagnostic fd
    ]

-- | @[GHC-xxxxx]@ code embedded in a diagnostic message, when present.
diagCode :: Text -> Text
diagCode msg =
  let ws = T.words msg
  in case [w | w <- ws, "[GHC-" `T.isPrefixOf` w, "]" `T.isSuffixOf` w] of
       (c : _) -> T.init (T.drop 1 c)   -- GHC-xxxxx (no brackets)
       []      -> ""

-- | (line, column) of a diagnostic's start, 0-based LSP positions.
rangeToLineCol :: Range -> Maybe (Int, Int)
rangeToLineCol r =
  let Position { _line, _character } = _start r
  in Just (fromIntegral _line, fromIntegral _character)

-- | F3: typecheck every module listed in the project's .cabal and
-- return per-file diagnostics (errors AND warnings). Sequential —
-- the Shake graph dedupes shared deps anyway.
ideProjectDiagnostics :: IdeSession -> [FilePath] -> IO [(FilePath, [Value])]
ideProjectDiagnostics s fs =
  withMVar (isEvalLock s) $ \_ ->
    mapM (\fp -> (,) fp <$> ideDiagnosticsFor' s fp) fs

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


-- | F3: relative module paths (library + test stanzas) parsed from
-- .cabal content. Module names map to @dir/Mod/Sub.hs@ under each
-- stanza's hs-source-dirs.
projectModuleFilesFromCabal :: Text -> [FilePath]
projectModuleFilesFromCabal cabal =
  concatMap stanzaModules (splitStanzas sigLines)
  where
    sigLines =
      [ T.strip t
      | t <- T.lines cabal
      , not (T.null (T.strip t))
      , not ("--" `T.isPrefixOf` T.strip t)
      ]
    stanzaModules (LibStanza, body)  = mk "src" body
    stanzaModules (TestStanza _, body) = mk "test" body
    mk def body =
      let dir = T.unpack (sourceDirOf (T.pack def) body)
      in map (moduleToPath dir) (listFieldOf "exposed-modules" body
                              <> listFieldOf "other-modules" body)
    moduleToPath dir name =
      dir <> "/" <> map (\c -> if c == '.' then '/' else c) (T.unpack name) <> ".hs"

-- | All values of a comma-separated list field, joining
-- continuation lines (@\  , Foo@) to the header line.
listFieldOf :: Text -> [Text] -> [Text]
listFieldOf name body =
  case dropWhile (not . isField name) body of
    [] -> []
    (h : rest) ->
      let first = T.strip (snd (fieldSplit h))
          conts = map (T.strip . T.dropWhile (== ','))
                    (takeWhile isCont rest)
          items = filter (not . T.null) (first : conts)
      in items
  where
    isField n t = T.toLower (T.strip (fst (fieldSplit t))) == n
    isCont t = "," `T.isPrefixOf` T.strip t

-- | Compile and run a String-valued expression in the context of the
-- component owning the anchor file, with extra interactive imports
-- (Prelude is always added — F0 finding). 30s wall budget.
-- | Arguments for interactive evaluation against a project module.
data EvalArgs = EvalArgs
  { eaAnchor     :: FilePath
    -- ^ The module that will be IN SCOPE (IIModule) during eval.
  , eaImports    :: [Text]
    -- ^ Extra 'IIDecl' imports (e.g. System.IO.Unsafe).
  , eaQuickCheck :: Bool
    -- ^ Expose the QuickCheck package via dynflags.
  }

-- | Serialized: the underlying HscEnv carries mutable interactive
-- state (context, EPS) — concurrent setContext/compileExpr corrupts
-- it (F3: the concurrent first-burst regression).
ideEvalExprIn :: IdeSession -> EvalArgs -> Text -> IO (Either Text Text)
ideEvalExprIn s ea expr =
  withMVar (isEvalLock s) $ \_ -> ideEvalExprIn' s ea expr

ideEvalExprIn' :: IdeSession -> EvalArgs -> Text -> IO (Either Text Text)
ideEvalExprIn' s ea expr = do
  nfp0 <- toNormalizedFilePath' <$> makeAbsolute (eaAnchor ea)
  scopeAnchorForEval s nfp0
  menv <- ideInteractiveEnvFor s nfp0 (eaQuickCheck ea)
  case menv of
    Left e   -> pure (Left e)
    Right (mn, hsc) -> fmap joinTimeout (timeout 30_000_000 (tryRun mn hsc))
  where
    joinTimeout Nothing = Left "timeout after 30s"
    joinTimeout (Just (Left e)) = Left ("EXC: " <> T.pack (show e))
    joinTimeout (Just (Right v)) = Right v
    tryRun mn hsc = do
      r <- try (evalGhcEnv hsc (go mn)) :: IO (Either SomeException String)
      pure (fmap T.pack r)
    go mn = do
      setContext (IIModule (mkModuleName mn) : mkContext (eaImports ea))
      hv <- compileExpr (T.unpack expr)
      -- F30: force the full render inside the budget window.
      let str = unsafeCoerce hv :: String
      _ <- liftIO (evaluate (length str))
      pure str

-- | Type of an expression in the interactive context of the anchor
-- module (see 'ideInteractiveEnvFor').
ideTypeOfExprIn :: IdeSession -> EvalArgs -> Text -> IO (Either Text Text)
ideTypeOfExprIn s ea expr = do
  nfp0 <- toNormalizedFilePath' <$> makeAbsolute (eaAnchor ea)
  scopeAnchorForEval s nfp0
  menv <- ideInteractiveEnvFor s nfp0 False
  case menv of
    Left e   -> pure (Left e)
    Right (mn, hsc) -> fmap joinTimeout (timeout 30_000_000 (tryRun mn hsc))
  where
    joinTimeout Nothing = Left "timeout after 30s"
    joinTimeout (Just (Left e)) = Left ("EXC: " <> T.pack (show e))
    joinTimeout (Just (Right v)) = Right v
    tryRun mn hsc = do
      r <- try (evalGhcEnv hsc (go mn)) :: IO (Either SomeException Text)
      pure r
    go mn = do
      setContext (IIModule (mkModuleName mn) : mkContext (eaImports ea))
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

-- | Interactive context from an import list (plain 'IIDecl's; the
-- anchor module itself is already in scope via 'IIModule').
mkContext :: [Text] -> [InteractiveImport]
mkContext extraImports =
  [ IIDecl (simpleImportDecl (mkModuleName (T.unpack e)))
  | e <- extraImports ++ ["Prelude"]
  ]

--------------------------------------------------------------------------------
-- F3: interactive evaluation with the anchor module IN SCOPE
-- (hls-eval-plugin's initialiseSessionForEval + Rules.hs, adapted)
--------------------------------------------------------------------------------

-- | Marker rule: files queued for evaluation get
-- @NeedsCompilation = Just BCOLinkable@ so 'GetLinkable' works for
-- them without TH-dependent revdeps (the hls-eval-plugin
-- @redefinedNeedsCompilation@ pattern).
data IsEvaluating = IsEvaluating
  deriving stock (Eq, Show)

type instance RuleResult IsEvaluating = Bool

instance NFData IsEvaluating where
  rnf IsEvaluating = ()

instance Hashable IsEvaluating where
  hashWithSalt s IsEvaluating = s

newtype EvaluatingVar = EvaluatingVar (IORef (HashSet NormalizedFilePath))
instance IsIdeGlobal EvaluatingVar

evalQueueRules :: Recorder (WithPriority Log) -> Rules ()
evalQueueRules recorder = do
  addIdeGlobal . EvaluatingVar =<< liftIO (newIORef mempty)
  defineEarlyCutoff (cmapWithPrio LogShake recorder) $ RuleNoDiagnostics $ \IsEvaluating f -> do
    alwaysRerun
    EvaluatingVar var <- getIdeGlobalAction
    b <- liftIO ((f `Set.member`) <$> readIORef var)
    pure (Just (if b then BS.singleton 1 else BS.empty), Just b)
  defineEarlyCutoff (cmapWithPrio LogShake recorder)
    $ RuleWithCustomNewnessCheck (<=) $ \NeedsCompilation f -> do
        isEvaluating <- use_ IsEvaluating f
        if isEvaluating
          then pure (Just (encodeLinkableType (Just BCOLinkable)), Just (Just BCOLinkable))
          else NS.needsCompilationRule f

-- | Mark a file as being evaluated (linkables on demand).
queueForEvaluation :: IdeState -> NormalizedFilePath -> IO ()
queueForEvaluation ide nfp = do
  EvaluatingVar var <- getIdeGlobalState ide
  atomicModifyIORef' var (\fs -> (Set.insert nfp fs, ()))

unqueueForEvaluation :: IdeState -> NormalizedFilePath -> IO ()
unqueueForEvaluation ide nfp = do
  EvaluatingVar var <- getIdeGlobalState ide
  atomicModifyIORef' var $ \fs -> (Set.delete nfp fs, ())

-- | Build an 'HscEnv' whose interactive context has the anchor
-- module in scope via 'IIModule', with linkables for every home
-- dependency (the missing piece behind \"which is not loaded\") and
-- the anchor's own @rdr_env@ re-injected into its interface (the
-- plugin's Note [Clearing mi_globals] workaround). @needsQC@ exposes
-- the QuickCheck package via dynflags — works in ANY component's
-- session once linkables are loaded.
-- | NOT self-locking: callers ('ideEvalExprIn' / 'ideTypeOfExprIn')
-- already hold 'isEvalLock' — an MVar is not reentrant, nesting it
-- self-deadlocks (F3: the matrix hang).
-- | The anchor's eval lifecycle: queue it for linkables (the
-- redefined NeedsCompilation rule) and drive the official
-- didOpen-equivalent — GetModArtefacts only computes serialized core
-- (the GetLinkable BCO input) on the IsFOI branch, and without an LSP
-- client nothing records the state changes, so we mark it OnDisk and
-- restart the session with the affected keys in dirtyKeys (the
-- mechanism Notifications.hs drives for real editors).
--
-- NOT self-locking: callers hold 'isEvalLock' — a concurrent burst of
-- restarts aborts each other's sessions (F3: matrix prop-determinism
-- raced exactly this way); the lock must cover lifecycle + body.
scopeAnchorForEval :: IdeSession -> NormalizedFilePath -> IO ()
scopeAnchorForEval s nfp0 = do
  queueForEvaluation (isState s) nfp0
  setSomethingModified VFSUnmodified (isState s) "mcp-eval-scope" $ do
    ks <- addFileOfInterest (isState s) nfp0 OnDisk
    pure ( toKey IsEvaluating nfp0
         : toKey NeedsCompilation nfp0
         : toKey GetModificationTime nfp0
         : ks )

ideInteractiveEnvFor :: IdeSession -> NormalizedFilePath -> Bool -> IO (Either Text (String, HscEnv))
ideInteractiveEnvFor s nfp0 needsQC = do
  r <- try (body nfp0) :: IO (Either SomeException (String, HscEnv))
  _ <- try (unqueueForEvaluation (isState s) nfp0) :: IO (Either SomeException ())
  case r of
    Left e  -> pure (Left ("EXC: " <> T.pack (show e)))
    Right v -> pure (Right v)
  where
    body nfp = do
      (ms, env1) <- runAction "mcp-ide-eval-env" (isState s) $ do
        ms <- msrModSummary <$> use_ GetModSummary nfp
        depsEq <- hscEnv <$> use_ GhcSessionDeps nfp
        mg <- useNoFile_ GetModuleGraph
        let linkablesNeeded = transitiveDeps mg nfp
        linkables <- uses_ GetLinkable
                       (nfp : maybe [] transitiveModuleDeps linkablesNeeded)
        -- GHC 9.11+: setContext IIModule reads mi_top_env from the
        -- HPT iface (mkTopLevEnv); disk .hi files never carry it, so
        -- rebuild it from the TypeCheck result (the plugin's
        -- addRdrEnv, GHC 9.12 flavor: local GREs + iface imports).
        tm <- tmrTypechecked <$> use_ TypeCheck nfp
        let addRdrEnv hmi
              | iface <- hm_iface hmi
              , ms_mod ms == mi_module iface
              = hmi
                  { hm_iface =
                      set_mi_top_env
                        (Just
                          (IfaceTopEnv
                            (forceGlobalRdrEnv (globalRdrEnvLocal (tcg_rdr_env tm)))
                            (map mkIfaceImport (tcg_import_decls tm))))
                        iface
                  }
              | otherwise = hmi
            linkableHsc =
              loadModulesHome (map (addRdrEnv . linkableHomeMod) linkables) depsEq
        pure (ms, linkableHsc)
      envFinal <- evalGhcEnv env1 $ do
        -- setSessionDynFlags panics on multi-unit sessions ("can
        -- only be used with a single home unit" — a project has lib +
        -- test-suite units). The plugin master uses modifyDynFlags,
        -- which only touches the INTERACTIVE context flags.
        let df = evalDynFlags (ms_hspp_opts ms)
        modifyDynFlags (const df)
        when needsQC (void (exposePackages' ["QuickCheck"]))
        getSession
      pure (moduleNameString (moduleName (ms_mod ms)), envFinal)

-- | Typechecker import specs → iface representation, so the
-- reconstructed anchor iface records what it imports (consumed by
-- 'mkTopLevEnv' when hydrating the IIModule context).
mkIfaceImport :: ImportUserSpec -> IfaceImport
mkIfaceImport (ImpUserSpec decl ImpUserAll) =
  IfaceImport decl ImpIfaceAll
mkIfaceImport (ImpUserSpec decl (ImpUserExplicit env)) =
  IfaceImport decl (ImpIfaceExplicit (forceGlobalRdrEnv env))
mkIfaceImport (ImpUserSpec decl (ImpUserEverythingBut ns)) =
  IfaceImport decl (ImpIfaceEverythingBut ns)

-- | Eval-friendly dynflags (hls-eval-plugin): default rules on,
-- monomorphism off, quiet output.
evalDynFlags :: DynFlags -> DynFlags
evalDynFlags df = df
  { useColor = Never, canUseColor = False
  , backend = interpreterBackend
  , extensionFlags =
      EnumSet.insert ExtendedDefaultRules
        (EnumSet.delete MonomorphismRestriction (extensionFlags df))
  }

-- | addPackages (dynflags-only; safe once linkables are loaded).
exposePackages' :: [Text] -> Ghc ()
exposePackages' pkgs = do
  -- Interactive-context flags only (multi-unit safe); the
  -- interactive ic_dflags ARE the session flags under
  -- 'modifyDynFlags' (ghcide's Util sets both).
  hsc <- getSession
  df <- getSessionDynFlags
  let us = CUnits.unitState hsc
      -- Name-based -package exposure breaks with multiple installed
      -- versions ("member of the hidden package QuickCheck-2.18/2.19")
      -- — resolve the UnitId and expose -package-id instead.
      exposeFor n
        | Just uid <- lookupPackageName us (PackageName (mkFastString n)) =
            ExposePackage ("-package-id " <> n)
              (UnitIdArg (RealUnit (Definite uid))) (ModRenaming True [])
        | otherwise =
            ExposePackage ("-package " <> n) (PackageArg n) (ModRenaming True [])
      exposed =
        foldr (\n acc ->
                if any (isExposed n) acc then acc
                else exposeFor (T.unpack n) : acc)
              (packageFlags df) pkgs
  modifyDynFlags (\df -> df { packageFlags = exposed })
  where
    isExposed n (ExposePackage _ (PackageArg a) _) = a == T.unpack n
    isExposed _ _ = False
