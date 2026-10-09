-- | ghcide-backend routing (F1 pilot → F3 expansion).
--
-- 'routeIde' is the strangler seam: when
-- @HASKELL_FLOWS_BACKEND=ghcide@ is set, these verbs are served by
-- 'IdeSession' instead of 'ApiSession'. Every other tool keeps its
-- legacy handler untouched — the flag is additive and defaults to
-- the legacy backend.
--
-- F3 serves: @ghc_check@ (load / module / project),
-- @ghc_eval@, @ghc_inspect(action=type)@ and
-- @ghc_property(action=check)@ — single-run QuickCheck AND the
-- @runs >= 2@ determinism replay.
module HaskellFlows.Tool.IdeBacked
  ( routeIde
  , warmupIdeSession
  , withIdeSession
  , handlePropertyRun
  , ReplayOutcome (..)
  , replayProp
  , replayStored
  , moduleKeyOf
  ) where

import Control.Concurrent.MVar (MVar, isEmptyMVar, modifyMVar)
import Control.Exception (SomeException, try)
import Control.Applicative ((<|>))
import Control.Monad (filterM, void, when)
import Data.Aeson (Value, object, withObject, (.=), (.:))
import Data.Aeson (FromJSON (parseJSON))
import qualified Data.ByteString as BS
import qualified Data.Aeson
import Data.List (isPrefixOf)
import Data.Maybe (fromMaybe, listToMaybe)
import qualified Data.Aeson.KeyMap as KM
import Data.Aeson.Key (Key)
import Data.Aeson.Types (parseEither)
import Data.Function ((&))
import Data.IORef (IORef, readIORef)
import Data.List (foldl', isPrefixOf, isSuffixOf, minimumBy, sort)
import Data.Ord (comparing)
import Data.Maybe (fromMaybe)
import Data.Text (Text)
import qualified Data.Text.Encoding as TE
import qualified Data.Text.IO as TIO
import qualified Data.Text as T
import qualified Data.Text.IO as TIO
import System.Directory (canonicalizePath, doesFileExist, listDirectory)
import System.IO (hPutStrLn, stderr)
import System.FilePath (normalise, (</>))

import HaskellFlows.Config (defaultLimits, determinismMaxRuns)
import HaskellFlows.Data.PropertyStore (Store, loadAll, saveCases)
import HaskellFlows.Data.PropertyStore (StoredProperty (..))
import HaskellFlows.Ghc.IdeSession
  ( EvalArgs (..)
  , IdeSession (..)
  , anchorModuleIn
  , bootIdeSession
  , ideDiagnosticsFor
  , EvalError (..)
  , EvalErrorClass (..)
  , evalErrorKind
  , ideEvalExprIn
  , ideEvalActionIn
  , ideModuleNameOf
  , ideProjectDiagnostics
  , ideTypeOfExprIn
  , ideInteractiveIn
  )
import HaskellFlows.Parser.Cabal (projectModuleFilesFromCabal)
import HaskellFlows.Parser.Hole (TypedHole (..), parseTypedHoles)
import HaskellFlows.Tool.Hole (HoleArgs (..), holesPayload, parseErrorKind)
import qualified HaskellFlows.Tool.Info as InfoTool
import qualified HaskellFlows.Tool.Complete as CompleteTool
import qualified HaskellFlows.Tool.Goto as GotoTool
import qualified HaskellFlows.Tool.Browse as BrowseTool
import HaskellFlows.Util.Process (capOutput)
import HaskellFlows.Mcp.Envelope qualified as Env
import HaskellFlows.Mcp.Envelope
  ( ErrorKind (..)
  , ToolResponse
  , mkErrorEnvelope
  , mkFailed
  , mkOk
  )
import HaskellFlows.Mcp.ToolName (ToolName (..))
import HaskellFlows.Types (ProjectDir, unProjectDir)
import HaskellFlows.Ghc.Sanitize (maxEvalBytes, sanitizeExpression)

-- | Get-or-boot the ghcide session under the MVar (first caller boots,
-- everyone else reuses — same single-writer shape as 'srvGhcSession').
-- The handler itself runs OUTSIDE the MVar: a slow tool call must never
-- hold the boot lock (that would serialize every ghcide-backed call).
withIdeSession
  :: MVar (Maybe IdeSession)
  -> IORef ProjectDir
  -> (IdeSession -> IO ToolResponse)
  -> IO ToolResponse
withIdeSession = withIdeSessionG

-- | Generalised boot-or-reuse for continuations returning any type
-- (the gates replay lists of results, not a single ToolResponse).
withIdeSessionG
  :: MVar (Maybe IdeSession)
  -> IORef ProjectDir
  -> (IdeSession -> IO a)
  -> IO a
withIdeSessionG ref pdRef k = do
  s <- modifyMVar ref $ \case
    Just s -> pure (Just s, s)
    Nothing -> do
      s <- bootIdeSession =<< readIORef pdRef
      pure (Just s, s)
  k s

-- | Typed replay outcome. 'ReplayLoadFailed' carries the
-- compiler/eval error so the agent sees WHY a property never ran
-- (#51: ghost identifiers must be visible in outcome.error, not
-- silently folded into a regression with raw="").
data ReplayOutcome
  = ReplayPassed !Int
    -- ^ All runs green; payload = number of runs.
  | ReplayRegressed !Text
    -- ^ QuickCheck worst-of state (failed / gave_up / …).
  | ReplayLoadFailed !Text
    -- ^ Never compiled or no session resolved — the error text.

-- | Replay a batch of stored properties through the session.
replayStored
  :: MVar (Maybe IdeSession)
  -> IORef ProjectDir
  -> [StoredProperty]
  -> IO [(StoredProperty, ReplayOutcome)]
replayStored ref pdRef props =
  withIdeSessionG ref pdRef $ \s ->
    mapM (replayProp pdRef s) props

-- | Boot the ghcide session in the background (idempotent — shares the
-- MVar singleton). Used by the transport warmup so the first tool call
-- on the ghcide backend arrives warm.
warmupIdeSession :: MVar (Maybe IdeSession) -> IORef ProjectDir -> IO ()
warmupIdeSession ref pdRef =
  void (withIdeSession ref pdRef (\_ -> pure mkWarmupAck))
  where
    mkWarmupAck = mkOk (object ["warmup" .= ("ghcide" :: Text)])

-- | Nothing for tools the ghcide backend does not serve (yet).
routeIde
  :: MVar (Maybe IdeSession)
  -> IORef ProjectDir
  -> IORef Store
  -> ToolName
  -> Value
  -> Maybe (IO ToolResponse)
routeIde ref pdRef storeRef tn args = case tn of
  GhcCheck
    | actionIs "module"  args -> Just (withIdeSession ref pdRef (handleCheckModule pdRef storeRef (stripped args)))
    | actionIs "load"    args -> Just (withIdeSession ref pdRef (handleCheckLoad pdRef (stripped args)))
    | actionIs "project" args -> Just (withIdeSession ref pdRef (handleCheckProject pdRef (stripped args)))
    | otherwise -> Nothing
  GhcEval -> Just (withIdeSession ref pdRef (handleEval args))
  GhcInspect
    | actionIs "type" args -> Just (withIdeSession ref pdRef (handleType (stripped args)))
    | actionIs "hole" args -> Just (withIdeSession ref pdRef (handleInspectHole pdRef (stripped args)))
    | actionIs "info" args -> Just (withIdeSession ref pdRef (handleInspectInfo (stripped args)))
    | actionIs "browse" args -> Just (withIdeSession ref pdRef (handleInspectBrowse pdRef (stripped args)))
    | actionIs "complete" args -> Just (withIdeSession ref pdRef (handleInspectComplete (stripped args)))
    | actionIs "goto" args -> Just (withIdeSession ref pdRef (handleInspectGoto (stripped args)))
    | otherwise -> Nothing
  GhcProperty
    | actionIs "check" args -> Just (withIdeSession ref pdRef (handlePropertyCheck pdRef storeRef (stripped args)))
    | actionIs "run" args -> Just (withIdeSession ref pdRef (handlePropertyRun pdRef storeRef))
    | otherwise -> Nothing
  _ -> Nothing

argField :: Key -> Value -> Either String Text
argField field = parseEither (withObject "args" (.: field))

-- | True when the composite @action@ field equals the given verb.
actionIs :: Text -> Value -> Bool
actionIs want v = case parseEither (withObject "args" (.: "action")) v of
  Right got -> got == want
  Left _    -> False

-- | Drop the composite @action@ before forwarding to the legacy-shaped
-- handler args.
stripped :: Value -> Value
stripped val = case val of
  Data.Aeson.Object o -> Data.Aeson.Object (KM.delete "action" o)
  _ -> val

-- | Envelope taxonomy for an eval failure — the total mapping in
-- "HaskellFlows.Ghc.EvalError" (budget tripping is InnerTimeout, a
-- missing instance keeps its honest kind).
budgetErrorKind :: EvalError -> ErrorKind
budgetErrorKind = evalErrorKind

--------------------------------------------------------------------------------
-- anchor machinery
--------------------------------------------------------------------------------

-- | Module name for an anchor path (src/Foo/Bar.hs → Foo.Bar).
moduleOfPath :: FilePath -> Text
moduleOfPath fp =
  let noExt = T.dropEnd 3 (T.pack fp)
      afterMarker m = case T.breakOnEnd m noExt of
        (pre, _) | not (T.null pre) -> Just (T.drop (T.length pre) noExt)
        _ -> Nothing
      base = case afterMarker "/src/" of
        Just m  -> m
        Nothing -> fromMaybe noExt (afterMarker "/test/")
      dotted = T.replace "/" "." base
  in if T.null dotted then noExt else dotted

-- | Typecheck the anchors once so their component sessions resolve:
-- a cold GhcSessionDeps rule can return Nothing until the component
-- has been built at least once by any rule (F3 finding).
warmAnchors :: IdeSession -> [FilePath] -> IO ()
warmAnchors s = mapM_ (\a -> void (ideDiagnosticsFor s a))

-- | test/ files first — their sessions carry the test-suite deps.
sortedForProperty :: [FilePath] -> [FilePath]
sortedForProperty as =
  [ a | a <- as, "/test/" `T.isInfixOf` T.pack a ]
    <> [ a | a <- as, "/test/" `T.isInfixOf` T.pack a == False ]

-- | Retry policy for the anchor chain — a pure predicate over the
-- classified error (the AnchorPlan extraction point; classification
-- itself lives in "HaskellFlows.Ghc.EvalError"). 'scopeRetry' is the
-- original policy: only scope/resolution errors advance.
-- 'evalRetry' additionally advances on the two eval-only wrapper
-- classes: the missing-Show type error (pure wrap on an IO-typed
-- expression — the fmap wrapper is next) and BadDependency (an
-- anchor with a poisoned linkable graph the expression may not
-- even need).
scopeRetry, evalRetry :: EvalError -> Bool
scopeRetry ev = evClass ev == ECScope
evalRetry ev =
  scopeRetry ev || evClass ev `elem` [ECIoWrapper, ECBadDependency]

-- | Run the anchor chain until one succeeds; @advance@ decides
-- which errors move to the next candidate. Everything else is a
-- real error and stops the chain. Parametric in the success type
-- so callers can pair the result with its winning anchor.
firstRight
  :: (EvalError -> Bool) -> [IO (Either EvalError a)] -> IO (Maybe (Either EvalError a))
firstRight advance [] = pure Nothing
firstRight advance (io : rest) = do
  v <- io
  case v of
    Right x   -> pure (Just (Right x))
    Left e
      | advance e -> firstRight advance rest
      | otherwise -> pure (Just (Left e))

listHs :: FilePath -> IO [FilePath]
listHs dir = do
  r <- try (listDirectory dir) :: IO (Either SomeException [FilePath])
  case r of
    Left _   -> pure []
    Right es -> pure (sort [dir </> e | e <- es, ".hs" `isSuffixOf` e])

-- | Keep-first dedup: the EARLIEST occurrence wins, so an explicit
-- given/target anchor stays ahead of the src\/test enumeration even
-- when the same file appears in both (a foldr-prepend dedup keeps
-- the LAST occurrence and silently demotes the given — that
-- reordered the property chain into an ambiguous-import eval).
dedup :: [FilePath] -> [FilePath]
dedup = foldl' (\acc x -> if x `elem` acc then acc else acc ++ [x]) []

-- | Anchor strategy (F3): the interactive context resolves a HOME
-- module only when the anchor's 'GhcSessionDeps' preloaded it as a
-- dependency — so anchor on a module that IMPORTS the target. Chain:
-- the caller's module (absolutized against the project dir — the
-- server's CWD is NOT the project), then test/ (imports the lib),
-- then every .hs under src/.
anchorCandidates :: IORef ProjectDir -> Maybe FilePath -> IO [FilePath]
anchorCandidates pdRef mGiven = do
  pd <- unProjectDir <$> readIORef pdRef
  let given = map (pd </>) (maybe [] pure mGiven)
  -- A stored NAME-shaped module key produces a path GUESS that may
  -- not exist ("<root>/CheckPropDemo"); a nonexistent anchor's
  -- rules abort with BadDependency and stop the whole chain. The
  -- real src/test files follow anyway — keep only existing givens.
  givenOk <- filterM doesFileExist given
  testHs <- listHs (pd </> "test")
  srcHs <- listHs (pd </> "src")
  pure (dedup (givenOk <> testHs <> srcHs))

-- | Anchor chain for eval/type: src/ first (library names live
-- there), then test/ (imports the library).
anchorChain :: IdeSession -> IO [FilePath]
anchorChain s = do
  let root = isRoot s
  testHs <- listHs (root </> "test")
  srcHs <- listHs (root </> "src")
  -- A binary (non-UTF-8) file can never provide a productive
  -- scope: its GhcSessionDeps/GetLinkable rules abort with
  -- 'BadDependency' and poison every eval anchored on it.
  filterM fileValidUtf8 (dedup (srcHs <> testHs))

-- | First @.cabal@ directly under the project dir.
findCabalIn :: FilePath -> IO (Maybe FilePath)
findCabalIn dir = do
  r <- try (listDirectory dir) :: IO (Either SomeException [FilePath])
  pure $ case r of
    Left _   -> Nothing
    Right es -> case [dir </> e | e <- es, ".cabal" `isSuffixOf` e] of
      (c : _) -> Just c
      []      -> Nothing

--------------------------------------------------------------------------------
-- ghc_check: load / module / project
--------------------------------------------------------------------------------

-- | Resolve a caller-supplied module path against the project dir,
-- refusing escapes and (optionally) nonexistent files BEFORE any
-- ghcide rule runs. A bogus path fed to the session loader makes
-- hie-bios invoke @cabal v2-repl@ with out-of-tree targets — a
-- wedge vector, and pre-C1 'Tool.Load' guarded exactly this (its
-- tests died with it; the e2e scenarios LoadNonexistent #79 and
-- InjectionGuard keep the contract).
guardModulePath
  :: ProjectDir -> FilePath -> Bool -> IO (Either ToolResponse FilePath)
guardModulePath pd rel requireExists = do
  let root = unProjectDir pd
      abs' = normalise (root </> rel)
      underRoot = root == abs' || (root ++ "/") `isPrefixOf` abs'
  if not underRoot
    then pure . Left $ Env.mkRefused
      (mkErrorEnvelope Validation
        ("module path escapes the project root: " <> T.pack rel))
    else do
      exists <- doesFileExist abs'
      if requireExists && not exists
        then pure $ Left $ mkFailed
          (mkErrorEnvelope Validation
            ("module path does not exist: " <> T.pack rel))
        else pure (Right abs')

-- | Split diagnostics into the legacy load/module shape: @errors@ +
-- @warnings@ arrays of structured objects, @summary@ line. The
-- composite 'Env.withResultAction' contract applies: the executed
-- action is stamped into the result so 'suggestNext' discriminates.
-- | GHCi-style raw rendering of diagnostic VALUES (file:line:col:
-- sev: [code] message), joined by blank lines — the same shape the
-- legacy backend's @raw@ field carries (renderGhciStyle).
renderDiagsRaw :: [Value] -> Text
renderDiagsRaw = T.intercalate "\n\n" . map renderOne
  where
    renderOne v =
      let fld k = case KM.lookup k (objOf' v) of
                    Just (Data.Aeson.String t) -> T.unpack t
                    Just (Data.Aeson.Number n) -> show (round n :: Int)
                    _ -> ""
          objOf' j = case j of Data.Aeson.Object o -> o; _ -> mempty
          code = let c = fld "code" in if null c then "" else "[" <> c <> "] "
      in T.pack (fld "file" <> ":" <> fld "line" <> ":" <> fld "column"
                 <> ": " <> fld "severity" <> ": " <> code)
         <> T.pack (fld "message")

-- | Classify load diagnostics (pure, unit-testable): typed holes
-- (GHC-88464, "Found hole") surface as WARNINGS — the deferred-pass
-- contract a GHCi user gets; without '-fdefer-typed-holes' GHC
-- reports them as severity=error (GHC 9.12 User's Guide) — and
-- GHC-58427 "is not loaded" artifacts of the pass are dropped
-- whenever any other diagnostic exists (#57 / F-23: one entry per
-- real problem, errors clean of internal echoes).
classifyLoadDiags :: [Value] -> ([Value], [Value])
classifyLoadDiags diags =
  let errs0  = [d | d <- diags, isSevDiag "error" d]
      warns0 = [d | d <- diags, isSevDiag "warning" d]
      holes  = filter isHoleDiag errs0
      errs1  = filter (not . isHoleDiag) errs0
      real   = [d | d <- errs1, not (isDeferredArtifact d)]
      errs2  = if null (real <> warns0 <> holes)
                 then errs1   -- the artifact is the only signal: keep it
                 else real
  in (errs2, warns0 <> holes)

-- | GHC-58427 ("<module> is not loaded") — an internal echo of the
-- diagnostics pass, identified by its diagnostic code.
isDeferredArtifact :: Value -> Bool
isDeferredArtifact d =
  let asText (Data.Aeson.String t) = Just t
      asText _                     = Nothing
  in any (maybe False ("58427" `T.isInfixOf`))
       [ KM.lookup k (diagObj d) >>= asText | k <- ["code", "message"] ]

loadShapeEnvelope :: Text -> Text -> [Value] -> ToolResponse
loadShapeEnvelope action mp diags =
  let (errs, warns) = classifyLoadDiags diags
      summary
        | not (null errs) =
            "Compile failed. " <> T.pack (show (length errs)) <> " error(s), "
              <> T.pack (show (length warns)) <> " warning(s)."
        | otherwise =
            "Compiled OK. " <> T.pack (show (length warns)) <> " warning(s)."
      result = object
        [ "action" .= action
        , "module_path" .= mp
        , "backend" .= ("ghcide" :: Text)
        , "errors" .= errs
        , "warnings" .= warns
        , "raw" .= renderDiagsRaw diags
        , "summary" .= summary
        ]
  in if null errs
       then Env.mkOk result
       else Env.mkFailed
            ((mkErrorEnvelope CompileError summary)
               { Env.eeCause = Just "ghcide_diagnostics" })
            & \r -> r { Env.reResult = Just result }

handleCheckModule :: IORef ProjectDir -> IORef Store -> Value -> IdeSession -> IO ToolResponse
handleCheckModule pdRef storeRef raw s = case argField "module_path" raw of
  Left err -> pure (mkFailed (mkErrorEnvelope MissingArg (T.pack err)))
  Right mp -> do
    pd <- readIORef pdRef
    guarded <- guardModulePath pd (T.unpack mp) True
    case guarded of
      Left refusal -> pure refusal
      Right absPath -> handleCheckModuleAt pdRef storeRef mp absPath raw s

-- | Canonical store key of a module (#74): the module NAME derived
-- identically for absolute and relative inputs — "src/Foo.hs",
-- "/root/src/Foo.hs", "test/Spec.hs", "src/Expr/S.hs" → Foo, Foo,
-- Spec, Expr.S. The LATER of src\//test\// wins so nested trees keep
-- their hierarchy. Pure — the single law writers and readers share.
moduleKeyOf :: Text -> Text
moduleKeyOf p0 =
  let noExt = T.dropEnd 3 p0
      after m = case T.breakOnEnd m noExt of
        (pre, _) | not (T.null pre) -> Just (T.drop (T.length pre) noExt)
        _ -> Nothing
      -- the marker occurring LAST in the path leaves the SHORTEST
      -- remainder
      cands = [ r | Just r <- [after "src/", after "test/"] ]
      base = case cands of
        [] -> noExt
        _  -> minimumBy (comparing T.length) cands
  in T.replace "/" "." base

-- | Normalised store-key identity (#74): a stored property's
-- 'module' hint arrives as either a project-relative PATH
-- ("src/GateDemo.hs") or a module NAME ("GateDemo"). Both writers
-- and readers go through this one law, so the two shapes are
-- interchangeable. Pure — unit-testable.
propertyKeyMatches :: Text -> Maybe Text -> Bool
propertyKeyMatches _ Nothing = False
propertyKeyMatches targetName (Just k)
  | ".hs" `T.isSuffixOf` k || "/" `T.isInfixOf` k =
      moduleKeyOf k == targetName
  | otherwise = k == targetName

-- | Second half of the module check — runs with the guarded,
-- absolute path.
handleCheckModuleAt :: IORef ProjectDir -> IORef Store -> Text -> FilePath -> Value -> IdeSession -> IO ToolResponse
handleCheckModuleAt pdRef storeRef mp absPath raw s = do
    pd <- readIORef pdRef
    diags <- ideDiagnosticsFor s absPath
    -- #74/#42: the properties gate replays the stored properties
    -- that belong to THIS module under the normalised key —
    -- module-name-shaped and path-shaped entries both match.
    let targetName = moduleKeyOf mp
    store <- readIORef storeRef
    allProps <- loadAll store
    let mine = [ p | p <- allProps, propertyKeyMatches targetName (spModule p) ]
    replayed <- mapM (replayProp pdRef s) mine
    let propTotal   = length replayed
        propPassed  = length [ () | (_, ReplayPassed _)    <- replayed ]
        propFailed  = length [ () | (_, ReplayRegressed _) <- replayed ]
        propUnload  = length [ () | (_, ReplayLoadFailed _) <- replayed ]
        propOk      = propFailed == 0 && propUnload == 0
        propGate
          | propTotal == 0 =
              moduleGate True "no stored properties for this module"
          | propFailed > 0 =
              moduleGate False (T.pack (show propFailed) <> " stored propert"
                <> (if propFailed == 1 then "y regressed" else "ies regressed"))
          | propUnload > 0 =
              moduleGate False (T.pack (show propUnload) <> " stored propert"
                <> (if propUnload == 1 then "y failed" else "ies failed")
                <> " to load — replay via ghc_property(action=run) for the error")
          | otherwise =
              moduleGate True "stored properties pass"
        moduleGate ok why = object
          [ "ok" .= ok
          , "reason" .= (why :: Text)
          , "status" .= (if ok then "pass" else
                           if propUnload > 0 then "skipped" :: Text
                           else "failed" :: Text)
          , "total" .= propTotal
          , "passed" .= propPassed
          ]
    -- Product contract (CheckModule.renderResult): overall + gates,
    -- with warnings_block (default True = warnings block).
    let warnBlock = case KM.lookup "warnings_block" (diagObj raw) of
                      Just (Data.Aeson.Bool b) -> b
                      _                        -> True
        errs  = [d | d <- diags, isSevDiag "error" d]
        warns = [d | d <- diags, isSevDiag "warning" d]
        holes = [d | d <- diags, isHoleDiag d]
        compileOk = null errs
        overall = compileOk && (null warns || not warnBlock) && null holes
                  && propOk
        payload =
          [ "action"  .= ("module" :: Text)
          , "module_path" .= mp
          , "backend" .= ("ghcide" :: Text)
          , "module"  .= mp
          , "overall" .= overall
          , "errors"  .= errs
          , "warnings" .= warns
          , "raw"     .= renderDiagsRaw diags
          , "gates"   .= object
              [ "compile"  .= moduleGate compileOk
                  (if compileOk then "module compiles strictly"
                                else T.pack (show (length errs)) <> " error(s)")
              , "warnings" .= moduleGate (null warns || not warnBlock)
                  (if null warns then "no warnings (-Wall clean)"
                   else if warnBlock
                     then T.pack (show (length warns)) <> " warning(s) (blocking — pass warnings_block=false to keep iterating)"
                     else T.pack (show (length warns)) <> " warning(s) (informational; warnings_block=false)")
              , "holes"    .= moduleGate (null holes)
                  (if null holes then "no deferred typed holes"
                                 else T.pack (show (length holes)) <> " typed hole(s) found")
              , "properties" .= propGate
              ]
          , "summary" .= (if overall then "Compiled OK." else "Module check failed." :: Text)
          ]
    pure $ if overall
      then Env.mkOk (object payload)
      else Env.mkFailed
        ((mkErrorEnvelope CompileError "Module check failed — ghcide backend")
          { Env.eeCause = Just "ghcide_diagnostics" })
        & \r -> r { Env.reResult = Just (object payload) }

-- | Cradle-resolution diagnostics (module not in any component).
isCradleDiag :: Value -> Bool
isCradleDiag d = case KM.lookup "message" (diagObj d) of
  Just (Data.Aeson.String m) ->
       "not be listed in your .cabal file" `T.isInfixOf` m
    || "No cradle target found" `T.isInfixOf` m
    || "Loading the module" `T.isInfixOf` m && "failed" `T.isInfixOf` m
  _ -> False

-- | Severity classifier over diagnostic VALUES.
isSevDiag :: T.Text -> Value -> Bool
isSevDiag want d = KM.lookup "severity" (diagObj d) == Just (Data.Aeson.String want)

-- | The KeyMap of a diagnostic VALUE (empty for non-objects).
diagObj :: Value -> KM.KeyMap Value
diagObj (Data.Aeson.Object o) = o
diagObj _ = KM.empty

-- | Typed-hole diagnostics (deferred holes surface as warnings with
-- the hole marker in the message).
isHoleDiag :: Value -> Bool
isHoleDiag d = case KM.lookup "message" (diagObj d) of
  Just (Data.Aeson.String m) -> "Found hole" `T.isInfixOf` m
  _                          -> False

handleCheckLoad :: IORef ProjectDir -> Value -> IdeSession -> IO ToolResponse
handleCheckLoad pdRef raw s = do
  -- No module_path = "load the library": default to the first
  -- module listed in the .cabal (the scaffold's main lib module),
  -- falling back to the first .hs under src/.
  mp <- case argField "module_path" raw of
    Right m -> pure m
    Left _ -> do
      pd <- readIORef pdRef
      mCabal <- findCabalIn (unProjectDir pd)
      cabal <- maybe (pure "") (TIO.readFile) mCabal
      let fromCabal = case projectModuleFilesFromCabal cabal of
            (m : _) -> Just m
            []      -> Nothing
      srcHs <- listHs (unProjectDir pd </> "src")
      pure $ case fromCabal <|> listToMaybe srcHs of
        Just first -> T.pack (relativizeTo (unProjectDir pd) first)
        Nothing    -> ""
  if T.null mp
    then pure (mkFailed (mkErrorEnvelope MissingArg
                "no module_path given and no library module found to default to"))
    else handleCheckLoadAt pdRef mp raw s

handleCheckLoadAt :: IORef ProjectDir -> Text -> Value -> IdeSession -> IO ToolResponse
handleCheckLoadAt pdRef mp raw s = do
    pd <- readIORef pdRef
    guarded <- guardModulePath pd (T.unpack mp) True
    case guarded of
      Left refusal -> pure refusal
      Right absPath -> do
        -- Non-UTF-8 guard: GHC's reader diagnostics for binary files
        -- are unreliable; refuse cleanly BEFORE touching the session
        -- (Non-UTF-8 contract: rejected cleanly, session stays alive).
        bytes <- BS.readFile absPath
        case TE.decodeUtf8' bytes of
          Left _ ->
            pure . Env.mkFailed $ mkErrorEnvelope Validation
              ("module file is not valid UTF-8: " <> mp)
          Right _ -> do
            diags <- ideDiagnosticsFor s absPath
            pure (loadShapeEnvelope "load" mp diags)

-- | F3 project gate: typecheck every module listed in the .cabal.
-- Legacy-compatible @gates.compile@ verdict + per-module rows.
-- | 'True' when the file decodes as clean UTF-8 — the shared
-- boundary guard of check(load|project). A file that does not
-- exist is downstream's 'not_found' business (the .cabal may list
-- modules not yet scaffolded); only decode-ability is ours.
fileValidUtf8 :: FilePath -> IO Bool
fileValidUtf8 fp = do
  ok <- doesFileExist fp
  if not ok
    then pure True
    else either (const False) (const True) . TE.decodeUtf8' <$> BS.readFile fp

handleCheckProject :: IORef ProjectDir -> Value -> IdeSession -> IO ToolResponse
handleCheckProject pdRef raw s = do
  pd <- readIORef pdRef
  mCabal <- findCabalIn (unProjectDir pd)
  case mCabal of
    Nothing ->
      pure (mkFailed (mkErrorEnvelope MissingArg "no .cabal found in project dir"))
    Just cabalPath -> do
      cabal <- TIO.readFile cabalPath
      let mods = projectModuleFilesFromCabal cabal
      if null mods
        then pure (mkFailed (mkErrorEnvelope MissingArg
                              "no modules listed in .cabal"))
        else do
          -- Non-UTF-8 guard (same contract as action=load): binary
          -- modules make GHC's reader diagnostics unreliable; refuse
          -- cleanly BEFORE touching the session.
          let absMods = map (unProjectDir pd </>) mods
          mBad <- listToMaybe <$> filterM (fmap not . fileValidUtf8) absMods
          case mBad of
            Just fp -> pure . Env.mkFailed $ mkErrorEnvelope Validation
              ("module file is not valid UTF-8: "
                 <> T.pack (relativizeTo (unProjectDir pd) fp))
            Nothing -> checkProjectRows pd raw s absMods

-- | The project typecheck pass — reached only when every module
-- file decodes as UTF-8.
checkProjectRows :: ProjectDir -> Value -> IdeSession -> [FilePath] -> IO ToolResponse
checkProjectRows pd raw s absMods = do
  rows <- ideProjectDiagnostics s absMods
  -- Product contract (CheckProject): overall / total /
  -- checked / passed / failed / not_found / per_module.
  -- not_found = modules whose only diagnostics are
  -- cradle-resolution failures ("not listed in .cabal" /
  -- "No cradle target") — they never reached compilation.
  let warnBlock = case KM.lookup "warnings_block" (diagObj raw) of
                      Just (Data.Aeson.Bool b) -> b
                      _                        -> True
      outcomeOf (fp, ds) =
        let errs   = [d | d <- ds, isSevDiag "error" d]
            warns  = [d | d <- ds, isSevDiag "warning" d]
            cradle = [d | d <- ds, isCradleDiag d]
            nm     = T.pack (relativizeTo (unProjectDir pd) fp)
            detail = object
              [ "errors" .= errs
              , "warnings" .= warns
              ]
        in ( nm
           , if not (null cradle) && null errs
               then "not_found" :: Text
               else if not (null errs) then "failed"
               else if not (null warns) && warnBlock then "failed"
               else "ok"
           , detail )
      outcomes = map outcomeOf rows
      notFound = [nm | (nm, "not_found", _) <- outcomes]
      failing  = [nm | (nm, st, _) <- outcomes, st == "failed"]
      total    = length outcomes
      nChecked = total - length notFound
      okCount  = length [() | (_, "ok", _) <- outcomes]
      overall  = null failing && null notFound
      perModule =
        [ object
            [ "module" .= nm
            , "status" .= st
            , "module_path" .= nm
            , "detail" .= detail
            ]
        | (nm, st, detail) <- outcomes
        ]
      summaryText =
        T.pack (show okCount) <> "/" <> T.pack (show total)
          <> " modules compile clean."
          <> (if not (null notFound)
                then " (" <> T.pack (show (length notFound)) <> " not found)"
                else "")
      payload =
        [ "action"   .= ("project" :: Text)
        , "backend"  .= ("ghcide" :: Text)
        , "overall"  .= overall
        , "total"    .= total
        , "checked"  .= nChecked
        , "passed"   .= okCount
        , "failed"   .= length failing
        , "not_found" .= length notFound
        , "skipped"  .= (0 :: Int)
        , "gates"    .= object [ "compile" .= overall ]
        , "modules"  .= perModule
        , "per_module" .= perModule
        , "summary"  .= summaryText
        ]
  if overall
    then pure (Env.mkOk (object payload))
    else pure
          (Env.mkFailed
             (mkErrorEnvelope GateFailure
                (T.pack (show (length failing + length notFound))
                   <> " module(s) failing across project — ghcide backend"))
             & \r -> r { Env.reResult = Just (object payload) })

--------------------------------------------------------------------------------
-- ghc_eval / ghc_inspect(type)
--------------------------------------------------------------------------------

-- | Output capping lives in "HaskellFlows.Util.Process" ('capOutput')
-- — the single law eval output and every subprocess stream share.

handleEval :: Value -> IdeSession -> IO ToolResponse
handleEval raw s = case argField "expression" raw of
  Left err -> pure (mkFailed (mkErrorEnvelope MissingArg (T.pack err)))
  -- Boundary safety, single-sourced: the SAME 'sanitizeExpression'
  -- policy every retired GHCi tool routed through — newline/sentinel
  -- injection (InjectionGuard), the 64 KiB input cap (Oversized /
  -- CWE-400: reject BEFORE any compilation) and the two-limb GMP
  -- literal or @N^E@ exponent that segfaults the in-process RTS
  -- (#127) — untrappable by 'try', so it MUST die at this boundary.
  Right expr -> case sanitizeExpression expr of
    Left cmdErr ->
      pure . Env.mkRefused $ Env.sanitizeRejection "expression" cmdErr
    Right safe -> do
      anchors <- anchorChain s
      warmAnchors s anchors
      -- RCE-by-design contract: ghc_eval executes arbitrary IO, like
      -- GHCi. BOTH wrappers type as IO String so the run-the-action
      -- runner is uniform: the pure case renders via
      -- 'Control.Exception.evaluate', the IO case runs the user
      -- action and shows its result (writeFile renders "()",
      -- readFile renders the shown file contents). The pure wrap
      -- type-errors on IO actions ('No instance for `Show (IO …)'),
      -- which advances the chain to the fmap wrapper.
      r <- firstRight evalRetry
        [ ideEvalActionIn s (EvalArgs a ["System.IO", "Control.Exception"] False) wrap
        | a <- anchors
        , wrap <- [ "Control.Exception.evaluate (show (" <> safe <> "))"
                  , "fmap show (" <> safe <> ")" ] ]
      case r of
        Nothing -> pure (mkFailed (mkErrorEnvelope MissingArg
                            "no project module provides the names this expression needs"))
        Just (Left err) -> pure (mkFailed (mkErrorEnvelope (budgetErrorKind err) (evText err)))
        Just (Right out) ->
          let (capped, wasTruncated) = capOutput maxEvalBytes out
          in pure
              ( mkOk
                  ( object
                      [ "output" .= capped
                      , "truncated" .= wasTruncated
                      , "backend" .= ("ghcide" :: Text)
                      ]
                  )
              )

handleType :: Value -> IdeSession -> IO ToolResponse
handleType raw s = case argField "expression" raw of
  Left err -> pure (mkFailed (mkErrorEnvelope MissingArg (T.pack err)))
  -- Same boundary as 'handleEval': the parser DoS the eval cap closes
  -- is reachable through inspect(type) otherwise ('exprType' parses
  -- the whole expression with no cap of its own).
  Right expr -> case sanitizeExpression expr of
    Left cmdErr ->
      pure . Env.mkRefused $ Env.sanitizeRejection "expression" cmdErr
    Right safe -> do
      anchors <- anchorChain s
      warmAnchors s anchors
      r0 <- firstRight evalRetry
        [ ideTypeOfExprIn s (EvalArgs a ["System.IO"] False) safe | a <- anchors ]
      case r0 of
        Just (Right ty) -> pure (mkOk (object ["type" .= ty, "backend" .= ("ghcide" :: Text)]))
        Just (Left err) -> pure (mkFailed (mkErrorEnvelope (budgetErrorKind err) (evText err)))
        Nothing -> do
          anchor <- anchorModuleIn s
          r <- ideTypeOfExprIn s (EvalArgs anchor [] False) safe
          case r of
            Right ty -> pure (mkOk (object ["type" .= ty, "backend" .= ("ghcide" :: Text)]))
            Left err -> pure (mkFailed (mkErrorEnvelope (budgetErrorKind err) (evText err)))

--------------------------------------------------------------------------------
-- ghc_inspect(action=hole) — typed holes from ghcide diagnostics
--------------------------------------------------------------------------------

-- | W6 migration: the legacy engine loaded the module under
-- @-fdefer-typed-holes@ and parsed the GHCi-style rendering of the
-- captured diagnostics. ghcide's TypeCheck rule surfaces the SAME
-- GHC-88464 \"Found hole\" diagnostics (the load path's
-- 'classifyLoadDiags' already filters them), so the parse and the
-- payload shaping are the legacy module's pure functions — only the
-- session plumbing is gone.
handleInspectHole :: IORef ProjectDir -> Value -> IdeSession -> IO ToolResponse
handleInspectHole pdRef raw s = case parseEither parseJSON raw of
  Left parseError ->
    pure (mkFailed
      ((mkErrorEnvelope (parseErrorKind parseError)
          (T.pack ("Invalid arguments: " <> parseError)))
            { Env.eeCause = Just (T.pack parseError) }))
  Right (HoleArgs rawPath filt) -> do
    pd <- readIORef pdRef
    guarded <- guardModulePath pd (T.unpack rawPath) True
    case guarded of
      Left early -> pure early
      Right absPath -> do
        diags <- ideDiagnosticsFor s absPath
        let rendered = renderDiagsRaw diags
            allHoles = parseTypedHoles rendered
            holes    = case filt of
              Nothing -> allHoles
              Just nm -> filter ((== nm) . thHole) allHoles
            payload  = holesPayload rawPath holes
        -- Issue #90 §3 + §6: zero holes maps to 'no_match' — the
        -- question was well-formed, the answer is the empty set.
        pure $ case holes of
          [] -> Env.mkNoMatch payload
          _  -> mkOk payload

--------------------------------------------------------------------------------
-- ghc_inspect(info|browse|complete|goto) — interactive GHC queries
-- on the component of the anchor (W6)
--------------------------------------------------------------------------------

-- | The anchor every interactive inspect query runs against: the
-- first source module under src/ (the scaffold's lib root).
inspectAnchor :: IdeSession -> IO FilePath
inspectAnchor = anchorModuleIn

-- | Shared parse-failure shape of the migrated inspect verbs.
inspectParseFail :: String -> ToolResponse
inspectParseFail parseError =
  mkFailed
    ((mkErrorEnvelope (parseErrorKind parseError)
        (T.pack ("Invalid arguments: " <> parseError)))
          { Env.eeCause = Just (T.pack parseError) })

-- | Shared GHC-API-failure shape (legacy parity: InternalError,
-- exception text in message + cause).
inspectQueryFail :: EvalError -> ToolResponse
inspectQueryFail err =
  mkFailed
    ((mkErrorEnvelope InternalError ("GHC API error: " <> evText err))
      { Env.eeCause = Just (evText err) })

handleInspectInfo :: Value -> IdeSession -> IO ToolResponse
handleInspectInfo raw s = case parseEither parseJSON raw of
  Left parseError -> pure (inspectParseFail parseError)
  Right (InfoTool.InfoArgs nm) -> case sanitizeExpression nm of
    Left cmdErr ->
      pure . Env.mkRefused $ Env.sanitizeRejection "name" cmdErr
    Right safe -> do
      anchor <- inspectAnchor s
      r <- ideInteractiveIn s (EvalArgs anchor [] False)
             (InfoTool.queryInfo safe)
      pure $ case r of
        -- Issue #87 + #90 parity: the resolution attempt happened
        -- and didn't surface a binding — no_match, cause rides along.
        Left err -> Env.mkNoMatch
          (InfoTool.notInScopePayload safe (Just (evText err)))
        Right Nothing -> Env.mkNoMatch (InfoTool.notInScopePayload safe Nothing)
        Right (Just (pinfo, ctorPairs, methodPairs)) ->
          Env.mkOk (InfoTool.successPayload pinfo ctorPairs methodPairs)

handleInspectBrowse :: IORef ProjectDir -> Value -> IdeSession -> IO ToolResponse
handleInspectBrowse pdRef raw s = case parseEither parseJSON raw of
  Left parseError -> pure (inspectParseFail parseError)
  Right (BrowseTool.BrowseArgs m) -> do
    pd <- readIORef pdRef
    -- Anchor at the browsed module's own file when the project
    -- declares it: scoping the anchor bytecode-compiles THAT
    -- module, which is what makes its interface visible to the
    -- interactive queries (the single-anchor probe contract).
    -- Foreign modules keep the generic src anchor and resolve
    -- through the package-environment fallback. ghcide
    -- canonicalizes module paths (/var → /private/var on macOS);
    -- the graph-prefix filter in queryBrowseGraph only matches
    -- when the root carries the same canonical shape.
    root <- canonicalizePath (unProjectDir pd)
    mCabal <- findCabalIn (unProjectDir pd)
    cabal <- maybe (pure "") TIO.readFile mCabal
    let own = listToMaybe
          [ f | f <- projectModuleFilesFromCabal cabal
              , moduleKeyOf (T.pack f) == m ]
    anchor <- case own of
      Just f  -> pure (unProjectDir pd </> f)
      Nothing -> inspectAnchor s
    r <- ideInteractiveIn s (EvalArgs anchor [] False)
           (BrowseTool.queryBrowseGraph root m)
    case r of
      Left err -> pure (inspectQueryFail err)
      -- An empty graph answer is NOT a real browse: the module was
      -- in the graph but its interface isn't loadable in this HscEnv
      -- (ghcide keeps home-module interfaces in memory). Fall
      -- through to the contextual read like the Nothing case.
      Right (Just entries) | not (null entries) ->
        pure (mkOk (BrowseTool.browsePayload m entries))
      Right _ -> do
        -- W6: home modules whose interfaces live only in ghcide's
        -- memory — read the exports from the interactive context.
        r1' <- ideInteractiveIn s (EvalArgs anchor [] False)
                 (BrowseTool.queryBrowseContextual m)
        case r1' of
          Right (Just entries) -> pure (mkOk (BrowseTool.browsePayload m entries))
          _ -> do
            -- #168 fallback: try the package environment.
            r2 <- ideInteractiveIn s (EvalArgs anchor [] False)
                    (BrowseTool.queryBrowseFallback m)
            pure $ case r2 of
              Right (Just entries) -> mkOk (BrowseTool.browsePayload m entries)
              _ -> Env.withNextStep BrowseTool.moduleNotInGraphNextStep
                     (Env.mkNoMatch (BrowseTool.moduleNotInGraphPayload m))

handleInspectComplete :: Value -> IdeSession -> IO ToolResponse
handleInspectComplete raw s = case parseEither parseJSON raw of
  Left parseError -> pure (inspectParseFail parseError)
  Right (CompleteTool.CompleteArgs prefix limit) ->
    case sanitizeExpression prefix of
      Left cmdErr ->
        pure . Env.mkRefused $ Env.sanitizeRejection "prefix" cmdErr
      Right safe -> do
        anchor <- inspectAnchor s
        r <- ideInteractiveIn s (EvalArgs anchor [] False)
               (CompleteTool.queryCompletions safe)
        case r of
          Left err -> pure (inspectQueryFail err)
          Right cands
            -- #252: no in-scope matches + qualified prefix → the
            -- module isn't imported; answer from the graph/env.
            | null cands
            , Just (qual, npfx) <- CompleteTool.splitQualifiedPrefix safe -> do
                r2 <- ideInteractiveIn s (EvalArgs anchor [] False)
                        (CompleteTool.queryQualifiedFallback qual npfx)
                pure $ CompleteTool.renderCompletions prefix limit
                  (case r2 of Right xs -> xs; Left _ -> [])
            | otherwise ->
                pure (CompleteTool.renderCompletions prefix limit cands)

handleInspectGoto :: Value -> IdeSession -> IO ToolResponse
handleInspectGoto raw s = case parseEither parseJSON raw of
  Left parseError -> pure (inspectParseFail parseError)
  Right (GotoTool.GotoArgs nm) -> case sanitizeExpression nm of
    Left cmdErr ->
      pure . Env.mkRefused $ Env.sanitizeRejection "name" cmdErr
    Right safe -> do
      anchor <- inspectAnchor s
      r <- ideInteractiveIn s (EvalArgs anchor [] False)
             (GotoTool.queryLocation safe)
      case r of
        Left err -> pure (inspectQueryFail err)
        Right (Just loc) -> pure $
          -- Issue #117: file locations → ok; library locations →
          -- no_match with the reason in the payload.
          case loc of
            GotoTool.InFile {}   -> mkOk (GotoTool.locationPayload safe loc)
            GotoTool.InModule {} -> Env.mkNoMatch (GotoTool.locationPayload safe loc)
        Right Nothing -> do
          -- #224: qualified name? retry the unqualified suffix
          -- against the session before the generic remediation.
          let unqual = T.takeWhileEnd (/= '.') safe
          if T.length unqual < T.length safe && not (T.null unqual)
            then do
              r2 <- ideInteractiveIn s (EvalArgs anchor [] False)
                      (GotoTool.queryLocation unqual)
              pure $ case r2 of
                Right (Just loc) ->
                  Env.mkNoMatch (GotoTool.qualifiedPreloadPayload safe unqual loc)
                _ -> Env.mkNoMatch (GotoTool.notInScopePayload safe)
            else pure (Env.mkNoMatch (GotoTool.notInScopePayload safe))

--------------------------------------------------------------------------------
-- ghc_property(action=check) — QuickCheck via the session
--------------------------------------------------------------------------------

-- | QuickCheck via the ghcide session. Compiles
-- @unsafePerformIO (quickCheckWithResult stdArgs prop)@ rendered as
-- @STATE|<state>|N|<tests>|OUT|<output>@ in the component context of
-- an anchor whose graph provides both QuickCheck and the property's
-- home module. @runs >= 2@ replays N times (@RUN;STATE|…@) and
-- reports stability — the determinism route.
handlePropertyCheck :: IORef ProjectDir -> IORef Store -> Value -> IdeSession -> IO ToolResponse
handlePropertyCheck pdRef storeRef raw s = case argField "property" raw of
  Left err -> pure (mkFailed (mkErrorEnvelope MissingArg (T.pack err)))
  Right prop -> do
    let runs = case KM.lookup "runs" (objOf raw) of
                 Just (Data.Aeson.Number n) ->
                   -- clampRuns invariant (was the dead Determinism
                   -- handler's): a flaky-rerun request is capped into
                   -- [1, determinismMaxRuns]; large values only burn
                   -- subprocess-equivalent budget.
                   max 1 (min (determinismMaxRuns defaultLimits) (round n))
                 _ -> 1
        anchorArg = case KM.lookup "module" (objOf raw) of
          Just (Data.Aeson.String m) -> Just (T.unpack m)
          _ -> Nothing
        expr = qcExpr prop runs
        targetMod = maybe "" (moduleOfPath . T.unpack) moduleArgText
        moduleArgText = case KM.lookup "module" (objOf raw) of
          Just (Data.Aeson.String m) -> Just m
          _ -> Nothing

    anchors <- anchorCandidates pdRef anchorArg
    warmAnchors s anchors
    -- When the winning anchor is NOT the target file (e.g. the
    -- test-suite Spec provides QuickCheck while the target lives in
    -- the lib component), the target must be imported explicitly —
    -- by its TRUE module name (GetModSummary), never a path guess.
    pdNow <- unProjectDir <$> readIORef pdRef
    tmName <- case anchorArg of
      Just rel -> ideModuleNameOf s (pdNow </> rel)
      Nothing  -> pure Nothing
    let importsFor a =
          [ "Test.QuickCheck", "System.IO.Unsafe" ]
            <> [ T.pack n
               | n <- maybe [] pure tmName
               , anchorArg /= Nothing
               , a /= pdNow </> fromMaybe "" anchorArg ]
    let runChain =
          firstRight scopeRetry
            [ fmap (fmap (a,)) (ideEvalExprIn s (EvalArgs a (importsFor a) True) expr)
            | a <- anchors ]
    r <- runChain
    -- Cross-component transient (the Mutation-scenario family):
    -- when a component is discovered mid-action, the graph restart
    -- aborts the warm pass, an interface file is read from a stale
    -- cache-hash dir ("withBinaryFile: does not exist"), and the
    -- eval surfaces GHC-47808. One warm-and-retry lets the restarted
    -- graph settle (the second pass finds the .hi written under the
    -- current hash).
    r' <- case r of
      Just (Left err)
        | evClass err == ECInterfaceCache -> do
            warmAnchors s anchors
            runChain
      _ -> pure r
    case r' of
      Nothing -> do
        -- Honest failure: re-run the LAST anchor once (no retry) to
        -- surface the real error instead of a generic "could not
        -- resolve" — the chain's per-anchor errors are otherwise
        -- discarded on exhaustion and the agent is left blind.
        lastErr <- case reverse anchors of
          (a : _) -> either evText (const "")
            <$> ideEvalExprIn s (EvalArgs a ["Test.QuickCheck", "System.IO.Unsafe"] True)
                                  (qcExpr prop runs)
          [] -> pure "no anchor modules found under src/ or test/"
        pure (Env.mkUnavailable (mkErrorEnvelope Validation
          ("could not resolve a GHC session with QuickCheck for this property — "
             <> T.take 400 lastErr)))
      Just (Left err) ->
        -- Parity with the legacy QcUnparsed surface: a property that
        -- fails to compile must carry the compiler output in 'hint'
        -- — without it the agent sees raw="" and zero explanation.
        -- B-6: a missing Arbitrary instance gets its honest taxonomy
        -- (missing_instance, not compile_error) so the nextStep
        -- steering routes to the arbitrary-template action.
        let missingArb = case evClass err of
              ECMissingInstance cls -> cls == "arbitrary"
              _                     -> False
            kind
              | missingArb = MissingInstance
              | otherwise  = budgetErrorKind err
            state
              | missingArb = ("missing_instance" :: Text)
              | otherwise  = "unparsed"
            payload =
              object
                [ "action" .= ("check" :: Text)
                , "property" .= prop
                , "state" .= state
                , "hint" .= evText err
                , "backend" .= ("ghcide" :: Text)
                ]
            steer
              | missingArb =
                  " Generate an instance template via ghc_property(action=arbitrary)."
              | otherwise = ""
        in pure
          ( (mkFailed (mkErrorEnvelope kind (evText err <> steer)))
              { Env.reResult = Just payload }
          )
      Just (Right (winner, out)) -> do
        -- Product contract parity with the legacy backend: a pass
        -- persists the law into the project's property store so
        -- ghc_property(action="run") / property_store(action="run")
        -- can replay it later. The recorded module hint is the
        -- caller's module argument when present.
        when (qcWorstOf runs out == "passed") $ do
          store <- readIORef storeRef
          -- The store records where the property actually RESOLVED
          -- (the winning anchor = its definition site) — a caller's
          -- module hint may point elsewhere and break replay (the
          -- scope-fix contract: prop defined in test/Spec.hs).
          let defSite = T.pack (relativizeTo pdNow winner)
          saveCases store prop (Just defSite) (qcNOf runs out)
        pure (qcResponse prop runs out)
  where
    objOf (Data.Aeson.Object o) = o
    objOf _ = KM.empty

-- | The compiled QuickCheck expression. Single run renders
-- @STATE|s|N|n|OUT|o@; @runs >= 2@ renders @RUN;…@ entries — parsed
-- by 'qcResponse'.
qcExpr :: Text -> Int -> Text
qcExpr prop runs =
  let renderR =
        "concat [case r of {"
        <> "Test.QuickCheck.Success{} -> \"passed\";"
        <> "Test.QuickCheck.Failure{} -> \"failed\";"
        <> "Test.QuickCheck.GaveUp{} -> \"gave_up\";"
        <> "_ -> \"exception\"}"
        <> ", \"|N|\", show (Test.QuickCheck.numTests r)"
        <> ", \"|OUT|\", Test.QuickCheck.output r]"
      qc = "Test.QuickCheck.quickCheckWithResult Test.QuickCheck.stdArgs (" <> prop <> ")"
  in if runs < 2
       then "System.IO.Unsafe.unsafePerformIO (" <> qc <> " >>= \\r -> return (" <> renderR <> "))"
       else "System.IO.Unsafe.unsafePerformIO (mapM (const (" <> qc <> ")) [1 :: Int .."
            <> T.pack (show runs)
            <> "] >>= \\rs -> return (concatMap (\\r -> concat [\"RUN;\", " <> renderR <> "]) rs))"

-- | Parse the rendered QuickCheck output into the legacy payload
-- shape (@state@ / @passed@ / @counterexample@ / @runs@ / @stable@).
qcResponse :: Text -> Int -> Text -> ToolResponse
qcResponse prop runs out =
  let entries
        | runs >= 2 = map parseRun' (filter (not . T.null) (T.splitOn "RUN;" out))
        | otherwise = [parseRun' out]
      states = [st | (st, _, _) <- entries]
      stable = length (distinct states) <= 1
      worst = qcWorstOf runs out
      n = qcNOf runs out
      cex = case [o | (st, _, Just o) <- entries, st == "failed"] of
        (o : _) -> T.strip (cexOf o)
        [] -> ""
      base =
        [ "action" .= ("check" :: Text)
        , "property" .= prop
        , "state" .= worst
        , "passed" .= n
        , "backend" .= ("ghcide" :: Text)
        ]
      withRuns =
        if runs >= 2
          then base <> [ "runs" .= runs
                       , "stable" .= stable
                       , "summary" .= (T.pack (show n) <> " runs passed"
                          <> (if stable then "" else " (unstable)" :: Text))
                       ]
          else base
      withCex =
        if worst == "failed" && not (T.null cex)
          then withRuns <> ["counterexample" .= cex]
          else withRuns
  in if worst == "passed"
       then Env.mkOk (object withCex)
       else Env.mkFailed
            (mkErrorEnvelope (qcErrorKind worst)
               ("Property " <> worst <> " — ghcide backend"))
            & \r -> r { Env.reResult = Just (object withCex) }
  where
    distinct = foldr (\x acc -> if x `elem` acc then acc else x : acc) []
    cexOf o = case T.lines o of
      (_ : rest) -> T.unlines rest
      [] -> o

-- | Worst QuickCheck state across all RUN; entries (shared by the
-- check route and the regression replay).
qcWorstOf :: Int -> Text -> Text
qcWorstOf runs out =
  let entries
        | runs >= 2 = map parseRun' (filter (not . T.null) (T.splitOn "RUN;" out))
        | otherwise = [parseRun' out]
      states = [st | (st, _, _) <- entries]
  in pickWorst states

pickWorst :: [Text] -> Text
pickWorst states
  | "failed" `elem` states = "failed"
  | "exception" `elem` states = "exception"
  | "gave_up" `elem` states = "gave_up"
  | otherwise = "passed"

-- | Case count of the first RUN; entry (numTests).
qcNOf :: Int -> Text -> Int
qcNOf runs out =
  let entries
        | runs >= 2 = map parseRun' (filter (not . T.null) (T.splitOn "RUN;" out))
        | otherwise = [parseRun' out]
  in case [n' | (_, Just n', _) <- entries] of
       (x : _) -> x
       [] -> 0

parseRun' :: Text -> (Text, Maybe Int, Maybe Text)
parseRun' t =
  let (st, rest1) = splitOnce' "|N|" t
      (nTxt, rest2) = splitOnce' "|OUT|" rest1
  in (T.strip st, readMaybeInt' nTxt, Just rest2)

splitOnce' :: Text -> Text -> (Text, Text)
splitOnce' sep t = case T.breakOn sep t of
  (a, b) | T.null b -> (t, "")
         | otherwise -> (a, T.drop (T.length sep) b)

readMaybeInt' :: Text -> Maybe Int
readMaybeInt' t = case reads (T.unpack (T.strip t)) of
  [(x, "")] -> Just x
  _         -> Nothing

-- | Map a QuickCheck verdict onto the envelope taxonomy.
qcErrorKind :: Text -> ErrorKind
qcErrorKind "failed" = CompileError
qcErrorKind "exception" = TypeError
qcErrorKind _ = Validation

--------------------------------------------------------------------------------
-- ghc_property(action=run) — regression replay via the session
--------------------------------------------------------------------------------

-- | Replay every persisted property through the ghcide session —
-- parity with the legacy Regression route (which replays via a
-- cabal v2-repl subprocess). Response shape is the legacy
-- @runResult@: action/total/passed/regressions/load_failed/summary,
-- where a property that could not even resolve a scope lands in
-- 'load_failed' rather than counting as a regression.
handlePropertyRun :: IORef ProjectDir -> IORef Store -> IdeSession -> IO ToolResponse
handlePropertyRun pdRef storeRef s = do
  store <- readIORef storeRef
  props <- loadAll store
  if null props
    then pure
      ( mkOk
          ( object
              [ "action" .= ("run" :: Text)
              , "total" .= (0 :: Int)
              , "passed" .= (0 :: Int)
              , "regressions" .= ([] :: [Value])
              , "load_failed" .= ([] :: [Value])
              , "summary" .= ("Replayed 0 stored properties." :: Text)
              ]
          )
      )
    else do
      results <- mapM (replayProp pdRef s) props
      let regressions =
            [ object
                [ "expression" .= spExpression p
                , "module" .= spModule p
                , "outcome" .= object ["state" .= st]
                ]
            | (p, ReplayRegressed st) <- results
            ]
          loadFailed =
            [ object
                [ "expression" .= spExpression p
                , "module" .= spModule p
                , "outcome" .= object
                    [ "state" .= ("load_failed" :: Text)
                    , "error" .= err
                    ]
                ]
            | (p, ReplayLoadFailed err) <- results
            ]
          total = length props
          regressed = length regressions
          loadFailures = length loadFailed
          passed = total - regressed - loadFailures
          success = regressed == 0 && loadFailures == 0
          summary =
            "Replayed "
              <> T.pack (show total)
              <> " stored properties: "
              <> T.pack (show passed)
              <> " passed, "
              <> T.pack (show regressed)
              <> " regressed"
              <> (if loadFailures > 0 then ", " <> T.pack (show loadFailures) <> " failed to load" else "")
              <> "."
          payload =
            object
              [ "action" .= ("run" :: Text)
              , "total" .= total
              , "passed" .= passed
              , "regressions" .= regressions
              , "load_failed" .= loadFailed
              , "summary" .= summary
              ]
      if success
        then pure (mkOk payload)
        else pure
          ( (mkFailed (mkErrorEnvelope Validation summary))
              { Env.reResult = Just payload }
          )

-- | Replay one stored property. 'Right (Just n)' = passed n cases;
-- 'Right Nothing' = no anchor could even scope the property
-- (load_failed); 'ReplayRegressed' = it ran and regressed.
replayProp :: IORef ProjectDir -> IdeSession -> StoredProperty -> IO (StoredProperty, ReplayOutcome)
replayProp pdRef s p = do
  let prop = spExpression p
      anchorArg = T.unpack <$> spModule p
  anchors <- anchorCandidates pdRef anchorArg
  warmAnchors s anchors
  r <- firstRight scopeRetry
    [ ideEvalExprIn s (EvalArgs a ["Test.QuickCheck", "System.IO.Unsafe"] True) (qcExpr prop 1)
    | a <- anchors
    ]
  case r of
    Nothing ->
      pure (p, ReplayLoadFailed
        "could not resolve a GHC session with QuickCheck for this property")
    Just (Left err) -> pure (p, ReplayLoadFailed (evText err))
    Just (Right out) ->
      let worst = qcWorstOf 1 out
      in if worst == "passed"
           then pure (p, ReplayPassed (qcNOf 1 out))
           else pure (p, ReplayRegressed worst)

-- | Strip the project-root prefix for response-facing paths.
relativizeTo :: FilePath -> FilePath -> FilePath
relativizeTo root fp =
  let root' = if last root == '/' then root else root <> "/"
  in if root' `isPrefixOf` fp then drop (length root') fp else fp
