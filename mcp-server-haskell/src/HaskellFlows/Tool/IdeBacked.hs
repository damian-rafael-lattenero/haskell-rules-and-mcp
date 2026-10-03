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
  ) where

import Control.Concurrent.MVar (MVar, modifyMVar)
import Control.Exception (SomeException, try)
import Control.Monad (void)
import Data.Aeson (Value, object, withObject, (.=), (.:))
import qualified Data.Aeson
import Data.Maybe (fromMaybe)
import qualified Data.Aeson.KeyMap as KM
import Data.Aeson.Key (Key)
import Data.Aeson.Types (parseEither)
import Data.Function ((&))
import Data.IORef (IORef, readIORef)
import Data.List (isPrefixOf, isSuffixOf, sort)
import Data.Maybe (fromMaybe)
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.IO as TIO
import System.Directory (listDirectory)
import System.FilePath ((</>))

import HaskellFlows.Ghc.IdeSession
  ( EvalArgs (..)
  , IdeSession (..)
  , anchorModuleIn
  , bootIdeSession
  , ideDiagnosticsFor
  , ideEvalExprIn
  , ideModuleNameOf
  , ideProjectDiagnostics
  , ideTypeOfExprIn
  , projectModuleFilesFromCabal
  )
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

-- | Get-or-boot the ghcide session under the MVar (first caller boots,
-- everyone else reuses — same single-writer shape as 'srvGhcSession').
-- The handler itself runs OUTSIDE the MVar: a slow tool call must never
-- hold the boot lock (that would serialize every ghcide-backed call).
withIdeSession
  :: MVar (Maybe IdeSession)
  -> IORef ProjectDir
  -> (IdeSession -> IO ToolResponse)
  -> IO ToolResponse
withIdeSession ref pdRef k = do
  s <- modifyMVar ref $ \case
    Just s -> pure (Just s, s)
    Nothing -> do
      s <- bootIdeSession =<< readIORef pdRef
      pure (Just s, s)
  k s

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
  -> ToolName
  -> Value
  -> Maybe (IO ToolResponse)
routeIde ref pdRef tn args = case tn of
  GhcCheck
    | actionIs "module"  args -> Just (withIdeSession ref pdRef (handleCheckModule pdRef (stripped args)))
    | actionIs "load"    args -> Just (withIdeSession ref pdRef (handleCheckLoad pdRef (stripped args)))
    | actionIs "project" args -> Just (withIdeSession ref pdRef (handleCheckProject pdRef))
    | otherwise -> Nothing
  GhcEval -> Just (withIdeSession ref pdRef (handleEval args))
  GhcInspect
    | actionIs "type" args -> Just (withIdeSession ref pdRef (handleType (stripped args)))
    | otherwise -> Nothing
  GhcProperty
    | actionIs "check" args -> Just (withIdeSession ref pdRef (handlePropertyCheck pdRef (stripped args)))
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

-- | Distinguish timeouts from compile failures in the eval/type
-- envelopes — the budget tripping is not a compile error.
budgetErrorKind :: Text -> ErrorKind
budgetErrorKind err
  | "timeout" `T.isInfixOf` T.toLower err = InnerTimeout
  | otherwise = CompileError

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

-- | Run the anchor chain until one succeeds. Scope errors (module not
-- loaded / not found / name not in scope) advance to the next anchor —
-- the next candidate's module graph may import what this one lacks.
-- Everything else is a real error and stops the chain.
firstRight :: [IO (Either Text Text)] -> IO (Maybe (Either Text Text))
firstRight [] = pure Nothing
firstRight (io : rest) = do
  v <- io
  case v of
    Right txt -> pure (Just (Right txt))
    Left e
      | any (`T.isInfixOf` e)
          [ "Could not find module", "Could not load module"
          , "not loaded", "Variable not in scope"
          , "could not resolve GHC session" ]
      -> firstRight rest
    Left e -> pure (Just (Left e))

listHs :: FilePath -> IO [FilePath]
listHs dir = do
  r <- try (listDirectory dir) :: IO (Either SomeException [FilePath])
  case r of
    Left _   -> pure []
    Right es -> pure (sort [dir </> e | e <- es, ".hs" `isSuffixOf` e])

dedup :: [FilePath] -> [FilePath]
dedup = foldr (\x acc -> if x `elem` acc then acc else x : acc) []

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
  testHs <- listHs (pd </> "test")
  srcHs <- listHs (pd </> "src")
  pure (dedup (given <> testHs <> srcHs))

-- | Anchor chain for eval/type: src/ first (library names live
-- there), then test/ (imports the library).
anchorChain :: IdeSession -> IO [FilePath]
anchorChain s = do
  let root = isRoot s
  testHs <- listHs (root </> "test")
  srcHs <- listHs (root </> "src")
  pure (dedup (srcHs <> testHs))

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

loadShapeEnvelope :: Text -> Text -> [Value] -> ToolResponse
loadShapeEnvelope action mp diags =
  let errs = [d | d <- diags, isSev "error" d]
      warns = [d | d <- diags, isSev "warning" d]
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
  where
    isSev want d = KM.lookup "severity" (objOf d) == Just (Data.Aeson.String want)
    objOf (Data.Aeson.Object o) = o
    objOf _ = KM.empty

handleCheckModule :: IORef ProjectDir -> Value -> IdeSession -> IO ToolResponse
handleCheckModule pdRef raw s = case argField "module_path" raw of
  Left err -> pure (mkFailed (mkErrorEnvelope MissingArg (T.pack err)))
  Right mp -> do
    pd <- readIORef pdRef
    diags <- ideDiagnosticsFor s (unProjectDir pd </> T.unpack mp)
    pure (loadShapeEnvelope "module" mp diags)

handleCheckLoad :: IORef ProjectDir -> Value -> IdeSession -> IO ToolResponse
handleCheckLoad pdRef raw s = case argField "module_path" raw of
  Left err -> pure (mkFailed (mkErrorEnvelope MissingArg (T.pack err)))
  Right mp -> do
    pd <- readIORef pdRef
    diags <- ideDiagnosticsFor s (unProjectDir pd </> T.unpack mp)
    pure (loadShapeEnvelope "load" mp diags)

-- | F3 project gate: typecheck every module listed in the .cabal.
-- Legacy-compatible @gates.compile@ verdict + per-module rows.
handleCheckProject :: IORef ProjectDir -> IdeSession -> IO ToolResponse
handleCheckProject pdRef s = do
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
          rows <- ideProjectDiagnostics s (map (unProjectDir pd </>) mods)
          let rowsV =
                [ object
                    [ "module_path" .= T.pack (relativize (unProjectDir pd) fp)
                    , "errors" .= [d | d <- ds, isSev "error" d]
                    , "warnings" .= [d | d <- ds, isSev "warning" d]
                    ]
                | (fp, ds) <- rows
                ]
              totalErrs = sum [length (filter (isSev "error") ds) | (_, ds) <- rows]
          if totalErrs == 0
            then pure (Env.mkOk (object
                  [ "action" .= ("project" :: Text)
                  , "backend" .= ("ghcide" :: Text)
                  , "gates" .= object [ "compile" .= True ]
                  , "modules" .= rowsV
                  , "summary" .=
                      ("All " <> T.pack (show (length rows)) <> " modules compile clean." :: Text)
                  ]))
            else pure
                  (Env.mkFailed
                     ((mkErrorEnvelope CompileError
                         (T.pack (show totalErrs) <> " error(s) across project — ghcide backend"))
                     ) & \r -> r { Env.reResult = Just (object
                        [ "action" .= ("project" :: Text)
                        , "backend" .= ("ghcide" :: Text)
                        , "gates" .= object [ "compile" .= False ]
                        , "modules" .= rowsV
                        ]) })
  where
    isSev want d = KM.lookup "severity" (objOf d) == Just (Data.Aeson.String want)
    objOf (Data.Aeson.Object o) = o
    objOf _ = KM.empty
    relativize root fp =
      let root' = if last root == '/' then root else root <> "/"
      in if root' `isPrefixOf` fp then drop (length root') fp else fp

--------------------------------------------------------------------------------
-- ghc_eval / ghc_inspect(type)
--------------------------------------------------------------------------------

handleEval :: Value -> IdeSession -> IO ToolResponse
handleEval raw s = case argField "expression" raw of
  Left err -> pure (mkFailed (mkErrorEnvelope MissingArg (T.pack err)))
  Right expr -> do
    anchors <- anchorChain s
    warmAnchors s anchors
    r <- firstRight
      [ ideEvalExprIn s (EvalArgs a [] False) ("show (" <> expr <> ")")
      | a <- anchors ]
    case r of
      Nothing -> pure (mkFailed (mkErrorEnvelope MissingArg
                          "no project module provides the names this expression needs"))
      Just (Left err) -> pure (mkFailed (mkErrorEnvelope (budgetErrorKind err) err))
      Just (Right out) ->
        pure
          ( mkOk
              ( object
                  [ "output" .= out
                  , "truncated" .= False
                  , "backend" .= ("ghcide" :: Text)
                  ]
              )
          )

handleType :: Value -> IdeSession -> IO ToolResponse
handleType raw s = case argField "expression" raw of
  Left err -> pure (mkFailed (mkErrorEnvelope MissingArg (T.pack err)))
  Right expr -> do
    anchors <- anchorChain s
    warmAnchors s anchors
    r0 <- firstRight
      [ ideTypeOfExprIn s (EvalArgs a [] False) expr | a <- anchors ]
    case r0 of
      Just (Right ty) -> pure (mkOk (object ["type" .= ty, "backend" .= ("ghcide" :: Text)]))
      Just (Left err) -> pure (mkFailed (mkErrorEnvelope (budgetErrorKind err) err))
      Nothing -> do
        anchor <- anchorModuleIn s
        r <- ideTypeOfExprIn s (EvalArgs anchor [] False) expr
        case r of
          Right ty -> pure (mkOk (object ["type" .= ty, "backend" .= ("ghcide" :: Text)]))
          Left err -> pure (mkFailed (mkErrorEnvelope (budgetErrorKind err) err))

--------------------------------------------------------------------------------
-- ghc_property(action=check) — QuickCheck via the session
--------------------------------------------------------------------------------

-- | QuickCheck via the ghcide session. Compiles
-- @unsafePerformIO (quickCheckWithResult stdArgs prop)@ rendered as
-- @STATE|<state>|N|<tests>|OUT|<output>@ in the component context of
-- an anchor whose graph provides both QuickCheck and the property's
-- home module. @runs >= 2@ replays N times (@RUN;STATE|…@) and
-- reports stability — the determinism route.
handlePropertyCheck :: IORef ProjectDir -> Value -> IdeSession -> IO ToolResponse
handlePropertyCheck pdRef raw s = case argField "property" raw of
  Left err -> pure (mkFailed (mkErrorEnvelope MissingArg (T.pack err)))
  Right prop -> do
    let runs = case KM.lookup "runs" (objOf raw) of
                 Just (Data.Aeson.Number n) -> max 1 (round n)
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
    r <- firstRight
      [ ideEvalExprIn s (EvalArgs a (importsFor a) True) expr | a <- anchors ]
    case r of
      Nothing ->
        pure (Env.mkUnavailable (mkErrorEnvelope Validation
          "could not resolve a GHC session with QuickCheck for this property"))
      Just (Left err) ->
        pure (mkFailed (mkErrorEnvelope (budgetErrorKind err) err))
      Just (Right out) -> pure (qcResponse prop runs out)
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
            <> "] >>= \\rs -> return (concatMap (\\r -> \"RUN;\" : [" <> renderR <> "]) rs))"

-- | Parse the rendered QuickCheck output into the legacy payload
-- shape (@state@ / @passed@ / @counterexample@ / @runs@ / @stable@).
qcResponse :: Text -> Int -> Text -> ToolResponse
qcResponse prop runs out =
  let entries
        | runs >= 2 = map parseRun (filter (not . T.null) (T.splitOn "RUN;" out))
        | otherwise = [parseRun out]
      states = [st | (st, _, _) <- entries]
      stable = length (distinct states) <= 1
      worst
        | "failed" `elem` states = ("failed" :: Text)
        | "exception" `elem` states = "exception"
        | "gave_up" `elem` states = "gave_up"
        | otherwise = "passed"
      n = case [n' | (_, Just n', _) <- entries] of
            (x : _) -> x
            [] -> 0
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
      withRuns = if runs >= 2 then base <> ["runs" .= runs, "stable" .= stable] else base
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
    parseRun t =
      let (st, rest1) = splitOnce "|N|" t
          (nTxt, rest2) = splitOnce "|OUT|" rest1
      in (T.strip st, readMaybeInt nTxt, Just rest2)
    splitOnce sep t = case T.breakOn sep t of
      (a, b) | T.null b -> (t, "")
             | otherwise -> (a, T.drop (T.length sep) b)
    readMaybeInt t = case reads (T.unpack (T.strip t)) of
      [(x, "")] -> Just x
      _ -> Nothing
    distinct = foldr (\x acc -> if x `elem` acc then acc else x : acc) []
    cexOf o = case T.lines o of
      (_ : rest) -> T.unlines rest
      [] -> o
    qcErrorKind "failed" = CompileError
    qcErrorKind "exception" = TypeError
    qcErrorKind _ = Validation
