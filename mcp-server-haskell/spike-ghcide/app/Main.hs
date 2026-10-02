{-# LANGUAGE ImpredicativeTypes #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

-- | F0 spike: prove that ghcide-as-library gives the MCP what its own
-- hand-rolled GHC session could not:
--
--   A — embed IdeState without LSP; force real type-error diagnostics.
--   B — per-component sessions: QuickCheck visible from the *test-suite*
--       component and NOT from the *library* component (the differential
--       proof that hie-bios/cabal component resolution works — the
--       documented blocker of GHC-API-rewrite-plan.md).
--   C — in-process execution against the component-correct HscEnv:
--       compileExpr + run under a wall-clock timeout.
module Main (main) where

import Control.Concurrent.STM (atomically)
import Control.Exception (SomeException, try)
import Control.Monad.IO.Class (liftIO)
import Data.IORef
import Data.List (isInfixOf)
import Data.Time.Clock (UTCTime, diffUTCTime, getCurrentTime)
import Development.IDE (runAction, use)
import Development.IDE.GHC.Util (evalGhcEnv)
import Development.IDE.Core.RuleTypes (GhcSessionDeps (..), TypeCheck (..))
import Development.IDE.Core.Shake (IdeState, getDiagnostics)
import Development.IDE.Main (Arguments (..), Command (..), IdeCommand (..), Log, defaultArguments, defaultMain)
import Development.IDE.Types.Diagnostics (Diagnostic (..), DiagnosticSeverity (..), FileDiagnostic (..))
import Development.IDE.Types.HscEnvEq (HscEnvEq (hscEnv))
import Development.IDE.Types.Location (NormalizedFilePath, toNormalizedFilePath')
import GHC
  ( InteractiveImport (IIDecl)
  , compileExpr
  , mkModuleName
  , runGhc
  , setContext
  , setSession
  , simpleImportDecl
  )
import GHC.Paths (libdir)
import Ide.Logger (Doc, Recorder, WithPriority, cmapWithPrio, makeDefaultStderrRecorder, pretty)
import Ide.Types (IdePlugins)
import System.Directory (makeAbsolute)
import System.Environment (getArgs)
import System.Exit (exitFailure, exitSuccess)
import System.FilePath ((</>))
import System.IO (hSetBuffering, stdout, BufferMode (LineBuffering))
import System.Timeout (timeout)
import qualified Data.Text as T
import Unsafe.Coerce (unsafeCoerce)

toN :: FilePath -> NormalizedFilePath
toN = toNormalizedFilePath'

elapsed :: UTCTime -> UTCTime -> String
elapsed t0 t1 = show (realToFrac (diffUTCTime t1 t0) :: Double) ++ "s"

-- | Compile a String-valued expression in the context of an HscEnvEq.
-- The expression must be pure (no IO) and render through 'show' at the
-- call site (wrap as: "show (...)").
evalPureIn :: HscEnvEq -> [String] -> String -> IO (Either String String)
evalPureIn eq importMods expr =
  fmap joinTimeout (timeout 30_000_000 runIt)
  where
    joinTimeout Nothing = Left "TIMEOUT after 30s"
    joinTimeout (Just r) = r
    runIt = do
      r <- try go :: IO (Either SomeException String)
      pure (either (Left . ("EXC: " ++) . show) Right r)
    go = evalGhcEnv (hscEnv eq) $ do
      setContext (map mkIIDecl (importMods ++ ["Prelude"]))
      hv <- compileExpr expr
      pure (unsafeCoerce hv :: String)

mkIIDecl :: String -> InteractiveImport
mkIIDecl m = IIDecl (simpleImportDecl (mkModuleName m))

-- | Compile an IO String-valued expression and run it (proves runtime
-- execution, not just typechecking).
evalIOIn :: HscEnvEq -> String -> String -> IO (Either String String)
evalIOIn eq importMod expr =
  fmap joinTimeout (timeout 30_000_000 runIt)
  where
    joinTimeout Nothing = Left "TIMEOUT after 30s"
    joinTimeout (Just r) = r
    runIt = do
      r <- try go :: IO (Either SomeException String)
      pure (either (Left . ("EXC: " ++) . show) Right r)
    go = do
      act <- evalGhcEnv (hscEnv eq) $ do
        setContext
          [ IIDecl (simpleImportDecl (mkModuleName importMod))
          , IIDecl (simpleImportDecl (mkModuleName "Prelude"))
          ]
        hv <- compileExpr expr
        pure (unsafeCoerce hv :: IO String)
      act

-- | Error-severity messages for a given file.
errsIn :: [FileDiagnostic] -> NormalizedFilePath -> [String]
errsIn diags f =
  [ T.unpack (_message d)
  | fd <- diags
  , fdFilePath fd == f
  , let d = fdLspDiagnostic fd
  , _severity d == Just DiagnosticSeverity_Error
  ]

sectionA :: FilePath -> IdeState -> IO Bool
sectionA root ide = do
  putStrLn "== [A] embed IdeState + diagnostics"
  t0 <- getCurrentTime
  let goodF = toN (root </> "src/SpikeLib.hs")
      brokenF = toN (root </> "src/Broken.hs")
  _ <- runAction "spike-a-good" ide (use TypeCheck goodF)
  diags1 <- atomically (getDiagnostics ide)
  _ <- runAction "spike-a-broken" ide (use TypeCheck brokenF)
  diags2 <- atomically (getDiagnostics ide)
  t1 <- getCurrentTime
  let goodErrs = errsIn diags1 goodF
      brokenErrs = errsIn diags2 brokenF
  putStrLn ("   SpikeLib.hs errors: " ++ show (length goodErrs))
  putStrLn ("   Broken.hs  errors: " ++ show (length brokenErrs))
  mapM_ (putStrLn . ("     " ++) . take 160) (take 2 brokenErrs)
  putStrLn ("   A wall time (2 typechecks): " ++ elapsed t0 t1)
  let ok = null goodErrs && not (null brokenErrs)
  putStrLn ("   [A] " ++ passFail ok)
  pure ok

sectionBC :: FilePath -> IdeState -> IO (Bool, Bool)
sectionBC root ide = do
  putStrLn "== [B] per-component sessions (the differential proof)"
  mTestEq <- runAction "spike-b-test" ide (use GhcSessionDeps (toN (root </> "test/Spec.hs")))
  mLibEq <- runAction "spike-b-lib" ide (use GhcSessionDeps (toN (root </> "src/SpikeLib.hs")))
  case (mTestEq, mLibEq) of
    (Nothing, _) -> do
      putStrLn "   could not obtain test-suite HscEnvEq"
      putStrLn "   [B] FAIL — component resolution for test/Spec.hs failed"
      pure (False, False)
    (_, Nothing) -> do
      putStrLn "   could not obtain library HscEnvEq"
      putStrLn "   [B] FAIL — component resolution for src/SpikeLib.hs failed"
      pure (False, False)
    (Just testEq, Just libEq) -> do
      putStrLn "   both components resolved; asking QuickCheck in each:"
      let qcExpr =
            "show (Test.QuickCheck.getNonNegative (Test.QuickCheck.NonNegative (5 :: Int)))"
          qcMods = ["Test.QuickCheck"]
      qcT <- evalPureIn testEq qcMods qcExpr
      qcL <- evalPureIn libEq qcMods qcExpr
      putStrLn ("   QuickCheck from TEST component:  " ++ take 120 (either ("ERR: " ++) ("OK: " ++) qcT))
      putStrLn ("   QuickCheck from LIB  component:  " ++ take 120 (either id ("UNEXPECTED OK: " ++) qcL))
      let bOk = isRight qcT && isLeft qcL
      putStrLn ("   [B] " ++ passFail bOk ++ " — component-scoped package visibility proven")

      putStrLn "== [C] in-process execution (compileExpr + run, 30s budget)"
      plainL <- evalPureIn libEq ["Prelude"] "show (1 + 1 :: Int)"
      ioT <-
        evalIOIn
          testEq
          "Test.QuickCheck"
          "Test.QuickCheck.sample' (Test.QuickCheck.arbitrary :: Test.QuickCheck.Gen Int) >>= \\xs -> pure (show (take 3 xs))"
      putStrLn ("   eval 1+1 in LIB session:         " ++ either ("ERR: " ++) id plainL)
      putStrLn ("   IO run in TEST session (QC):     " ++ either ("ERR: " ++) id ioT)
      let cOk = plainL == Right "2" && isRight ioT
      putStrLn ("   [C] " ++ passFail cOk ++ " — pure eval + IO execution with timeout")
      pure (bOk, cOk)
  where
    isRight (Right _) = True
    isRight _ = False
    isLeft (Left _) = True
    isLeft _ = False

passFail :: Bool -> String
passFail True = "PASS"
passFail False = "FAIL"

main :: IO ()
main = do
  hSetBuffering stdout LineBuffering
  args <- getArgs
  rootRel <- case args of
    [r] -> pure r
    _ -> fail "usage: spike-ghcide <abs-or-rel path to target project>"
  root <- makeAbsolute rootRel
  t0 <- getCurrentTime
  recorderDoc <- makeDefaultStderrRecorder Nothing
  let recorder :: Recorder (WithPriority Log)
      recorder = cmapWithPrio (pretty :: Log -> Doc ()) recorderDoc
      plugins :: IdePlugins IdeState
      plugins = mempty
      baseArgs = defaultArguments recorder root plugins
      spike :: IdeState -> IO ()
      spike ide = do
        tBoot <- getCurrentTime
        putStrLn ("== IdeCommand reached; boot: " ++ elapsed t0 tBoot)
        aOk <- sectionA root ide
        (bOk, cOk) <- sectionBC root ide
        tEnd <- getCurrentTime
        putStrLn ("== total spike wall time: " ++ elapsed t0 tEnd)
        let verdict = aOk && bOk && cOk
        putStrLn
          ( "== F0 GATE: "
              ++ passFail verdict
              ++ " (A="
              ++ passFail aOk
              ++ " B="
              ++ passFail bOk
              ++ " C="
              ++ passFail cOk
              ++ ")"
          )
        if verdict then exitSuccess else exitFailure
      args = baseArgs {argCommand = Custom (IdeCommand spike)}
  defaultMain recorder args
