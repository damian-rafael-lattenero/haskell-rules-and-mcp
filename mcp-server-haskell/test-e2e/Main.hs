-- | Entry point for the E2E test-suite.
--
-- Two layers:
--
--   1. 'runSmoke' — a single subprocess round-trip that proves
--      the real @haskell-flows-mcp@ binary answers @initialize@ +
--      @tools/list@ over stdio. Covers the transport layer. Fast
--      (~100ms).
--
--   2. The expr-evaluator scenario — in-process 'dispatchTool'
--      calls against a 'Server' pointed at a temp project dir.
--      Covers every tool's behaviour + the post-BUG-* invariants.
--
-- Both layers share the same progress-streaming log format
-- ('E2E.Assert.stepHeader' + 'liveCheck' + 'stepFooter') so you
-- can watch the scenario advance line-by-line — no mystery hangs.
module Main where

import Control.Exception (bracket, try, SomeException)
import qualified Data.Text as T
import Data.Time.Clock.POSIX (getPOSIXTime)
import System.Directory (getTemporaryDirectory, removePathForcibly, createDirectoryIfMissing)
import System.Exit (exitFailure, exitSuccess)
import System.FilePath ((</>))
import System.IO (BufferMode (..), hSetBuffering, stderr, stdout)

import qualified E2E.Assert as Assert
import qualified E2E.Client as Client
import qualified E2E.Smoke  as Smoke
import qualified Scenarios.ExprEvaluator as Expr

main :: IO ()
main = do
  hSetBuffering stdout LineBuffering
  hSetBuffering stderr LineBuffering
  binary <- Client.findMcpBinaryPath
  putStrLn "==> haskell-flows-mcp e2e"
  putStrLn ("==> binary: " <> binary)
  -- Layer 1: transport smoke. Confirms the binary speaks MCP
  -- over stdio end-to-end. If this fails, everything downstream
  -- is moot — short-circuit.
  Assert.beginSection "Transport smoke (subprocess, 1 round-trip)"
  t0 <- Assert.stepHeader 0 "initialize + initialized + tools/list"
  smoke <- Smoke.runSmoke binary
  _ <- Assert.liveCheck (Assert.Check
         { Assert.cName   = "binary answers tools/list with ≥ 1 tool"
         , Assert.cOk     = Smoke.srPassed smoke
         , Assert.cDetail = T.pack ("smoke log: " <> Smoke.srLog smoke)
         })
  Assert.stepFooter 0 t0

  if not (Smoke.srPassed smoke)
    then do
      putStrLn ""
      putStrLn ("FAIL: transport smoke failed (log: "
                <> Smoke.srLog smoke <> ")")
      exitFailure
    else do
      -- Layer 2: in-process scenario against a live Server.
      withTempProjectDir $ \projectDir -> do
        wallStart <- getPOSIXTime
        res <- try $ bracket
          (Client.newClient binary [("HASKELL_PROJECT_DIR", projectDir)])
          Client.close
          (`Expr.runExprScenario` projectDir)
        wallEnd <- getPOSIXTime
        let secs = (realToFrac (wallEnd - wallStart) :: Double)
        case res of
          Left (e :: SomeException) -> do
            putStrLn ""
            putStrLn ("E2E framework error: " <> show e)
            exitFailure
          Right checks -> do
            let passed = length (filter Assert.cOk checks)
                total  = length checks
                fails  = filter (not . Assert.cOk) checks
            putStrLn ""
            putStrLn "════════════════════════════════════════════════════════"
            putStrLn ("  " <> show passed <> " / " <> show total
                       <> " scenario checks passed in "
                       <> formatSecs secs <> " s")
            mapM_ (\c -> putStrLn ("  · FAIL " <> takeLine (show (Assert.cName c))))
                  fails
            putStrLn "════════════════════════════════════════════════════════"
            if Assert.allPassed checks then exitSuccess else exitFailure
  where
    formatSecs :: Double -> String
    formatSecs x =
      let r = round (x * 100) :: Int
          whole = r `div` 100
          frac  = r `mod` 100
          fracS = if frac < 10 then "0" <> show frac else show frac
      in show whole <> "." <> fracS
    takeLine = takeWhile (/= '\n')

-- | Create a unique temp project dir, run the action with it,
-- delete it on exit (success or failure). No leaked
-- dist-newstyle / .haskell-flows clutter under $TMPDIR.
withTempProjectDir :: (FilePath -> IO a) -> IO a
withTempProjectDir k = do
  base <- getTemporaryDirectory
  ts   <- getPOSIXTime
  let dir = base </> ("haskell-flows-e2e-" <> show (floor (ts * 1000000) :: Int))
  bracket
    (createDirectoryIfMissing True dir >> pure dir)
    removePathForcibly
    k
