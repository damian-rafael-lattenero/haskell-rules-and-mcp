-- | Flow: cross-validation of @ghc_check_project@ against @cabal build@.
--
-- The oracle for every other scenario is "whatever the MCP returns".
-- That works for describing behaviour but fails the user's lens —
-- if the MCP and GHC disagree, no test currently catches it. The
-- fix is to spawn @cabal build@ directly for each test project and
-- compare the exit code against the MCP's @ghc_check_project@
-- overall verdict. That gives us an independent oracle.
--
-- The flow per project:
--
--   1. Scaffold via the MCP (ghc_create_project + add_modules).
--   2. Write the source files directly to disk.
--   3. Ask the MCP: @ghc_check_project@.
--   4. Ask cabal: @cabal build --ghc-options=-Werror all@ from the
--      project directory.
--   5. The two verdicts must AGREE:
--        cabal exit=0   <=>   MCP overall=true
--        cabal exit!=0  <=>   MCP overall=false
--
-- Any drift is a bug — either the MCP is lying about green/red
-- (worse: shipping broken code), or the MCP is pessimistic
-- (blocking good code). Both are real failures.
--
-- Project set (deliberately small — cabal build is slow):
--
--   * /happy/      — module compiles clean. Cabal=OK, MCP=OK.
--   * /typeError/  — type mismatch. Cabal=FAIL, MCP=FAIL.
--   * /multiModule/ — A imports B, both compile. Cabal=OK, MCP=OK.
module Scenarios.FlowCrossValidation
  ( runFlow
  ) where

import Control.Exception (bracket, SomeException, try)
import Data.Aeson (Value (..), object, (.=))
import qualified Data.Aeson.Key as Key
import qualified Data.Aeson.KeyMap as KeyMap
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.IO as TIO
import System.Directory (createDirectoryIfMissing)
import System.Environment (getEnvironment)
import System.Exit (ExitCode (..))
import System.FilePath ((</>))
import System.Process (readCreateProcessWithExitCode, proc, CreateProcess (..))

import E2E.Assert
  ( Check (..)
  , checkPure
  , liveCheck
  , stepFooter
  , stepHeader
  )
import qualified E2E.Client as Client
import E2E.Envelope (fieldBool)
import HaskellFlows.Mcp.ToolName (ToolName (..))

--------------------------------------------------------------------------------
-- test projects
--------------------------------------------------------------------------------

-- | One test project is a name + a list of (modulePath, sourceBody)
-- + whether we EXPECT it to compile. The source is written to
-- @projectDir/modulePath@ verbatim.
data TestProject = TestProject
  { tpName            :: !Text
  , tpModules         :: ![Text]              -- module names as declared to add_modules
  , tpFiles           :: ![(FilePath, Text)]  -- (relative path, body)
  , tpExpectedCompile :: !Bool
  }

projects :: [TestProject]
projects =
  [ TestProject
      { tpName = "happy"
      , tpModules = ["Foo"]
      , tpFiles =
          [ ( "src/Foo.hs"
            , T.unlines
                [ "module Foo where"
                , ""
                , "inc :: Int -> Int"
                , "inc x = x + 1"
                ]
            )
          ]
      , tpExpectedCompile = True
      }
  , TestProject
      { tpName = "typeError"
      , tpModules = ["Bad"]
      , tpFiles =
          [ ( "src/Bad.hs"
            , T.unlines
                [ "module Bad where"
                , ""
                , "f :: Int -> Int"
                , "f x = show x     -- type mismatch"
                ]
            )
          ]
      , tpExpectedCompile = False
      }
  , TestProject
      { tpName = "multiModule"
      , tpModules = ["A", "B"]
      , tpFiles =
          [ ( "src/A.hs"
            , T.unlines
                [ "module A (answer) where"
                , ""
                , "answer :: Int"
                , "answer = 42"
                ]
            )
          , ( "src/B.hs"
            , T.unlines
                [ "module B where"
                , ""
                , "import A (answer)"
                , ""
                , "announce :: Int"
                , "announce = answer + 1"
                ]
            )
          ]
      , tpExpectedCompile = True
      }
  ]

--------------------------------------------------------------------------------
-- flow
--------------------------------------------------------------------------------

runFlow :: Client.McpClient -> FilePath -> IO [Check]
runFlow _inheritedClient projectDir = do
  -- Each sub-project needs its own HASKELL_PROJECT_DIR, and the
  -- Server captures that at construction. We allocate a fresh
  -- client per project and close it before moving on, so state
  -- never leaks. The outer client passed by Main.hs is unused.
  eBin <- try Client.findMcpBinaryPath
            :: IO (Either SomeException FilePath)
  case eBin of
    Left ex -> do
      t <- stepHeader 1 "skipped · could not resolve haskell-flows-mcp binary"
      cSkip <- liveCheck $ checkPure
        "cross-validation · binary path resolves for per-project clients"
        False
        ("Set HASKELL_FLOWS_MCP_BIN or run under cabal. Error: "
          <> T.pack (show ex))
      stepFooter 1 t
      pure [cSkip]
    Right binary -> do
      allChecks <- mapM (runOneProject binary projectDir) projects
      pure (concat allChecks)

-- | Run one project end-to-end, emitting exactly 2 checks:
--   * sanity: cabal's verdict matches the project's declared
--     expectation (the scenario is miswired if cabal disagrees).
--   * cross:  MCP and cabal agree on green/red (the headline oracle).
runOneProject :: FilePath -> FilePath -> TestProject -> IO [Check]
runOneProject binary rootDir tp = do
  t0 <- stepHeader 1 ("cross-validation · " <> tpName tp)

  let subdir = rootDir </> T.unpack ("xv-" <> tpName tp)
  createDirectoryIfMissing True subdir

  -- Per-project client so HASKELL_PROJECT_DIR points at this subdir.
  bracket
    (Client.newClient binary [("HASKELL_PROJECT_DIR", subdir)])
    Client.close
    $ \c -> do
      _ <- Client.callTool c GhcCreateProject
             (object [ "name" .= ("xv-demo" :: Text) ])
      _ <- Client.callTool c GhcAddModules
             (object [ "modules" .= tpModules tp ])

      createDirectoryIfMissing True (subdir </> "src")
      mapM_ (\(rel, body) -> TIO.writeFile (subdir </> rel) body)
            (tpFiles tp)

      mcpR <- Client.callTool c GhcCheckProject (object [])
      let mcpOverall = fieldBool "overall" mcpR

      cabalExit <- runCabalBuild subdir

      let cabalOk = cabalExit == ExitSuccess
          mcpOk   = mcpOverall == Just True
          agree   = cabalOk == mcpOk

      cSanity <- liveCheck $ checkPure
        ("sanity · project '" <> tpName tp
          <> "' behaves as declared (cabal = expected)")
        (cabalOk == tpExpectedCompile tp)
        ("The scenario's own expectation disagrees with cabal. Fix the \
         \test project, not the MCP. cabalExit="
         <> T.pack (show cabalExit)
         <> "  expected=" <> T.pack (show (tpExpectedCompile tp)))

      cCross <- liveCheck $ checkPure
        ("cross · project '" <> tpName tp
          <> "' — MCP and cabal agree on green/red")
        agree
        ("THE TWO ORACLES DISAGREE. cabalExit="
         <> T.pack (show cabalExit)
         <> "  (cabalOk=" <> T.pack (show cabalOk) <> ")"
         <> "  MCP overall=" <> T.pack (show mcpOverall)
         <> "  raw MCP: " <> truncRender mcpR)

      stepFooter 1 t0
      pure [cSanity, cCross]

--------------------------------------------------------------------------------
-- cabal oracle
--------------------------------------------------------------------------------

-- | Run @cabal build all@ inside @dir@. Shares the default
-- @dist-newstyle/@ with the ambient toolchain: the oracle doesn't
-- need a private build dir because MCP's own compilation now
-- lands in @dist-newstyle-mcp/@ (see 'CabalBootstrap.bootstrapOne'
-- where @cabal v2-repl --builddir=dist-newstyle-mcp@ keeps the
-- defer-flag-poisoned interfaces out of the user's build tree).
runCabalBuild :: FilePath -> IO ExitCode
runCabalBuild dir = do
  currentEnv <- getEnvironment
  let cp = (proc "cabal" ["build", "all"])
             { env = Just currentEnv
             , cwd = Just dir
             }
  (ec, _out, _err) <- readCreateProcessWithExitCode cp ""
  pure ec

--------------------------------------------------------------------------------
-- helpers
--------------------------------------------------------------------------------

truncRender :: Value -> Text
truncRender v =
  let raw = T.pack (show v)
      cap = 400
  in if T.length raw > cap then T.take cap raw <> "…(truncated)" else raw
