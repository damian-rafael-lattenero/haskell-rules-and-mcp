-- | Flow: Issue #91 Phase A — drive the server with the
-- Claude-Code-shaped wire (every primitive stringified) against
-- each tool that migrated to PermissiveJSON in #88.
--
-- The threat model
-- ----------------
-- Real-world MCP hosts wrap the JSON-RPC bytes between the agent
-- and the server. The Claude Code wrapper systematically
-- stringifies primitives: a number @17@ goes out as @\"17\"@, a
-- bool @true@ goes out as @\"true\"@. Pre-#88 the server
-- rejected those wires with raw Aeson errors, making
-- 'ghc_refactor rename_local' completely unusable through Claude
-- Code in the dogfood pass.
--
-- This scenario closes the loop on the test surface side: the
-- 'E2E.Client' is the canonical Haskell client that serialises
-- JSON the way 'aeson' does — perfectly typed primitives every
-- time. A unit test on the parser side (Spec.hs) proves the
-- newtype accepts both forms in isolation, but doesn't witness
-- the full wire shape: tool-args envelope + RPC framing +
-- end-to-end response.
--
-- The oracle
-- ----------
-- For each of the four tools migrated in #88
-- ('ghc_refactor', 'ghc_fix_warning', 'ghc_remove_modules',
-- 'ghc_complete'), drive the server with the *stringified*
-- shape and assert the call doesn't fail at the parser
-- boundary. We bind only on the wire shape — the actual
-- semantic outcome (rename success / refactor result / etc.) is
-- already covered by other scenarios.
--
-- A failure here would manifest as @status: \"failed\"@ with
-- @error.kind: \"type_mismatch\"@ — the exact shape the dogfood
-- caught pre-#88. Post-#88 the parser accepts both wires.
module Scenarios.FlowCrossClientStringification
  ( runFlow
  ) where

import Data.Aeson (Value (..), object, (.=))
import Data.Maybe (isJust)
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.IO as TIO
import System.Directory (createDirectoryIfMissing)
import System.FilePath ((</>))

import E2E.Assert
  ( Check (..)
  , checkPure
  , liveCheck
  , stepFooter
  , stepHeader
  )
import qualified E2E.Client as Client
import E2E.Envelope (statusOk, errorKind)
import HaskellFlows.Mcp.ToolName (ToolName (..))

-- | A trivial source we can rename inside.
fooSrc :: Text
fooSrc =
  "module Foo (alpha, beta) where\n\
  \\n\
  \alpha :: Int\n\
  \alpha = 1\n\
  \\n\
  \beta :: Int\n\
  \beta = 2\n"

runFlow :: Client.McpClient -> FilePath -> IO [Check]
runFlow c projectDir = do
  ----------------------------------------------------------------
  -- setup
  ----------------------------------------------------------------
  t0 <- stepHeader 1 "scaffold + Foo + ghc_load (warm cache)"
  _ <- Client.callTool c GhcProject
         (object [ "action" .= ("create" :: Text), "name" .= ("xclient-demo" :: Text) ])
  _ <- Client.callTool c GhcModules
         (object [ "action" .= ("add" :: Text), "modules" .= (["Foo"] :: [Text]) ])
  createDirectoryIfMissing True (projectDir </> "src")
  TIO.writeFile (projectDir </> "src" </> "Foo.hs") fooSrc
  _ <- Client.callTool c GhcLoad
         (object [ "module_path" .= ("src/Foo.hs" :: Text) ])
  stepFooter 1 t0

  ----------------------------------------------------------------
  -- (1) ghc_refactor with EVERY numeric param stringified.
  --     Pre-#88 this returned a parser error and the rename
  --     never ran. Post-#88 the call must NOT fail at the
  --     boundary; whether the rename itself succeeds is a
  --     separate axis (we only pin "no parser-side rejection").
  ----------------------------------------------------------------
  t1 <- stepHeader 2 "ghc_refactor · scope_line_start/end as strings (#91/#88)"
  r1 <- Client.callTool c GhcRefactor
          (object
             [ "action"           .= ("rename_local" :: Text)
             , "module_path"      .= ("src/Foo.hs" :: Text)
             , "old_name"         .= ("alpha" :: Text)
             , "new_name"         .= ("alphaPrime" :: Text)
             , "scope_line_start" .= ("3" :: Text)   -- stringified Int
             , "scope_line_end"   .= ("4" :: Text)   -- stringified Int
             , "dry_run"          .= ("true" :: Text) -- stringified Bool
             ])
  c1 <- liveCheck $ checkPure
          "ghc_refactor accepts stringified Int/Bool params (#91/#88)"
          (errorKind r1 /= Just "type_mismatch")
          ("Pre-#88 the parser rejected stringified Int with \
           \error.kind=type_mismatch. Got error.kind=" <>
           T.pack (show (errorKind r1)) <> ". The semantic outcome \
           \(rename succeeded or not) is out of scope here; we only \
           \pin that the parser boundary doesn't reject.")
  stepFooter 2 t1

  ----------------------------------------------------------------
  -- (2) ghc_remove_modules with stringified booleans.
  --     'modules: [Foo]' makes the call self-contained: we just
  --     need the parser to accept the bool wires; whether the
  --     module is actually removed is downstream.
  ----------------------------------------------------------------
  t2 <- stepHeader 3 "ghc_remove_modules · delete_files/force as strings (#91/#88)"
  r2 <- Client.callTool c GhcModules
          (object [ "action" .= ("remove" :: Text), "modules"      .= (["NonExistent"] :: [Text])
             , "delete_files" .= ("false" :: Text)  -- stringified Bool
             , "force"        .= ("false" :: Text)  -- stringified Bool
             ])
  c2 <- liveCheck $ checkPure
          "ghc_remove_modules accepts stringified booleans (#91/#88)"
          (errorKind r2 /= Just "type_mismatch")
          ("Stringified 'false' parsed as a non-Bool would have \
           \been pre-#88's failure. Got error.kind=" <>
           T.pack (show (errorKind r2)))
  stepFooter 3 t2

  ----------------------------------------------------------------
  -- (3) ghc_fix_warning · 'line' is REQUIRED, not optional.
  --     The required-Int parser was the most user-visible win.
  ----------------------------------------------------------------
  t3 <- stepHeader 4 "ghc_fix_warning · required line as string (#91/#88)"
  r3 <- Client.callTool c GhcFixWarning
          (object
             [ "module_path" .= ("src/Foo.hs" :: Text)
             , "line"        .= ("3" :: Text)        -- stringified required Int
             , "code"        .= ("GHC-66111" :: Text)
             , "apply"       .= ("false" :: Text)    -- stringified Bool
             ])
  c3 <- liveCheck $ checkPure
          "ghc_fix_warning accepts stringified required Int (#91/#88)"
          (errorKind r3 /= Just "type_mismatch"
            && errorKind r3 /= Just "missing_arg")
          ("Pre-#88 the required-Int parser rejected stringified '3' \
           \with type_mismatch. Got error.kind=" <>
           T.pack (show (errorKind r3)))
  stepFooter 4 t3

  ----------------------------------------------------------------
  -- (4) ghc_complete · 'limit' is optional with default 25.
  --     Stringified '10' must produce IntField 10, not the
  --     default. Verifying *behaviour* (response was capped at
  --     10 entries) is downstream; we only pin "no parser
  --     rejection".
  ----------------------------------------------------------------
  t4 <- stepHeader 5 "ghc_complete · optional limit as string (#91/#88)"
  r4 <- Client.callTool c GhcComplete
          (object
             [ "prefix" .= ("sho" :: Text)
             , "limit"  .= ("10" :: Text)  -- stringified Int (optional)
             ])
  c4 <- liveCheck $ checkPure
          "ghc_complete accepts stringified limit (#91/#88)"
          ( errorKind r4 /= Just "type_mismatch"
              && isJust (statusOk r4)  -- some response shape exists
          )
          ("Pre-#88 the optional-Int parser silently fell back to \
           \the default 25 when given stringified '10'. Got \
           \error.kind=" <> T.pack (show (errorKind r4)))
  stepFooter 5 t4

  pure [c1, c2, c3, c4]
