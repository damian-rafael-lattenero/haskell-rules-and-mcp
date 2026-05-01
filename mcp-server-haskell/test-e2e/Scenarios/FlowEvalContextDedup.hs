-- | Flow: Issue #86 — @ghc_eval@ must not grow the
-- @InteractiveContext@ import list across calls.
--
-- The bug
-- -------
-- 'augmentEvalContext' used to do
--
-- > existing <- getContext
-- > setContext (existing <> newImports)
--
-- with @newImports@ a fixed 5-element baseline. Pre-fix, every
-- 'ghc_eval' call appended another full copy of the baseline:
-- @import Prelude@ landed twice after the first eval, three times
-- after the second, and so on. Cosmetic on @ghc_imports@; the real
-- harm was unbounded growth of the scope chain that every
-- @exprType@ / @compileExpr@ walks — a long-running session paid
-- @O(n)@ per eval where @n@ tracks total prior eval calls.
--
-- The oracle
-- ----------
-- Drive @ghc_eval@ at least three times with a trivial
-- @1 + 1@-style expression (cheap, deterministic), interleaved
-- with @ghc_imports@ snapshots. After the first call, the import
-- count must NEVER grow again — a pre-fix run produced 9 →
-- 14 → 19 → … entries; a post-fix run produces a stable count
-- from call 2 onwards.
--
-- We additionally pin the duplicate-row check directly: at every
-- snapshot, no module name should appear twice in the rendered
-- @imports@ list. Pre-fix this assertion failed at the second
-- snapshot.
module Scenarios.FlowEvalContextDedup
  ( runFlow
  ) where

import Data.Aeson (Value (..), object, (.=))
import qualified Data.List as List
import Data.Maybe (fromMaybe)
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.IO as TIO
import qualified Data.Vector as V
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
import E2E.Envelope (lookupField, statusOk)
import HaskellFlows.Mcp.ToolName (ToolName (..))

-- | Trivial source so 'ghc_load' has something to work with —
-- the bug fires regardless of project size, but loading at least
-- one module exercises the post-load augment path that injects
-- the baseline imports the first time.
fooSrc :: Text
fooSrc =
  "module Foo where\n\
  \\n\
  \foo :: Int\n\
  \foo = 1\n"

runFlow :: Client.McpClient -> FilePath -> IO [Check]
runFlow c projectDir = do
  ----------------------------------------------------------------
  -- setup — minimal scaffold + one source module
  ----------------------------------------------------------------
  t0 <- stepHeader 1 "scaffold + Foo + load"
  _ <- Client.callTool c GhcProject
         (object [ "action" .= ("create" :: Text), "name" .= ("eval-ctx-demo" :: Text) ])
  _ <- Client.callTool c GhcModules
         (object [ "action" .= ("add" :: Text), "modules" .= (["Foo"] :: [Text]) ])
  createDirectoryIfMissing True (projectDir </> "src")
  TIO.writeFile (projectDir </> "src" </> "Foo.hs") fooSrc
  _ <- Client.callTool c GhcLoad
         (object [ "module_path" .= ("src/Foo.hs" :: Text) ])
  stepFooter 1 t0

  ----------------------------------------------------------------
  -- Drive 4 eval/imports cycles. Capture the import count and
  -- the imports list at each snapshot. The fix is verified by:
  --   (a) count is non-decreasing → ALWAYS true even pre-fix; we
  --       care about the negation: count must be CONSTANT from
  --       snapshot 2 onwards.
  --   (b) no module appears twice in any single snapshot — pre-fix
  --       'Prelude' showed up twice at snapshot 2, three times at
  --       snapshot 3, etc.
  ----------------------------------------------------------------
  t1 <- stepHeader 2 "4 eval/imports cycles · count stable + no dups"
  let evalOnce = do
        _ <- Client.callTool c GhcEval
               (object [ "expression" .= ("1 + 1" :: Text) ])
        Client.callTool c GhcImports (object [])
  snap1 <- evalOnce
  snap2 <- evalOnce
  snap3 <- evalOnce
  snap4 <- evalOnce

  let snaps        = [snap1, snap2, snap3, snap4]
      counts       = map importsCount snaps
      duplicates   = map duplicateNames snaps
      -- The post-fix invariant: import count is CONSTANT from
      -- snapshot 2 onwards. We allow snap1 ≠ snap2 because the
      -- very first eval may legitimately add the baseline
      -- (when 'autoLoadProject' didn't already cover all five).
      tail3        = drop 1 counts
      stableCount  = case tail3 of
                       []     -> True
                       (x:xs) -> all (== x) xs
      anyDup       = not (all null duplicates)

  c1 <- liveCheck $ checkPure
          "ghc_eval · ghc_imports.count stable from snapshot 2 onwards (#86)"
          stableCount
          ("Pre-fix the count grew on EVERY eval call. \
           \Got snapshot counts: " <> T.pack (show counts) <>
           " (need all-equal from index 1 onwards).")
  c2 <- liveCheck $ checkPure
          "ghc_eval · no module appears twice in any single imports snapshot (#86)"
          (not anyDup)
          ("Pre-fix 'Prelude' appeared 2x at snap 2, 3x at snap 3, etc. \
           \Per-snapshot duplicates: " <>
           T.pack (show duplicates))
  c3 <- liveCheck $ checkPure
          "ghc_eval · the four eval calls all returned status=ok (#86)"
          (all (\s -> statusOk s == Just True) snaps)
          "Bug-pinning prerequisite: every eval call must succeed. \
          \If any failed, the count-stability assertion is moot."
  stepFooter 2 t1

  pure [c1, c2, c3]

--------------------------------------------------------------------------------
-- helpers
--------------------------------------------------------------------------------

-- | Pull @count@ off an @ghc_imports@ response. Defaults to @-1@
-- when the field is missing — that surfaces as an obvious
-- assertion failure rather than a silent zero.
importsCount :: Value -> Int
importsCount v = case lookupField "count" v of
  Just (Number n) -> round n
  _               -> -1

-- | Project an @ghc_imports@ response down to the *bare module
-- name* of every entry, then return the names that appear more
-- than once. An empty list is the post-fix invariant.
--
-- We strip the @\"import \"@ prefix and any qualifier / @as@
-- suffix so equality is on the module identity, not on the full
-- import-decl source — the test should not be sensitive to
-- qualifier-shape drift.
duplicateNames :: Value -> [Text]
duplicateNames v =
  let arr = case lookupField "imports" v of
              Just (Array xs) -> xs
              _               -> V.empty
      names = [ stripImportPrefix s | String s <- V.toList arr ]
      groups = List.group (List.sort names)
  in [ head g | g <- groups, length g > 1 ]

-- | Reduce @"import qualified Data.List as L"@ → @"Data.List"@.
-- Tolerates @qualified@ and @as <alias>@; anything that doesn't
-- start with @\"import \"@ is returned unchanged so a malformed
-- entry surfaces in the dup check rather than getting silently
-- normalised away.
stripImportPrefix :: Text -> Text
stripImportPrefix t =
  case T.stripPrefix "import " (T.strip t) of
    Nothing   -> t
    Just rest ->
      let afterQual = fromMaybe rest (T.stripPrefix "qualified " rest)
          name      = T.takeWhile (\c -> c /= ' ' && c /= '(') afterQual
      in name

