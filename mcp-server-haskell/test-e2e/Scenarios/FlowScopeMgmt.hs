-- | Flow: scope + export management.
--
-- Exercises the four tools that shape what is VISIBLE in a
-- module — either as exports (what the module offers) or as
-- imports (what the module consumes).
--
-- Tools exercised:
--
--   ghc_browse        (list a module's exported bindings)
--   ghc_imports       (list the GHCi session's live imports)
--   ghc_apply_exports (rewrite the module header's export list)
--   ghc_add_import    (suggest import lines for a bare name;
--                       Hoogle-backed — may be unavailable)
--
-- The add_import step gracefully accepts hoogle-unavailable
-- responses so the E2E stays green on machines without hoogle
-- installed (common on CI runners + fresh Nix shells).
module Scenarios.FlowScopeMgmt
  ( runFlow
  ) where

import Data.Aeson (Value (..), object, (.=))
import qualified Data.Aeson.Key as Key
import qualified Data.Aeson.KeyMap as KeyMap
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.IO as TIO
import qualified Data.Vector as V
import System.Directory (createDirectoryIfMissing)
import System.FilePath ((</>))

import E2E.Assert
  ( Check (..)
  , checkJsonField
  , checkJsonFieldMatches
  , checkPure
  , liveCheck
  , stepFooter
  , stepHeader
  )
import qualified E2E.Client as Client
import HaskellFlows.Mcp.ToolName (ToolName (..))

--------------------------------------------------------------------------------
-- source
--------------------------------------------------------------------------------

-- | Module with NO explicit export list — starts in the
-- "everything is exported" default. 'ghc_apply_exports' is
-- idempotent: if the module already has a list, it returns
-- 'no_change=true' and does nothing. We want to exercise the
-- rewrite branch, so we start without one and ask the tool to
-- ADD a narrow export list.
widgetSrc :: Text
widgetSrc =
  "module Widget where\n\
  \\n\
  \greet :: String -> String\n\
  \greet n = \"Hello, \" ++ n\n\
  \\n\
  \double :: Int -> Int\n\
  \double x = x * 2\n\
  \\n\
  \shout :: String -> String\n\
  \shout = map toUpperCheap\n\
  \  where\n\
  \    toUpperCheap c = if c >= 'a' && c <= 'z'\n\
  \                       then toEnum (fromEnum c - 32)\n\
  \                       else c\n"

--------------------------------------------------------------------------------
-- runFlow
--------------------------------------------------------------------------------

runFlow :: Client.McpClient -> FilePath -> IO [Check]
runFlow c projectDir = do
  ----------------------------------------------------------------
  -- setup
  ----------------------------------------------------------------
  t0 <- stepHeader 1 "scaffold + write Widget + load"
  _ <- Client.callTool c GhcCreateProject
         (object [ "name" .= ("scope-demo" :: Text) ])
  _ <- Client.callTool c GhcModules
         (object [ "action" .= ("add" :: Text), "modules" .= (["Widget"] :: [Text]) ])
  createDirectoryIfMissing True (projectDir </> "src")
  let srcPath = projectDir </> "src" </> "Widget.hs"
  TIO.writeFile srcPath widgetSrc
  _ <- Client.callTool c GhcLoad
         (object [ "module_path" .= ("src/Widget.hs" :: Text) ])
  stepFooter 1 t0

  ----------------------------------------------------------------
  -- ghc_browse — enumerate Widget's exports
  ----------------------------------------------------------------
  t1 <- stepHeader 2 "ghc_browse(Widget) — list exports"
  browseR <- Client.callTool c GhcBrowse
              (object [ "module" .= ("Widget" :: Text) ])
  c1 <- liveCheck $ checkJsonField
          "browse success" browseR "success" (Bool True)
  c2 <- liveCheck $ checkJsonFieldMatches
          "browse · count ≥ 3 entries"
          browseR "count" (numberAtLeast 3)
          "Widget exports greet / double / shout — at least 3"
  c3 <- liveCheck $ checkJsonFieldMatches
          "browse · entries array mentions 'greet'"
          browseR "entries" (anyEntryContains "greet")
          "expected at least one entry line containing 'greet'"
  stepFooter 2 t1

  ----------------------------------------------------------------
  -- ghc_imports — list the current in-scope imports
  ----------------------------------------------------------------
  t2 <- stepHeader 3 "ghc_imports — session imports"
  importsR <- Client.callTool c GhcImports (object [])
  c4 <- liveCheck $ checkJsonField
          "imports success" importsR "success" (Bool True)
  c5 <- liveCheck $ checkJsonFieldMatches
          "imports · 'imports' is an array"
          importsR "imports" isArray
          "the 'imports' field must be an array"
  stepFooter 3 t2

  ----------------------------------------------------------------
  -- ghc_apply_exports — trim Widget's header to just 'greet'
  ----------------------------------------------------------------
  t3 <- stepHeader 4 "ghc_apply_exports(Widget, [greet])"
  applyR <- Client.callTool c GhcApplyExports (object
    [ "module_path" .= ("src/Widget.hs" :: Text)
    , "exports"     .= (["greet"] :: [Text])
    ])
  c6 <- liveCheck $ checkJsonField
          "apply_exports success" applyR "success" (Bool True)
  bodyAfter <- TIO.readFile srcPath
  -- Invariants: (a) the module opening is still there,
  -- (b) an export list is now present with 'greet' inside,
  -- (c) the other defs ('double', 'shout') are still defined
  -- in the body but NOT in the export list.
  c7 <- liveCheck $ checkPure
    "apply_exports · header now contains an export list with 'greet'"
    (  "module Widget" `T.isInfixOf` bodyAfter
    && containsExportedName "greet" bodyAfter
    && not (containsExportedName "double" bodyAfter)
    && not (containsExportedName "shout"  bodyAfter)
    )
    "expected the module header rewritten so the export list \
    \carries 'greet' and excludes double/shout"
  stepFooter 4 t3

  ----------------------------------------------------------------
  -- ghc_add_import — best-effort, gracefully skip if hoogle
  -- is not installed on the machine running the E2E.
  ----------------------------------------------------------------
  t4 <- stepHeader 5 "ghc_add_import(fromMaybe) — hoogle-backed"
  addR <- Client.callTool c GhcAddImport
            (object [ "name" .= ("fromMaybe" :: Text) ])
  c8 <- liveCheck $ checkJsonFieldMatches
          "add_import returns a structured response"
          addR "success" (\case Bool _ -> True; _ -> False)
          "expected a boolean 'success' field (true OR false — \
          \false is fine if hoogle isn't installed)"
  stepFooter 5 t4

  pure [c1, c2, c3, c4, c5, c6, c7, c8]

--------------------------------------------------------------------------------
-- helpers
--------------------------------------------------------------------------------

numberAtLeast :: Int -> Value -> Bool
numberAtLeast n (Number x) = n <= (round x :: Int)
numberAtLeast _ _          = False

isArray :: Value -> Bool
isArray (Array _) = True
isArray _         = False

-- | Does any string entry in the given array contain the
-- supplied substring?
anyEntryContains :: Text -> Value -> Bool
anyEntryContains needle (Array a) =
  any matches (V.toList a)
  where
    matches (String s) = needle `T.isInfixOf` s
    matches _          = False
anyEntryContains _ _ = False

-- | Collapse runs of whitespace in the first 200 chars of the
-- file — good enough to detect "( greet )" vs "(greet)" in
-- rewritten headers regardless of formatting choice.
compactHeader :: Text -> Text
compactHeader = T.filter (/= ' ') . T.take 200

-- | True iff the given name appears inside the module's
-- EXPORT LIST only (between "(" after "module X" and the
-- closing ")" before "where"). We scan linearly — good enough
-- for the small scope of E2E fixture modules.
containsExportedName :: Text -> Text -> Bool
containsExportedName name body =
  case T.breakOn "(" body of
    (_, rest) | not (T.null rest) ->
      let inside = fst (T.breakOn ") where" (T.drop 1 rest))
      in name `T.isInfixOf` inside
    _ -> False

_unused :: KeyMap.KeyMap Value -> Key.Key
_unused _ = Key.fromText ""
