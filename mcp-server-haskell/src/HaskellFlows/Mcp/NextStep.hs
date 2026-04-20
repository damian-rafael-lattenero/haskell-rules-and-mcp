-- | @nextStep@ — structured "what to do next" hint injected into
-- every successful tool response.
--
-- The MCP protocol already carries tool descriptors (static,
-- 'tools/list') and a session-level 'instructions' field (one-shot,
-- 'initialize'). What those do not tell the agent is which tool to
-- reach for *after* the current one succeeded. That decision was
-- implicit — the agent had to re-read the descriptors and infer a
-- chain. F-14 from the Phase 11d/e dogfood surfaced the gap: even
-- with F-13's richer 'instructions', a fresh agent burned several
-- turns on "ok, I created a project, now what?" questions that a
-- per-response hint would have closed in one round-trip.
--
-- This module provides a tiny decision table: given a tool name + a
-- success flag + the tool's JSON payload, it returns an optional
-- 'NextStep' that the server layer injects into the outgoing
-- payload. The agent sees a structured @nextStep@ alongside the
-- tool's data:
--
-- > {
-- >   "files_written": [ … ],
-- >   "success": true,
-- >   "nextStep": {
-- >     "tool": "ghci_deps",
-- >     "why":  "scaffold only has `base`; add the deps you need before wiring up modules.",
-- >     "example": { "action": "add", "package": "QuickCheck", "stanza": "test-suite" }
-- >   }
-- > }
--
-- The hint is informational — it never executes anything, never
-- leaks secrets (only tool names + canonical example args, all
-- internal). The agent is free to ignore it.
module HaskellFlows.Mcp.NextStep
  ( NextStep (..)
  , suggestNext
  , injectNextStep
  ) where

import Data.Aeson
import qualified Data.Aeson.KeyMap as KeyMap
import qualified Data.Aeson.Key as Key
import qualified Data.ByteString.Lazy as BL
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import qualified Data.Text.Lazy as TL
import qualified Data.Text.Lazy.Encoding as TLE

import HaskellFlows.Mcp.Protocol

-- | Structured next-step hint. 'nsExample' is an optional sample
-- arguments object the agent can use verbatim.
data NextStep = NextStep
  { nsTool    :: !Text
  , nsWhy     :: !Text
  , nsExample :: !(Maybe Value)
  }
  deriving stock (Eq, Show)

instance ToJSON NextStep where
  toJSON ns =
    object $
      [ "tool" .= nsTool ns
      , "why"  .= nsWhy ns
      ]
      <> maybe [] (\e -> ["example" .= e]) (nsExample ns)

--------------------------------------------------------------------------------
-- decision table
--------------------------------------------------------------------------------

-- | Map a (toolName, wasSuccessful, payload) triple to the next
-- recommended tool. 'Nothing' means no strong suggestion — the
-- agent should fall back to 'ghci_workflow(action="help")' if
-- genuinely unsure.
suggestNext :: Text -> Bool -> Value -> Maybe NextStep
suggestNext toolName ok payload
  | not ok    = Nothing   -- errors speak for themselves; let the agent parse them
  | otherwise = dispatch toolName payload

dispatch :: Text -> Value -> Maybe NextStep
dispatch name payload = case name of

  -- New scaffold → add deps next. QuickCheck in test-suite is the
  -- highest-hit case; agents doing non-test projects will override.
  "ghci_create_project" -> Just NextStep
    { nsTool = "ghci_deps"
    , nsWhy  = "Your scaffold has only `base`. Add the deps you need \
              \(QuickCheck for tests, plus any runtime libraries) before \
              \wiring up modules."
    , nsExample = Just (object
        [ "action"  .= ("add" :: Text)
        , "package" .= ("QuickCheck" :: Text)
        , "version" .= (">= 2.14" :: Text)
        , "stanza"  .= ("test-suite" :: Text)
        ])
    }

  -- After editing deps, reload to pick up the new package graph.
  "ghci_deps" -> case depsAction payload of
    Just "add"    -> Just loadAfterDepsEdit
    Just "remove" -> Just loadAfterDepsEdit
    _             -> Nothing
    where
      loadAfterDepsEdit = NextStep
        { nsTool = "ghci_load"
        , nsWhy  = "Dependency set changed. Reload your entry module so \
                  \the GHCi session sees the new package graph."
        , nsExample = Just (object
            [ "module_path" .= ("<your entry module>" :: Text) ])
        }

  -- Module loaded clean → propose laws next.
  "ghci_load" -> case (loadHasWarnings payload, loadHasErrors payload) of
    (_, True)     -> Nothing  -- errors speak for themselves
    (True, False) -> Just NextStep
      { nsTool = "ghci_hole"
      , nsWhy  = "The load reported warnings — if they are typed holes, \
                \'ghci_hole' gives you their expected types and in-scope \
                \fits in one call."
      , nsExample = Nothing
      }
    (False, False) -> Just NextStep
      { nsTool = "ghci_suggest"
      , nsWhy  = "Module compiles clean. Ask 'ghci_suggest' for QuickCheck \
                \laws its type signatures imply; feed the High-confidence \
                \ones into 'ghci_quickcheck'."
      , nsExample = Just (object
          [ "function_name" .= ("<pick one from the module>" :: Text) ])
      }

  -- Typed holes listed → implementation work, then reload.
  "ghci_hole" -> Just NextStep
    { nsTool = "ghci_load"
    , nsWhy  = "Implement the holes using the fits listed above, then \
              \reload with diagnostics=true to confirm the types now \
              \line up."
    , nsExample = Just (object
        [ "module_path" .= ("<same module you just inspected>" :: Text)
        , "diagnostics" .= True
        ])
    }

  -- Arbitrary template generated → paste + reload.
  "ghci_arbitrary" -> Just NextStep
    { nsTool = "ghci_load"
    , nsWhy  = "Paste the instance into the module that declares the \
              \type, then reload to confirm it compiles."
    , nsExample = Nothing
    }

  -- Suggestions → run them via quickcheck, pick highest confidence.
  "ghci_suggest" -> Just NextStep
    { nsTool = "ghci_quickcheck"
    , nsWhy  = "Feed the highest-confidence suggestion into quickcheck. \
              \Passing properties auto-persist to .haskell-flows/properties.json \
              \for the next regression run."
    , nsExample = Just (object
        [ "property"    .= ("<copy from suggestion.property>" :: Text)
        , "module_path" .= ("<module defining the function>" :: Text)
        ])
    }

  -- QuickCheck passed → keep chaining, or gate.
  "ghci_quickcheck" -> case qcState payload of
    Just "passed" -> Just NextStep
      { nsTool = "ghci_check_module"
      , nsWhy  = "Law holds. Either run 'ghci_suggest' for the next \
                \candidate or roll up into a per-module gate."
      , nsExample = Just (object
          [ "module_path" .= ("<same module>" :: Text) ])
      }
    Just "failed" -> Just NextStep
      { nsTool = "ghci_eval"
      , nsWhy  = "Property failed. Evaluate the reported counter-example \
                \with 'ghci_eval' to see intermediate values before editing."
      , nsExample = Nothing
      }
    _ -> Nothing

  -- Regression list → run the set.
  "ghci_regression" -> case regressionAction payload of
    Just "list" -> Just NextStep
      { nsTool = "ghci_regression"
      , nsWhy  = "You now know the persisted set. Run it to confirm every \
                \property still holds after recent edits."
      , nsExample = Just (object [ "action" .= ("run" :: Text) ])
      }
    Just "run"  -> Just NextStep
      { nsTool = "ghci_check_project"
      , nsWhy  = "All persisted properties re-played. Roll into the \
                \project-wide gate for pre-push readiness."
      , nsExample = Nothing
      }
    _ -> Nothing

  -- Refactor landed → verify compile + rerun regressions.
  "ghci_refactor" -> Just NextStep
    { nsTool = "ghci_load"
    , nsWhy  = "Refactor was snapshot-and-compile-verified, but a reload \
              \with diagnostics=true surfaces new holes or warnings in \
              \one shot."
    , nsExample = Just (object
        [ "module_path" .= ("<same module>" :: Text)
        , "diagnostics" .= True
        ])
    }

  -- Per-module gate passed → project-wide gate.
  "ghci_check_module" -> Just NextStep
    { nsTool = "ghci_check_project"
    , nsWhy  = "Module-complete. Run the project-wide gate to confirm \
              \every other module still compiles cleanly with your \
              \changes."
    , nsExample = Nothing
    }

  -- Project gate green → coverage + CI mirror.
  "ghci_check_project" -> Just NextStep
    { nsTool = "ghci_coverage"
    , nsWhy  = "Project-wide gate is green. Coverage gives you the HPC \
              \summary (8 metrics) to confirm your tests actually \
              \exercise the modules you changed. After that, run \
              \scripts/ci-local.sh --fast from the repo root before \
              \pushing."
    , nsExample = Nothing
    }

  -- Toolchain — if everything green, go build.
  "ghci_toolchain_status" -> Just NextStep
    { nsTool = "ghci_workflow"
    , nsWhy  = "With the toolchain confirmed, 'ghci_workflow(action=\"help\")' \
              \gives you the next action tailored to the session's \
              \current state (alive GHCi, loaded modules, etc)."
    , nsExample = Just (object [ "action" .= ("help" :: Text) ])
    }

  -- Cabal validated → if clean, proceed with deps / build.
  "ghci_validate_cabal" -> case cabalErrors payload of
    Just n | n > 0 -> Just NextStep
      { nsTool = "ghci_deps"
      , nsWhy  = "The .cabal file has errors. Fix them via 'ghci_deps' \
                \rather than editing by hand — the post-edit invariant \
                \check catches shape bugs before they land."
      , nsExample = Just (object [ "action" .= ("list" :: Text) ])
      }
    _ -> Nothing

  -- Lint surface → interpret yourself; no one-shot fix.
  "ghci_lint" -> Nothing

  -- Format → reload to confirm no behaviour change.
  "ghci_format" -> Just NextStep
    { nsTool = "ghci_load"
    , nsWhy  = "Formatter rewrote the module. Reload to confirm it still \
              \compiles and no whitespace-sensitive construct broke."
    , nsExample = Nothing
    }

  -- Batch → no single next step (depends on what the batch did); let
  -- the agent look at the individual results.
  "ghci_batch" -> Nothing

  -- Workflow meta — would loop if we suggested itself.
  "ghci_workflow" -> Nothing

  -- Exploratory tools — no strong suggestion.
  "ghci_type"    -> Nothing
  "ghci_info"    -> Nothing
  "ghci_eval"    -> Nothing
  "ghci_goto"    -> Nothing
  "ghci_doc"     -> Nothing
  "ghci_complete" -> Nothing
  "hoogle_search" -> Nothing

  -- Coverage is typically terminal in a session.
  "ghci_coverage" -> Nothing

  _ -> Nothing

--------------------------------------------------------------------------------
-- payload probes (small, hand-written, no lens-aeson dep)
--------------------------------------------------------------------------------

-- | Extract a string field from a JSON object payload. Returns
-- 'Nothing' if the payload is not an object, the field is missing,
-- or its value is not a string.
stringField :: Text -> Value -> Maybe Text
stringField k (Object o) = case KeyMap.lookup (Key.fromText k) o of
  Just (String s) -> Just s
  _               -> Nothing
stringField _ _ = Nothing

-- | Extract an integer field.
intField :: Text -> Value -> Maybe Int
intField k (Object o) = case KeyMap.lookup (Key.fromText k) o of
  Just (Number n) -> Just (round n)
  _               -> Nothing
intField _ _ = Nothing

-- | True if the payload's @warnings@ array is non-empty.
loadHasWarnings :: Value -> Bool
loadHasWarnings (Object o) = case KeyMap.lookup "warnings" o of
  Just (Array a) -> not (null a)
  _              -> False
loadHasWarnings _ = False

loadHasErrors :: Value -> Bool
loadHasErrors (Object o) = case KeyMap.lookup "errors" o of
  Just (Array a) -> not (null a)
  _              -> False
loadHasErrors _ = False

depsAction :: Value -> Maybe Text
depsAction = stringField "action"

regressionAction :: Value -> Maybe Text
regressionAction = stringField "action"

qcState :: Value -> Maybe Text
qcState = stringField "state"

cabalErrors :: Value -> Maybe Int
cabalErrors = intField "errors"

--------------------------------------------------------------------------------
-- injection
--------------------------------------------------------------------------------

-- | Splice a 'NextStep' into the first 'TextContent' block of a
-- 'ToolResult', assuming that block's text is JSON-encoded. If the
-- content is not JSON or not an object, the tool result is returned
-- unchanged — we prefer silently skipping injection over corrupting
-- a non-JSON payload.
injectNextStep :: NextStep -> ToolResult -> ToolResult
injectNextStep ns tr = tr { trContent = map splice (trContent tr) }
  where
    splice (TextContent t) = case decodeObject t of
      Nothing -> TextContent t
      Just o  ->
        let enriched = Object (KeyMap.insert "nextStep" (toJSON ns) o)
        in TextContent (encodeText enriched)

-- | Decode a Text into a JSON object. Returns 'Nothing' if the Text
-- is not valid JSON or not an object at the top level.
decodeObject :: Text -> Maybe (KeyMap.KeyMap Value)
decodeObject t =
  case decode (BL.fromStrict (TE.encodeUtf8 t)) of
    Just (Object o) -> Just o
    _               -> Nothing

encodeText :: Value -> Text
encodeText = TL.toStrict . TLE.decodeUtf8 . encode
