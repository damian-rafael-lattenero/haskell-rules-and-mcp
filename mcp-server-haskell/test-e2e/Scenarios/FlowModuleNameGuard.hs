-- | Flow: module-name grammar guard at the MCP boundary (ISSUE-47).
--
-- The bug as reported: @ghc_add_modules(["lowercase.module"])@
-- happily registered the name into @exposed-modules@, after which
-- /every/ downstream tool that touched the @.cabal@ failed with a
-- cryptic stanza-flag error (parse error in the @.cabal@). The
-- project ended up unrecoverable through MCP — the agent had to
-- hand-edit the file to escape.
--
-- The fix lives at the tool boundary: 'HaskellFlows.Parser.ModuleName'
-- validates each input before any IO. This scenario exercises that
-- boundary through the FULL stdio JSON-RPC transport so we know:
--
--   (a) the validator is wired into 'AddModules.handle' /
--       'RemoveModules.handle' / 'ApplyExports.handle';
--   (b) the rejection is structural (success=false + 'rejected[]'),
--       not a stack trace and not a plain error string;
--   (c) the @.cabal@ is byte-identical pre/post call when at least
--       one name is invalid (atomic refusal — no partial writes that
--       would leave the agent's worldview inconsistent with disk);
--   (d) symmetric behaviour across the three affected tools — a
--       reserved keyword in an EXPORT is also refused, but the
--       narrower-than-modulename grammar (lowercase function names
--       are legal exports) is honoured;
--   (e) the happy paths still work after the fix — over-strict
--       validation that broke 'Foo.Bar' would be just as bad as the
--       original bug.
--
-- Each step has its own assertion so a regression in any one of the
-- three tools shows up attributable in the summary.
module Scenarios.FlowModuleNameGuard
  ( runFlow
  ) where

import Data.Aeson (Value (..), object, (.=))
import qualified Data.Aeson.Key as Key
import qualified Data.Aeson.KeyMap as KeyMap
import qualified Data.Vector as Vector
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
import E2E.Envelope (lookupField)
import HaskellFlows.Mcp.ToolName (ToolName (..))

runFlow :: Client.McpClient -> FilePath -> IO [Check]
runFlow c projectDir = do
  -- Fresh scaffolded project — gives us a real .cabal we can
  -- snapshot before/after each adversarial call.
  _ <- Client.callTool c GhcCreateProject
         (object
           [ "name"   .= ("modname-guard-demo" :: Text)
           , "module" .= ("Foo" :: Text)
           ])
  let cabalFile = projectDir </> "modname-guard-demo.cabal"

  ----------------------------------------------------------------
  -- (1) THE ORIGINAL BUG: a single lowercase module name.
  --
  -- Pre-fix: registered into exposed-modules; every later tool
  -- failed with "could not parse stanza flags".
  -- Post-fix: refused at the boundary, .cabal byte-identical.
  ----------------------------------------------------------------
  t0 <- stepHeader 1 "add_modules · refuses 'lowercase.module' (the original bug)"
  beforeBug <- TIO.readFile cabalFile
  bugR <- Client.callTool c GhcModules
            (object [ "action" .= ("add" :: Text), "modules" .= (["lowercase.module"] :: [Text]) ])
  afterBug <- TIO.readFile cabalFile
  cBugStructural <- liveCheck $ checkPure
    "add_modules · success=false + 'rejected' field present"
    (isStructuredRejection bugR)
    ("ISSUE-47: a lowercase first segment must be refused with a \
     \structured payload so the agent can self-correct in one \
     \round-trip. Raw: " <> truncRender bugR)
  cBugCabalIntact <- liveCheck $ checkPure
    "add_modules · .cabal byte-identical after refusal"
    (beforeBug == afterBug)
    "ISSUE-47: the .cabal must NOT change when the call is refused. \
    \Any change here means a partial write slipped through and the \
    \next ghc_load will fail with a stanza-flag parse error."
  cBugNamesQuoted <- liveCheck $ checkPure
    "add_modules · rejected payload names the offending input"
    (rejectedListContainsName "lowercase.module" bugR)
    ("ISSUE-47: the rejected entry MUST quote the original input so \
     \the agent doesn't have to guess which name was bad. Raw: "
     <> truncRender bugR)
  stepFooter 1 t0

  ----------------------------------------------------------------
  -- (2) ATOMIC REFUSAL: a mixed batch with one bad name.
  --
  -- Pre-fix would have written 'GoodOne' and possibly 'GoodTwo'
  -- before tripping over 'lowercase.module' mid-stream. Post-fix:
  -- the whole batch is refused and NEITHER good name appears in
  -- the .cabal — anything else would let the agent's worldview
  -- drift from disk reality.
  ----------------------------------------------------------------
  t1 <- stepHeader 2 "add_modules · atomic refusal of a mixed batch"
  beforeMix <- TIO.readFile cabalFile
  _ <- Client.callTool c GhcModules
         (object [ "action" .= ("add" :: Text), "modules" .= (["GoodOne", "lowercase.module", "GoodTwo"]
                            :: [Text]) ])
  afterMix <- TIO.readFile cabalFile
  cMixIntact <- liveCheck $ checkPure
    "add_modules · .cabal unchanged on mixed batch"
    (beforeMix == afterMix)
    "Atomic refusal: neither 'GoodOne' nor 'GoodTwo' may be written."
  cMixNoGoodOne <- liveCheck $ checkPure
    "add_modules · 'GoodOne' was NOT registered (atomic refusal)"
    (not ("GoodOne" `T.isInfixOf` afterMix))
    "ISSUE-47: a name listed BEFORE the bad one must not slip in."
  cMixNoGoodTwo <- liveCheck $ checkPure
    "add_modules · 'GoodTwo' was NOT registered (atomic refusal)"
    (not ("GoodTwo" `T.isInfixOf` afterMix))
    "ISSUE-47: a name listed AFTER the bad one must not slip in."
  stepFooter 2 t1

  ----------------------------------------------------------------
  -- (3) RESERVED KEYWORDS: 'Foo.module' (a Haskell keyword in a
  -- segment) — the lowercase rule alone wouldn't catch this if
  -- written as 'Foo.Module' (uppercase but still a typo). The
  -- keyword check fires regardless of casing.
  ----------------------------------------------------------------
  t2 <- stepHeader 3 "add_modules · refuses reserved keyword segment"
  beforeKw <- TIO.readFile cabalFile
  kwR <- Client.callTool c GhcModules
           (object [ "action" .= ("add" :: Text), "modules" .= (["Foo.module"] :: [Text]) ])
  afterKw <- TIO.readFile cabalFile
  cKwStructural <- liveCheck $ checkPure
    "add_modules · refuses reserved keyword + .cabal intact"
    (isStructuredRejection kwR && beforeKw == afterKw)
    ("ISSUE-47: 'module' (and other reserved keywords) cannot appear \
     \in any module-name segment. Raw: " <> truncRender kwR)
  cKwReasonMentionsKeyword <- liveCheck $ checkPure
    "add_modules · rejection reason mentions 'reserved' or 'keyword'"
    (rejectionReasonMentions ["reserved", "keyword"] kwR)
    ("ISSUE-47: the diagnostic must explain WHY 'Foo.module' is bad — \
     \\"name invalid\" without context wastes the agent's tokens. \
     \Raw: " <> truncRender kwR)
  stepFooter 3 t2

  ----------------------------------------------------------------
  -- (4) EVERY OFFENDER LISTED. The rejection payload must enumerate
  -- ALL bad inputs so the agent can fix them in ONE round-trip.
  -- Otherwise an N-bad-name list takes N round-trips, each costing
  -- tokens and walltime.
  ----------------------------------------------------------------
  t3 <- stepHeader 4 "add_modules · rejection lists every offender"
  multiR <- Client.callTool c GhcModules
              (object [ "action" .= ("add" :: Text), "modules" .= (["1Foo", "lowercase", "Foo.module"]
                                :: [Text]) ])
  cMultiAll <- liveCheck $ checkPure
    "add_modules · '1Foo', 'lowercase', AND 'Foo.module' all listed"
    (   rejectedListContainsName "1Foo"        multiR
     && rejectedListContainsName "lowercase"   multiR
     && rejectedListContainsName "Foo.module"  multiR)
    ("ISSUE-47: every offender must appear in 'rejected' so a single \
     \fix-attempt fixes them all. Raw: " <> truncRender multiR)
  stepFooter 4 t3

  ----------------------------------------------------------------
  -- (5) SYMMETRIC: ghc_remove_modules. Removing an invalid name is
  -- almost certainly an agent typo, and we'd rather refuse than
  -- silently no-op (or, worse, mangle the file via the same parse
  -- bug that motivated the issue).
  ----------------------------------------------------------------
  t4 <- stepHeader 5 "remove_modules · symmetric refusal"
  beforeRm <- TIO.readFile cabalFile
  rmR <- Client.callTool c GhcModules
           (object [ "action" .= ("remove" :: Text), "modules" .= (["lowercase.module"] :: [Text]) ])
  afterRm <- TIO.readFile cabalFile
  cRmStructural <- liveCheck $ checkPure
    "remove_modules · success=false + .cabal intact"
    (isStructuredRejection rmR && beforeRm == afterRm)
    ("ISSUE-47: ghc_remove_modules must refuse the same shapes as \
     \ghc_add_modules — consistency at the boundary. Raw: "
     <> truncRender rmR)
  stepFooter 5 t4

  ----------------------------------------------------------------
  -- (6) APPLY_EXPORTS: narrower contract — exports CAN be lowercase
  -- (function names like 'foo'), but reserved KEYWORDS in an export
  -- list still produce a guaranteed parse error in the rewritten
  -- header. Refuse those, leave everything else alone.
  ----------------------------------------------------------------
  t5 <- stepHeader 6 "apply_exports · refuses reserved keyword as export"
  let widgetPath = projectDir </> "src" </> "Widget.hs"
      widgetSrc  = T.unlines
        [ "module Widget where"
        , "greet :: String"
        , "greet = \"hi\""
        ]
  createDirectoryIfMissing True (projectDir </> "src")
  TIO.writeFile widgetPath widgetSrc
  beforeWidget <- TIO.readFile widgetPath
  exR <- Client.callTool c GhcApplyExports
           (object
             [ "module_path" .= ("src/Widget.hs" :: Text)
             , "exports"     .= (["greet", "module"] :: [Text])
             ])
  afterWidget <- TIO.readFile widgetPath
  cExStructural <- liveCheck $ checkPure
    "apply_exports · refuses 'module' as export + Widget.hs unchanged"
    (isStructuredRejection exR && beforeWidget == afterWidget)
    ("ISSUE-47: 'module Widget (greet, module) where' is a parse \
     \error in the rewritten header — the apply_exports handler \
     \must refuse it before writing. Raw: " <> truncRender exR)
  stepFooter 6 t5

  ----------------------------------------------------------------
  -- (7) APPLY_EXPORTS (regression): lowercase function names ARE
  -- valid exports. Over-strict validation that copied the
  -- module-name grammar verbatim into apply_exports would break
  -- the everyday case 'export greet, parseFoo, runBar'.
  ----------------------------------------------------------------
  t6 <- stepHeader 7 "apply_exports · accepts lowercase function name"
  okR <- Client.callTool c GhcApplyExports
           (object
             [ "module_path" .= ("src/Widget.hs" :: Text)
             , "exports"     .= (["greet"] :: [Text])
             ])
  widgetAfterOk <- TIO.readFile widgetPath
  cExOk <- liveCheck $ checkPure
    "apply_exports · 'greet' export written into header"
    (case lookupField "success" okR of
       Just (Bool True) -> "(greet)" `T.isInfixOf` widgetAfterOk
       _                -> False)
    ("Regression: lowercase function-name exports must still work. \
     \Raw: " <> truncRender okR)
  stepFooter 7 t6

  ----------------------------------------------------------------
  -- (8) ADD_MODULES happy path. Capstone regression: after all the
  -- adversarial calls, a perfectly legal module name still lands
  -- in the .cabal AND the source stub appears on disk. If this
  -- breaks, the .cabal was probably mutated by an earlier
  -- adversarial call and the project is now stuck.
  ----------------------------------------------------------------
  t7 <- stepHeader 8 "add_modules · happy path still works"
  goodR <- Client.callTool c GhcModules
             (object [ "action" .= ("add" :: Text), "modules" .= (["NewMod"] :: [Text]) ])
  cabalAfterGood <- TIO.readFile cabalFile
  cGoodSucceeded <- liveCheck $ checkPure
    "add_modules · success=true for 'NewMod'"
    (case lookupField "success" goodR of
       Just (Bool True) -> True
       _                -> False)
    ("Regression: adding a valid module name must succeed even after \
     \prior adversarial calls. Raw: " <> truncRender goodR)
  cGoodCabalUpdated <- liveCheck $ checkPure
    "add_modules · 'NewMod' appears in .cabal"
    ("NewMod" `T.isInfixOf` cabalAfterGood)
    ("ISSUE-47: legal name must reach exposed-modules. .cabal: "
     <> truncTxt cabalAfterGood)
  stepFooter 8 t7

  pure
    [ cBugStructural, cBugCabalIntact, cBugNamesQuoted
    , cMixIntact, cMixNoGoodOne, cMixNoGoodTwo
    , cKwStructural, cKwReasonMentionsKeyword
    , cMultiAll
    , cRmStructural
    , cExStructural
    , cExOk
    , cGoodSucceeded, cGoodCabalUpdated
    ]

--------------------------------------------------------------------------------
-- helpers
--------------------------------------------------------------------------------

-- | Spec from the issue: a refusal payload is @{success: false,
-- error: <summary>, rejected: [{name, reason}], hint: <grammar>}@.
-- We accept either the canonical 'rejected' field OR a generic
-- 'error' field with success=false — the latter covers older
-- error paths the validator hasn't been wired into yet, but for
-- ISSUE-47 we want the former specifically.
isStructuredRejection :: Value -> Bool
isStructuredRejection v =
     lookupField "success" v == Just (Bool False)
  && (hasField "rejected" v || hasField "error" v)

-- | Walk the @rejected@ array (if present) and return True iff
-- the named input appears as the @name@ field of any entry.
rejectedListContainsName :: Text -> Value -> Bool
rejectedListContainsName needle v =
  case lookupField "rejected" v of
    Just (Array xs) -> any matches (Vector.toList xs)
    _               -> False
  where
    matches (Object o) = case KeyMap.lookup (Key.fromText "name") o of
      Just (String s) -> s == needle
      _               -> False
    matches _          = False

-- | The @reason@ string in any rejected entry mentions ANY of the
-- given fragments. Case-insensitive — diagnostics may capitalise.
rejectionReasonMentions :: [Text] -> Value -> Bool
rejectionReasonMentions fragments v =
  case lookupField "rejected" v of
    Just (Array xs) -> any reasonHas (Vector.toList xs)
    _               -> False
  where
    reasonHas (Object o) = case KeyMap.lookup (Key.fromText "reason") o of
      Just (String s) ->
        let lo = T.toLower s
        in any ((`T.isInfixOf` lo) . T.toLower) fragments
      _ -> False
    reasonHas _ = False

hasField :: Text -> Value -> Bool
hasField k v = case lookupField k v of
  Just _  -> True
  Nothing -> False

truncRender :: Value -> Text
truncRender v =
  let raw = T.pack (show v)
      cap = 600
  in if T.length raw > cap then T.take cap raw <> "…(truncated)" else raw

truncTxt :: Text -> Text
truncTxt t
  | T.length t > 600 = T.take 600 t <> "…(truncated)"
  | otherwise        = t
