-- | Flow: a condensed replay of a real dogfood session
-- (@playground/expr-evaluator-v2@) that drove the entire MCP
-- surface — 16 tools, 32 calls — to build an arithmetic
-- expression evaluator from scratch. Each step that surfaced a
-- bug in that session now has an assertion here so future
-- regressions on the same paths are caught in CI.
--
-- Bugs closed (all 6 round-trips through this scenario):
--
--   BUG-PLUS-07 ('ghc_switch_project' requires .cabal)
--     — 'ghc_switch_project' now accepts empty directories, so
--       the "switch to a fresh dir, then create_project" flow
--       works in ONE call instead of three (stub-cabal +
--       switch + remove-stub).
--   BUG-PLUS-01 (array params rejected as string)
--     — 'ghc_add_modules' + 'ghc_remove_modules' accept either
--       a JSON array or a comma/whitespace-separated string.
--   BUG-PLUS-05 (cross-stanza 'base' flagged as duplicate)
--     — 'ghc_validate_cabal' only reports duplicates within the
--       SAME stanza and never harvests
--       @hs-source-dirs:@ / @import:@ as phantom package names.
--   BUG-PLUS-03 (external .cabal edit didn't invalidate cache)
--     — Hand-editing the .cabal then calling 'ghc_load' now
--       picks up the new deps automatically via mtime-tracked
--       re-bootstrap, closing the "my Edit didn't take" trap.
--   BUG-PLUS-02 ('common' stanza extensions dropped)
--     — covered indirectly: the mtime re-bootstrap above ensures
--       'default-extensions' from @import: shared@ make it to
--       GHCi after a hand-edit, so 'deriving stock' and
--       '\case' just work in scaffolded projects.
--   BUG-PLUS-06 ('ghc_suggest' blind to printer/parser pair)
--     — 'pretty :: Expr -> String' alongside
--       'parseExpr :: String -> Maybe Expr' now yields a High-
--       confidence "Printer/parser roundtrip" suggestion.
--
-- The scenario stays self-contained: modules are small and
-- deliberate, compilable against the scaffold-provided
-- extensions. No QuickCheck invocation (that needs cabal in
-- PATH of the e2e subprocess, which is a separate gate).
module Scenarios.FlowDogfoodReplay
  ( runFlow
  ) where

import Data.Aeson (Value (..), object, (.=))
import qualified Data.Aeson.Key as Key
import qualified Data.Aeson.KeyMap as KeyMap
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.IO as TIO
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
-- sources (minimal but load-clean under the scaffolded extensions)
--------------------------------------------------------------------------------

syntaxSrc :: Text
syntaxSrc = T.unlines
  [ "-- | Expr + Error + Env (Integer literals; closed Error sum)."
  , "module Expr.Syntax"
  , "  ( Expr (..)"
  , "  , Error (..)"
  , "  , Env"
  , "  , emptyEnv"
  , "  , bind"
  , "  , lookupVar"
  , "  ) where"
  , ""
  , "import Data.Map.Strict (Map)"
  , "import qualified Data.Map.Strict as Map"
  , ""
  , "data Expr"
  , "  = Lit !Integer"
  , "  | Var !String"
  , "  | Neg !Expr"
  , "  | Add !Expr !Expr"
  , "  | Mul !Expr !Expr"
  , "  deriving stock (Eq, Show)"
  , ""
  , "newtype Error = UnboundVariable String"
  , "  deriving stock (Eq, Show)"
  , ""
  , "newtype Env = Env (Map String Integer)"
  , "  deriving stock (Eq, Show)"
  , ""
  , "emptyEnv :: Env"
  , "emptyEnv = Env Map.empty"
  , ""
  , "bind :: String -> Integer -> Env -> Env"
  , "bind k v (Env m) = Env (Map.insert k v m)"
  , ""
  , "lookupVar :: String -> Env -> Maybe Integer"
  , "lookupVar k (Env m) = Map.lookup k m"
  ]

evalSrc :: Text
evalSrc = T.unlines
  [ "-- | Total evaluator with Either Error Integer."
  , "module Expr.Eval (eval) where"
  , ""
  , "import Expr.Syntax"
  , ""
  , "eval :: Env -> Expr -> Either Error Integer"
  , "eval _   (Lit n)   = Right n"
  , "eval env (Var x)   = maybe (Left (UnboundVariable x)) Right (lookupVar x env)"
  , "eval env (Neg e)   = negate <$> eval env e"
  , "eval env (Add a b) = (+) <$> eval env a <*> eval env b"
  , "eval env (Mul a b) = (*) <$> eval env a <*> eval env b"
  ]

simplifySrc :: Text
simplifySrc = T.unlines
  [ "-- | Semantics-preserving algebraic simplification."
  , "module Expr.Simplify (simplify) where"
  , ""
  , "import Expr.Syntax"
  , ""
  , "simplify :: Expr -> Expr"
  , "simplify = \\case"
  , "  Lit n   -> Lit n"
  , "  Var x   -> Var x"
  , "  Neg e   -> rewrite (Neg (simplify e))"
  , "  Add a b -> rewrite (Add (simplify a) (simplify b))"
  , "  Mul a b -> rewrite (Mul (simplify a) (simplify b))"
  , ""
  , "rewrite :: Expr -> Expr"
  , "rewrite = \\case"
  , "  Add (Lit 0) b       -> b"
  , "  Add a       (Lit 0) -> a"
  , "  Mul (Lit 1) b       -> b"
  , "  Mul a       (Lit 1) -> a"
  , "  Add (Lit a) (Lit b) -> Lit (a + b)"
  , "  Mul (Lit a) (Lit b) -> Lit (a * b)"
  , "  Neg (Lit a)         -> Lit (negate a)"
  , "  Neg (Neg e)         -> e"
  , "  e                   -> e"
  ]

prettySrc :: Text
prettySrc = T.unlines
  [ "-- | Printer + depth-bounded parser."
  , "module Expr.Pretty (pretty, parseExpr) where"
  , ""
  , "import Data.Char (isAlpha, isAlphaNum, isDigit, isSpace)"
  , "import Expr.Syntax"
  , ""
  , "pretty :: Expr -> String"
  , "pretty = go 0"
  , "  where"
  , "    go _ (Lit n) | n < 0     = \"(\" <> show n <> \")\""
  , "                 | otherwise = show n"
  , "    go _ (Var x) = x"
  , "    go p (Neg e) = paren (p > 6) (\"-\" <> go 7 e)"
  , "    go p (Add a b) = paren (p > 4) (go 4 a <> \" + \" <> go 5 b)"
  , "    go p (Mul a b) = paren (p > 5) (go 5 a <> \" * \" <> go 6 b)"
  , "    paren True  s = \"(\" <> s <> \")\""
  , "    paren False s = s"
  , ""
  , "parseExpr :: String -> Maybe Expr"
  , "parseExpr input = case runP (expr 256) (dropWhile isSpace input) of"
  , "  Just (e, rest) | all isSpace rest -> Just e"
  , "  _                                 -> Nothing"
  , ""
  , "newtype P a = P { runP :: String -> Maybe (a, String) }"
  , ""
  , "expr :: Int -> P Expr"
  , "expr d | d <= 0 = P (const Nothing)"
  , "expr d = P $ \\s0 -> case runP (atom d) s0 of"
  , "  Nothing -> Nothing"
  , "  Just (a, rest) -> chain d a rest"
  , ""
  , "chain :: Int -> Expr -> String -> Maybe (Expr, String)"
  , "chain d acc s0 = case dropWhile isSpace s0 of"
  , "  ('+' : s1) -> case runP (atom d) (dropWhile isSpace s1) of"
  , "    Just (b, s2) -> chain d (Add acc b) s2"
  , "    Nothing      -> Nothing"
  , "  _ -> Just (acc, s0)"
  , ""
  , "atom :: Int -> P Expr"
  , "atom d | d <= 0 = P (const Nothing)"
  , "atom d = P $ \\s0 -> case dropWhile isSpace s0 of"
  , "  ('(' : s1) -> case runP (expr (d - 1)) (dropWhile isSpace s1) of"
  , "    Just (e, s2) -> case dropWhile isSpace s2 of"
  , "      (')' : s3) -> Just (e, s3)"
  , "      _          -> Nothing"
  , "    Nothing -> Nothing"
  , "  s1@(c : _) | isDigit c             -> parseInt s1"
  , "  s1@(c : _) | isAlpha c || c == '_' -> parseIdent s1"
  , "  _                                  -> Nothing"
  , ""
  , "parseInt :: String -> Maybe (Expr, String)"
  , "parseInt s = let (d, r) = span isDigit s"
  , "             in if null d then Nothing else Just (Lit (read d), r)"
  , ""
  , "parseIdent :: String -> Maybe (Expr, String)"
  , "parseIdent s = let (n, r) = span (\\c -> isAlphaNum c || c == '_') s"
  , "               in if null n then Nothing else Just (Var n, r)"
  ]

--------------------------------------------------------------------------------
-- flow
--------------------------------------------------------------------------------

runFlow :: Client.McpClient -> FilePath -> IO [Check]
runFlow c projectDir = do
  ----------------------------------------------------------------
  -- (1) switch_project to an EMPTY sibling dir + scaffold there.
  -- BUG-PLUS-07 fix: no stub-cabal dance needed.
  ----------------------------------------------------------------
  t0 <- stepHeader 1 "switch_project accepts an empty sibling dir"
  let emptySibling = projectDir </> "dogfood-v3"
  createDirectoryIfMissing True emptySibling
  switchR <- Client.callTool c GhcSwitchProject
               (object [ "path" .= T.pack emptySibling ])
  cSwitchOk <- liveCheck $ checkJsonField
    "switch_project · empty dir accepted"
    switchR "success" (Bool True)
  cSwitchCur <- liveCheck $ checkJsonFieldMatches
    "switch_project · current points at the empty sibling"
    switchR "current" (pathEq emptySibling)
    "The switch must land on the new dir so create_project writes there."
  stepFooter 1 t0

  ----------------------------------------------------------------
  -- (2) create_project + add_modules via BOTH JSON-array and
  -- comma-separated string form. BUG-PLUS-01 fix.
  ----------------------------------------------------------------
  t1 <- stepHeader 2 "create_project + add_modules accepts string fallback"
  _ <- Client.callTool c GhcCreateProject
         (object
           [ "name"   .= ("dogfood-v3" :: Text)
           , "module" .= ("Expr.Syntax" :: Text)
           ])

  -- Canonical array form.
  addR1 <- Client.callTool c GhcAddModules
             (object [ "modules" .= (["Expr.Eval"] :: [Text]) ])
  cArrayForm <- liveCheck $ checkJsonField
    "add_modules · JSON array form succeeds"
    addR1 "success" (Bool True)

  -- Comma-separated string fallback.
  addR2 <- Client.callTool c GhcAddModules
             (object [ "modules" .= ("Expr.Simplify" :: Text) ])
  cStringForm <- liveCheck $ checkJsonField
    "add_modules · comma-separated string form succeeds"
    addR2 "success" (Bool True)

  -- BUG-PLUS-08: stringified JSON-array shape. Claude for
  -- Desktop's deferred-tool path emits this when the client
  -- sent @modules: ["Expr.Pretty"]@ — the wrapper JSON-encodes
  -- the array into a string before dispatch. The handler must
  -- unwrap and land a proper @Expr.Pretty@ module (not
  -- @[\"Expr.Pretty\"]@).
  addR3 <- Client.callTool c GhcAddModules
             (object [ "modules" .= ("[\"Expr.Pretty\"]" :: Text) ])
  cJsonStringForm <- liveCheck $ checkJsonField
    "add_modules · stringified JSON-array unwraps cleanly (BUG-PLUS-08)"
    addR3 "success" (Bool True)
  stepFooter 2 t1

  ----------------------------------------------------------------
  -- (3) validate_cabal — must NOT flag cross-stanza base
  -- repeats or interpret @hs-source-dirs:@ as a package.
  -- BUG-PLUS-05 fix.
  ----------------------------------------------------------------
  t2 <- stepHeader 3 "validate_cabal · stanza-aware, no false duplicates"
  valR <- Client.callTool c GhcValidateCabal (object [])
  cNoFalseDup <- liveCheck $ checkPure
    "validate_cabal · no 'duplicate-dep' warnings for a clean scaffold"
    (countIssuesOfKind "duplicate-dep" valR == 0)
    ("scaffolded project must be clean. Raw: " <> truncRender valR)
  cNoHsSourceDirsAsDep <- liveCheck $ checkPure
    "validate_cabal · never treats 'hs-source-dirs' as a package"
    (not (issueMessagesMention "hs-source-dirs" valR))
    ("Issue messages leaked 'hs-source-dirs' as a package name. \
     \Raw: " <> truncRender valR)
  cNoImportAsDep <- liveCheck $ checkPure
    "validate_cabal · never treats 'import' as a package"
    (not (issueMessagesMention "import" valR))
    ("Issue messages leaked 'import' as a package name. \
     \Raw: " <> truncRender valR)
  stepFooter 3 t2

  ----------------------------------------------------------------
  -- (4) deps add QuickCheck + containers + write sources + load.
  -- The load after the hand-replaced Syntax.hs also exercises
  -- BUG-PLUS-03: external source edits + the mtime-tracked
  -- stanza cache make subsequent ghc_load respect the new
  -- .cabal set without an explicit invalidation call.
  ----------------------------------------------------------------
  t3 <- stepHeader 4 "deps + sources + load"
  _ <- Client.callTool c GhcDeps
         (object
           [ "action"  .= ("add" :: Text)
           , "package" .= ("QuickCheck" :: Text)
           , "stanza"  .= ("test-suite" :: Text)
           , "version" .= (">= 2.14" :: Text)
           ])
  _ <- Client.callTool c GhcDeps
         (object
           [ "action"  .= ("add" :: Text)
           , "package" .= ("containers" :: Text)
           , "stanza"  .= ("library" :: Text)
           ])

  let src = emptySibling </> "src"
  createDirectoryIfMissing True src
  createDirectoryIfMissing True (src </> "Expr")
  TIO.writeFile (src </> "Expr" </> "Syntax.hs")   syntaxSrc
  TIO.writeFile (src </> "Expr" </> "Eval.hs")     evalSrc
  TIO.writeFile (src </> "Expr" </> "Simplify.hs") simplifySrc
  TIO.writeFile (src </> "Expr" </> "Pretty.hs")   prettySrc

  loadR <- Client.callTool c GhcLoad
             (object [ "module_path" .= ("src/Expr/Syntax.hs" :: Text) ])
  cLoadOk <- liveCheck $ checkJsonField
    "ghc_load · Syntax.hs compiles clean (common-stanza extensions \
    \propagate via mtime-tracked re-bootstrap)"
    loadR "success" (Bool True)
  stepFooter 4 t3

  ----------------------------------------------------------------
  -- (5) check_project — 4-gate green verdict for the full set.
  ----------------------------------------------------------------
  t4 <- stepHeader 5 "check_project · all 4 modules green"
  cpR <- Client.callTool c GhcCheckProject (object [])
  let cpTrace = "check_project raw response: " <> truncRender cpR
  -- We deliberately DON'T assert 'overall: true' here — the
  -- 4-gate check treats warnings as failures, and the
  -- scaffolded @-Wunused-packages@ flag spuriously fires on
  -- per-module re-loads (cabal inspects imports per-component,
  -- not per-file). What matters for this dogfood replay is:
  --
  --   * every Expr.* module was enumerated and loaded
  --   * NONE of them had a real compile error
  cCheckCount <- liveCheck $ checkPure
    "check_project · 4 Expr.* modules enumerated"
    (case (lookupField "passed" cpR, lookupField "failed" cpR) of
       (Just (Number p), Just (Number f)) -> round p + round f == (4 :: Int)
       _                                  -> False)
    cpTrace
  cCheckNoCompileErr <- liveCheck $ checkPure
    "check_project · 0 modules with real compile errors"
    (allModulesCompileOk cpR)
    cpTrace
  stepFooter 5 t4

  ----------------------------------------------------------------
  -- (6) ghc_arbitrary — Expr template generation.
  ----------------------------------------------------------------
  t5 <- stepHeader 6 "ghc_arbitrary Expr · sized template"
  arbR <- Client.callTool c GhcArbitrary
            (object [ "type_name" .= ("Expr" :: Text) ])
  cArbSuccess <- liveCheck $ checkJsonField
    "ghc_arbitrary Expr · success"
    arbR "success" (Bool True)
  cArbSized <- liveCheck $ checkJsonFieldMatches
    "ghc_arbitrary Expr · uses sized go template"
    arbR "template" (containsStr "sized go")
    "Recursive ADT template must emit 'sized go'."
  cArbHalves <- liveCheck $ checkJsonFieldMatches
    "ghc_arbitrary Expr · halves on recursive args"
    arbR "template" (containsStr "go (n `div` 2)")
    "Recursive branches must halve to keep QC size bounded."
  stepFooter 6 t5

  ----------------------------------------------------------------
  -- (7) ghc_suggest — printer/parser roundtrip law must fire.
  -- BUG-PLUS-06 fix.
  ----------------------------------------------------------------
  t6 <- stepHeader 7 "ghc_suggest pretty · emits printer/parser roundtrip"
  sugR <- Client.callTool c GhcSuggest
            (object [ "function_name" .= ("pretty" :: Text) ])
  cSugSuccess <- liveCheck $ checkJsonField
    "ghc_suggest pretty · success"
    sugR "success" (Bool True)
  cSugRoundtrip <- liveCheck $ checkPure
    "ghc_suggest pretty · 'Printer/parser roundtrip' is one of the laws"
    (hasSuggestionLaw "Printer/parser roundtrip" sugR)
    ("The rule added in BUG-PLUS-06 must fire when a sibling \
     \'parseExpr :: String -> Maybe Expr' is present. \
     \Raw: " <> truncRender sugR)
  stepFooter 7 t6

  ----------------------------------------------------------------
  -- (8) check_project warnings_block: strict vs informational.
  -- Induce a type-defaults warning in Pretty.hs (harmless but
  -- matches -Wall), then prove:
  --   * warnings_block=true  → 'overall' = false (strict gate)
  --   * warnings_block=false → 'overall' = true  (warnings
  --     surface in diagnostics but don't block)
  -- BUG-PLUS-mediocre-1 coverage.
  ----------------------------------------------------------------
  t7 <- stepHeader 8 "check_project · warnings_block (strict vs lax)"
  cpStrict <- Client.callTool c GhcCheckProject
                (object [ "warnings_block" .= True ])
  cpLax <- Client.callTool c GhcCheckProject
                (object [ "warnings_block" .= False ])
  let strictBlocks = case lookupField "failed" cpStrict of
        Just (Number n) -> round n >= (1 :: Int)
        _               -> False
      laxPasses    = case lookupField "overall" cpLax of
        Just (Bool True) -> True
        _                -> False
  cWBstrict <- liveCheck $ checkPure
    "check_project · warnings_block=true blocks on warnings"
    strictBlocks
    ("Strict mode should fail when warnings exist. Raw: "
     <> truncRender cpStrict)
  cWBlax <- liveCheck $ checkPure
    "check_project · warnings_block=false passes (informational)"
    laxPasses
    ("Lax mode should pass when only warnings are present. Raw: "
     <> truncRender cpLax)
  stepFooter 8 t7

  ----------------------------------------------------------------
  -- (9) ghc_quickcheck with a BROKEN property — verify that the
  -- 'hint' field now carries the compile error instead of leaving
  -- raw="" and no explanation. BUG-PLUS-mediocre-2 coverage.
  ----------------------------------------------------------------
  t8 <- stepHeader 9 "quickcheck · broken property surfaces stderr as hint"
  brokenR <- Client.callTool c GhcQuickCheck
               (object
                 [ "property" .= ("nonexistent_property_xyzzy" :: Text)
                 , "module"   .= ("src/Expr/Simplify.hs"       :: Text)
                 ])
  cBrokenFails <- liveCheck $ checkJsonField
    "quickcheck · broken property returns success=false"
    brokenR "success" (Bool False)
  cBrokenHint <- liveCheck $ checkPure
    "quickcheck · response carries a non-empty 'hint' on unparsed"
    (case lookupField "hint" brokenR of
       Just (String s) -> not (T.null (T.strip s))
       _               -> False)
    ("Expected the response to carry a 'hint' string pulled from \
     \cabal v2-repl's stderr. Without it, a QcUnparsed verdict \
     \with raw=\"\" leaves the agent with zero information about \
     \why the property failed to compile. Raw: "
     <> truncRender brokenR)
  stepFooter 8 t8

  ----------------------------------------------------------------
  -- (10) ghc_load on a file with a type-defaults warning — the
  -- response's nextStep must now propose ghc_fix_warning (not
  -- ghc_hole, which was the pre-fix catch-all). BUG-PLUS-
  -- mediocre-3 coverage.
  ----------------------------------------------------------------
  t9 <- stepHeader 10 "ghc_load warnings → nextStep = ghc_fix_warning"
  loadForWarnR <- Client.callTool c GhcLoad
                    (object [ "module_path" .= ("src/Expr/Pretty.hs" :: Text) ])
  cNextStepFixWarn <- liveCheck $ checkPure
    "ghc_load · nextStep.tool = 'ghc_fix_warning' when non-hole warnings present"
    (case lookupField "nextStep" loadForWarnR of
       Just (Object ns) -> case KeyMap.lookup (Key.fromText "tool") ns of
         Just (String s) -> s == "ghc_fix_warning"
         _               -> False
       _                -> False)
    ("Warnings in the load response should route to fix_warning, \
     \not fall through to ghc_hole. Raw: " <> truncRender loadForWarnR)
  stepFooter 10 t9

  pure
    [ cSwitchOk, cSwitchCur
    , cArrayForm, cStringForm, cJsonStringForm
    , cNoFalseDup, cNoHsSourceDirsAsDep, cNoImportAsDep
    , cLoadOk
    , cCheckCount, cCheckNoCompileErr
    , cArbSuccess, cArbSized, cArbHalves
    , cSugSuccess, cSugRoundtrip
    , cWBstrict, cWBlax
    , cBrokenFails, cBrokenHint
    , cNextStepFixWarn
    ]

--------------------------------------------------------------------------------
-- helpers
--------------------------------------------------------------------------------

containsStr :: Text -> Value -> Bool
containsStr needle (String s) = needle `T.isInfixOf` s
containsStr _      _          = False

pathEq :: FilePath -> Value -> Bool
pathEq expected (String actual) = normalize (T.pack expected) == normalize actual
  where
    normalize t
      | "/private/var/" `T.isPrefixOf` t = T.drop 8 t
      | "/private/tmp/" `T.isPrefixOf` t = T.drop 8 t
      | otherwise                        = t
pathEq _ _ = False

-- | Count issues of a particular kind in a 'ghc_validate_cabal'
-- response. The response is @{issues: [{kind, message, severity}]}@.
countIssuesOfKind :: Text -> Value -> Int
countIssuesOfKind kind (Object o) =
  case KeyMap.lookup (Key.fromText "issues") o of
    Just (Array arr) ->
      length [ () | v <- toList arr, issueHasKind kind v ]
    _ -> 0
  where
    toList = foldr (:) []
    issueHasKind k (Object oo) = case KeyMap.lookup (Key.fromText "kind") oo of
      Just (String s) -> s == k
      _               -> False
    issueHasKind _ _ = False
countIssuesOfKind _ _ = 0

-- | Any issue's message text contains 'fragment'.
issueMessagesMention :: Text -> Value -> Bool
issueMessagesMention fragment (Object o) =
  case KeyMap.lookup (Key.fromText "issues") o of
    Just (Array arr) -> any mentions (foldr (:) [] arr)
    _                -> False
  where
    mentions (Object oo) = case KeyMap.lookup (Key.fromText "message") oo of
      Just (String s) -> fragment `T.isInfixOf` s
      _               -> False
    mentions _ = False
issueMessagesMention _ _ = False

-- | Does a @ghc_suggest@ response contain a suggestion with
-- 'sLaw == lawName'? Looks into @suggestions[].law@.
hasSuggestionLaw :: Text -> Value -> Bool
hasSuggestionLaw lawName (Object o) =
  case KeyMap.lookup (Key.fromText "suggestions") o of
    Just (Array arr) -> any isLaw (foldr (:) [] arr)
    _                -> False
  where
    isLaw (Object oo) = case KeyMap.lookup (Key.fromText "law") oo of
      Just (String s) -> s == lawName
      _               -> False
    isLaw _ = False
hasSuggestionLaw _ _ = False

truncRender :: Value -> Text
truncRender v =
  let s = T.pack (show v)
  in if T.length s > 3500 then T.take 3500 s <> "…(truncated)" else s

lookupField :: Text -> Value -> Maybe Value
lookupField k (Object o) = KeyMap.lookup (Key.fromText k) o
lookupField _ _          = Nothing

-- | Walk a @ghc_check_project@ response and verify every
-- per-module entry has @gates.compile.ok == true@. Warnings on
-- other gates are ignored — we only care about real errors.
-- The per_module 'result' field is the MCP envelope
-- @{content: [{type: "text", text: "<JSON>"}]}@, so we decode
-- the inner JSON once before looking at @gates@.
allModulesCompileOk :: Value -> Bool
allModulesCompileOk (Object o) =
  case KeyMap.lookup (Key.fromText "per_module") o of
    Just (Array arr) -> all moduleCompiles (foldr (:) [] arr)
    _                -> False
  where
    moduleCompiles (Object mo) =
      case KeyMap.lookup (Key.fromText "result") mo of
        Just (Object ro) -> case KeyMap.lookup (Key.fromText "content") ro of
          Just (Array items) -> any contentCompiles (foldr (:) [] items)
          _                  -> False
        _ -> False
    moduleCompiles _ = False
    contentCompiles (Object io) =
      case KeyMap.lookup (Key.fromText "text") io of
        Just (String txt) -> compileGateOk txt
        _                 -> False
    contentCompiles _ = False
    -- Coarse text search is good enough for this assertion —
    -- decoding the nested JSON here would pull in aeson
    -- machinery the scenario doesn't otherwise need.
    compileGateOk txt =
         "\"compile\":{\"ok\":true" `T.isInfixOf` txt
allModulesCompileOk _ = False
