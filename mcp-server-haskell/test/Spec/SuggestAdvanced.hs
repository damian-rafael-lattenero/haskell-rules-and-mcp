-- | Unit tests for advanced Suggest rule roundtrips, Either-type rules,
-- withGhcSession stanza-flag invariant, and CabalBootstrap path-absolutize
-- helpers. All pure except testSuggestRoundtrip* and testSuggestEither*.
--
-- Extracted from the Spec.hs monolith (#271) via the function-export shape.
module Spec.SuggestAdvanced
  ( testSuggestRoundtripRule
  , testSuggestRoundtripNegative
  , testSuggestInterpreterNameGuard
  , testSuggestEvalPreservNoHash
  , testSuggestPrinterParserPairNames
  , testSuggestRoundtripUnrelatedFiltered
  , testSuggestEitherTotality
  , testSuggestEitherParserRoundtrip
  , testSuggestEitherRuleRegistered
  , testWithGhcSessionEnsuresStanza
  , testAbsolutizePathArgSingleToken
  , testAbsolutizePathArgEqForm
  , testAbsolutizeStanzaFlagsTwoToken
  , testAbsolutizeStanzaFlagsIdempotent
  , testAbsolutizeStanzaFlagsPreservesOrder
  ) where

import qualified Data.Text as T
import System.FilePath ((</>))

import HaskellFlows.Ghc.ApiSession (startGhcSession, killGhcSession, withGhcSession, absolutizePathArg, absolutizeStanzaFlags)
import HaskellFlows.Suggest.Rules
  ( Confidence (..)
  , RuleContext (..)
  , Suggestion (..)
  , applyRulesCtx
  , mkRuleContext
  , nameHintsInterpreter
  , nameHintsPrinter
  , namesFormPrinterParserPair
  )
import HaskellFlows.Types (mkProjectDir)

import Spec.Helpers (withTempProject)
import Control.Concurrent (threadDelay)
import Data.Maybe (isJust)
import qualified Data.Text.IO as TIO
import System.Directory (createDirectoryIfMissing, getTemporaryDirectory, removePathForcibly)
import Data.Time.Clock.POSIX (getPOSIXTime)
import qualified HaskellFlows.Ghc.ApiSession as ApiSession
import qualified HaskellFlows.Parser.TypeSignature

-- | A realistic printer/parser pair: focal is @pretty :: Expr ->
-- String@, sibling is @parseExpr :: String -> Maybe Expr@. The
-- rule must propose @parseExpr (pretty x) == Just x@.
testSuggestRoundtripRule :: IO Bool
testSuggestRoundtripRule = do
  -- parseSignature expects the RHS of '::' only. Passing the full
  -- 'name :: type' form in earlier iterations produced a garbled
  -- 'ParsedSig' whose psArgs was a TyApp of the function name —
  -- hence the rule never fired.
  let prettySig = HaskellFlows.Parser.TypeSignature.parseSignature
                    "Expr -> String"
      parserSig = HaskellFlows.Parser.TypeSignature.parseSignature
                    "String -> Maybe Expr"
  case (prettySig, parserSig) of
    (Just ps, Just qs) ->
      let ctx = RuleContext
            { rcName     = "pretty"
            , rcSig      = ps
            , rcSiblings = [("parseExpr", qs)]
            }
          suggestions = applyRulesCtx ctx
          hit = any (\s -> sLaw s == "Printer/parser roundtrip"
                          && "parseExpr" `T.isInfixOf` sProperty s
                          && "Just x"    `T.isInfixOf` sProperty s)
                   suggestions
      in pure hit
    _ -> pure False

-- | Negative: a same-type transform (@Expr -> Expr@) must NOT
-- trip the roundtrip rule even when a sibling returns Maybe Expr
-- — the rule is shape-keyed on A ≠ B.
testSuggestRoundtripNegative :: IO Bool
testSuggestRoundtripNegative = do
  let simpSig = HaskellFlows.Parser.TypeSignature.parseSignature
                  "Expr -> Expr"
      parserSig = HaskellFlows.Parser.TypeSignature.parseSignature
                  "String -> Maybe Expr"
  case (simpSig, parserSig) of
    (Just ps, Just qs) ->
      let ctx = RuleContext
            { rcName     = "simplify"
            , rcSig      = ps
            , rcSiblings = [("parseExpr", qs)]
            }
          roundtripSuggestions =
            filter (\s -> sLaw s == "Printer/parser roundtrip")
                   (applyRulesCtx ctx)
      in pure (null roundtripSuggestions)
    _ -> pure False

--------------------------------------------------------------------------------
-- #147: name-semantic guards prevent type-shape coincidence pairings
--------------------------------------------------------------------------------

-- | 'nameHintsInterpreter' must return True for evaluation-like
-- names and False for unrelated names that share the interpreter
-- type shape (e.g. @hash :: Expr -> Int@).
testSuggestInterpreterNameGuard :: IO Bool
testSuggestInterpreterNameGuard = pure $
     nameHintsInterpreter "eval"
  && nameHintsInterpreter "runExpr"
  && nameHintsInterpreter "interpret"
  && not (nameHintsInterpreter "hash")
  && not (nameHintsInterpreter "size")
  && not (nameHintsInterpreter "pretty")

-- | When a sibling's name does NOT hint at interpretation, the
-- evaluator-preservation rule must NOT pair it with the focal
-- transform. Pre-fix, @hash :: Expr -> Int@ was paired with
-- @simplify :: Expr -> Expr@ because it shared the interpreter
-- type shape.
testSuggestEvalPreservNoHash :: IO Bool
testSuggestEvalPreservNoHash = do
  let simpSig = HaskellFlows.Parser.TypeSignature.parseSignature "Expr -> Expr"
      evalSig = HaskellFlows.Parser.TypeSignature.parseSignature "Expr -> Int"
  case (simpSig, evalSig) of
    (Just ss, Just es) ->
      let ctx = RuleContext
            { rcName     = "simplify"
            , rcSig      = ss
            , rcSiblings = [("hash", es)]  -- same shape as eval but name doesn't hint
            }
          evalLaws = filter (\s -> sCategory s == "evaluator") (applyRulesCtx ctx)
      in pure (null evalLaws)
    _ -> pure False

-- | 'namesFormPrinterParserPair' must recognise valid pairs and
-- reject unrelated names that happen to coincide in type shape.
testSuggestPrinterParserPairNames :: IO Bool
testSuggestPrinterParserPairNames = pure $
     namesFormPrinterParserPair "pretty"      "parseExpr"
  && namesFormPrinterParserPair "encode"      "decode"
  && namesFormPrinterParserPair "serialize"   "deserialize"
  && namesFormPrinterParserPair "toJSON"      "fromJSON"
  && not (namesFormPrinterParserPair "size"        "length")
  && not (namesFormPrinterParserPair "hash"         "sort")
  && not (namesFormPrinterParserPair "pretty"       "hash")

-- | When a roundtrip-sibling's name does not correlate with the
-- focal's name (no printer/parser pair), no roundtrip law is emitted.
testSuggestRoundtripUnrelatedFiltered :: IO Bool
testSuggestRoundtripUnrelatedFiltered = do
  let focalSig  = HaskellFlows.Parser.TypeSignature.parseSignature "Expr -> Text"
      unrelSig  = HaskellFlows.Parser.TypeSignature.parseSignature "Text -> Expr"
  case (focalSig, unrelSig) of
    (Just fs, Just us) ->
      let ctx = RuleContext
            { rcName     = "hash"        -- not a printer name
            , rcSig      = fs
            , rcSiblings = [("lookup", us)]  -- not a parser name
            }
          roundtrips = filter (\s -> sLaw s == "Printer/parser roundtrip")
                               (applyRulesCtx ctx)
      in pure (null roundtrips)
    _ -> pure False

--------------------------------------------------------------------------------
-- #159: Either-return law templates
--------------------------------------------------------------------------------

-- | A function @parse :: Text -> Either String Expr@ must emit at
-- least the totality law even when no sibling is present. Before
-- the fix, @ghc_suggest@ returned 0 suggestions for this shape.
testSuggestEitherTotality :: IO Bool
testSuggestEitherTotality = do
  let sig = HaskellFlows.Parser.TypeSignature.parseSignature
              "Text -> Either String Expr"
  case sig of
    Just s ->
      let ctx = RuleContext { rcName = "parse", rcSig = s, rcSiblings = [] }
          sug = applyRulesCtx ctx
          hasTotality = any (\x -> sCategory x == "either") sug
      in pure hasTotality
    Nothing -> pure False

-- | When a printer sibling exists (@pretty :: Expr -> Text@), the
-- roundtrip property for an Either-returning parser must use
-- @Right x@, not @Just x@ or bare @x@.
testSuggestEitherParserRoundtrip :: IO Bool
testSuggestEitherParserRoundtrip = do
  let parseSig  = HaskellFlows.Parser.TypeSignature.parseSignature
                    "Text -> Either String Expr"
      prettySig = HaskellFlows.Parser.TypeSignature.parseSignature
                    "Expr -> Text"
  case (parseSig, prettySig) of
    (Just ps, Just pr) ->
      let ctx = RuleContext
            { rcName     = "parse"
            , rcSig      = ps
            , rcSiblings = [("pretty", pr)]
            }
          roundtrips = filter (\s -> sLaw s == "Printer/parser roundtrip")
                               (applyRulesCtx ctx)
          usesRight  = any (("Right x" `T.isInfixOf`) . sProperty) roundtrips
      in pure (not (null roundtrips) && usesRight)
    _ -> pure False

-- | The 'ruleEitherReturn' rule must be present in 'allRules'.
testSuggestEitherRuleRegistered :: IO Bool
testSuggestEitherRuleRegistered = do
  let sig = HaskellFlows.Parser.TypeSignature.parseSignature
              "Int -> Either String Bool"
  case sig of
    Just s ->
      let ctx = RuleContext { rcName = "validate", rcSig = s, rcSiblings = [] }
          sug = applyRulesCtx ctx
      in pure (any (\x -> sCategory x == "either") sug)
    Nothing -> pure False

--------------------------------------------------------------------------------
-- BUG-PLUS-03: external cabal edit invalidates stanza cache
--------------------------------------------------------------------------------

-- | Issue #49: every entry to 'withGhcSession' must re-run
-- 'ensureStanzaFlags' so external @.cabal@ edits picked up by
-- the next non-load tool ('ghc_type', 'ghc_eval', 'ghc_info', …)
-- without forcing the agent to issue an unrelated 'ghc_load'
-- first. Pre-fix the bootstrap was wired only to
-- 'loadForTarget'; tools that took the bare 'withGhcSession'
-- path served stale flags after a corruption-and-restore cycle.
--
-- Setup:
--   * Scaffold a real cabal project.
--   * One 'withGhcSession' call — populates mtime cache.
--   * Touch the @.cabal@ externally so mtime strictly advances.
--   * Another 'withGhcSession' call — must observe the bump.
testWithGhcSessionEnsuresStanza :: IO Bool
testWithGhcSessionEnsuresStanza = do
  base <- getTemporaryDirectory
  ts   <- getPOSIXTime
  let dir = base </> ("withghc-ensure-" <> show (floor (ts * 1000000) :: Int))
  createDirectoryIfMissing True dir
  TIO.writeFile (dir </> "demo.cabal") $ T.unlines
    [ "cabal-version: 2.4"
    , "name: demo"
    , "version: 0.1.0.0"
    , ""
    , "library"
    , "  build-depends: base"
    , "  default-language: Haskell2010"
    ]
  TIO.writeFile (dir </> "cabal.project") "packages: .\n"
  case mkProjectDir dir of
    Left _ -> do removePathForcibly dir; pure False
    Right pd -> do
      sess <- startGhcSession pd
      -- First withGhcSession — runs the auto-load + bootstrap.
      _ <- withGhcSession sess $ pure ()
      afterFirst <- ApiSession.readCabalMtimeForTest sess
      -- macOS fs mtime has 1-sec resolution; sleep past it.
      threadDelay 1_100_000
      TIO.writeFile (dir </> "demo.cabal") $ T.unlines
        [ "cabal-version: 2.4"
        , "name: demo"
        , "version: 0.2.0.0"
        , ""
        , "library"
        , "  build-depends: base, containers"
        , "  default-language: Haskell2010"
        ]
      -- Second withGhcSession with NO explicit ensureStanzaFlags
      -- must still bump the cached mtime. This is exactly the
      -- scenario non-load tools encounter.
      _ <- withGhcSession sess $ pure ()
      afterTouch <- ApiSession.readCabalMtimeForTest sess
      killGhcSession sess
      removePathForcibly dir
      pure
        ( isJust afterFirst
       && isJust afterTouch
       && afterFirst < afterTouch
        )

-- | Issue #43: 'absolutizePathArg' must absolutize the
-- single-token flag-embedded forms ('-isrc', '-IFoo', '-LDir')
-- and bare paths ('dist-newstyle/...') while leaving non-path
-- tokens, flag-only tokens, and already-absolute paths
-- untouched.
testAbsolutizePathArgSingleToken :: IO Bool
testAbsolutizePathArgSingleToken = pure $ and
  [ ApiSession.absolutizePathArg "/r" "-isrc"            == "-i/r/src"
  , ApiSession.absolutizePathArg "/r" "-IFoo"            == "-I/r/Foo"
  , ApiSession.absolutizePathArg "/r" "-LDir"            == "-L/r/Dir"
  , ApiSession.absolutizePathArg "/r" "dist-newstyle/x"  == "/r/dist-newstyle/x"
    -- already-absolute → untouched
  , ApiSession.absolutizePathArg "/r" "-i/abs/src"       == "-i/abs/src"
  , ApiSession.absolutizePathArg "/r" "/abs/path"        == "/abs/path"
    -- non-path tokens → untouched
  , ApiSession.absolutizePathArg "/r" "Shapes"           == "Shapes"
  , ApiSession.absolutizePathArg "/r" "-package-id=qux"  == "-package-id=qux"
    -- flag-only (no value glued) → untouched
  , ApiSession.absolutizePathArg "/r" "-package-db"      == "-package-db"
  ]

-- | Issue #43: '=' form ('-outputdir=DIR') is what GHC accepts
-- when paths come glued with '=' rather than space.
testAbsolutizePathArgEqForm :: IO Bool
testAbsolutizePathArgEqForm = pure $ and
  [ ApiSession.absolutizePathArg "/r" "-outputdir=dist-newstyle/build"
      == "-outputdir=/r/dist-newstyle/build"
  , ApiSession.absolutizePathArg "/r" "-hidir=hi"
      == "-hidir=/r/hi"
  , ApiSession.absolutizePathArg "/r" "-package-db=pkgs"
      == "-package-db=/r/pkgs"
  , ApiSession.absolutizePathArg "/r" "-odir=/already/abs"
      == "-odir=/already/abs"
    -- non-pathish long flags must NOT trigger the =-rewrite
  , ApiSession.absolutizePathArg "/r" "-funknown=value"
      == "-funknown=value"
  ]

-- | Issue #43: 'absolutizeStanzaFlags' walks the argv list and
-- pairs path-bearing flags with their next-token operand.
testAbsolutizeStanzaFlagsTwoToken :: IO Bool
testAbsolutizeStanzaFlagsTwoToken = pure $ and
  [ ApiSession.absolutizeStanzaFlags "/r"
      [ "-package-db", "dist-newstyle/store" ]
      == [ "-package-db", "/r/dist-newstyle/store" ]
  , ApiSession.absolutizeStanzaFlags "/r"
      [ "-hidir", "hi", "-odir", "obj" ]
      == [ "-hidir", "/r/hi", "-odir", "/r/obj" ]
  , ApiSession.absolutizeStanzaFlags "/r"
      [ "-package-env", ".ghc.environment.x" ]
      == [ "-package-env", "/r/.ghc.environment.x" ]
    -- absolute operand → leave alone
  , ApiSession.absolutizeStanzaFlags "/r"
      [ "-package-db", "/already/abs" ]
      == [ "-package-db", "/already/abs" ]
  ]

-- | Issue #43: applying 'absolutizeStanzaFlags' twice must be a
-- no-op. Idempotence keeps the function safe to call from
-- multiple code paths without double-rewrites.
testAbsolutizeStanzaFlagsIdempotent :: IO Bool
testAbsolutizeStanzaFlagsIdempotent =
  let raw =
        [ "-isrc"
        , "-package-db", "dist-newstyle/store"
        , "-outputdir=dist-newstyle/build"
        , "-this-unit-id", "demo-0.1.0.0-inplace"
        ]
      once  = ApiSession.absolutizeStanzaFlags "/r" raw
      twice = ApiSession.absolutizeStanzaFlags "/r" once
  in pure (once == twice)

-- | Issue #43: order of the input argv must be preserved
-- exactly (GHC's flag parser is order-sensitive — late
-- 'package-id' tokens depend on earlier 'package-db' tokens).
testAbsolutizeStanzaFlagsPreservesOrder :: IO Bool
testAbsolutizeStanzaFlagsPreservesOrder =
  let raw =
        [ "-package-db", "dist-newstyle/store"
        , "-hide-all-packages"
        , "-package-id", "QckChck-2.16-abc"
        , "-isrc"
        , "Shapes"
        ]
      out = ApiSession.absolutizeStanzaFlags "/r" raw
  in pure $ out ==
        [ "-package-db", "/r/dist-newstyle/store"
        , "-hide-all-packages"
        , "-package-id", "QckChck-2.16-abc"
        , "-i/r/src"
        , "Shapes"
        ]
