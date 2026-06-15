-- | Unit tests for Suggest.Rules law templates (involutive, self-inverse,
-- associativity confidence), browse-binding parser, and sibling-preservation
-- soundness. All pure.
--
-- Extracted from the Spec.hs monolith (#271) via the function-export shape.
module Spec.SuggestLaws
  ( testInvolutiveLowForNormalizer
  , testInvolutiveMediumForReverse
  , testSelfInverseLowForNormalizer
  , testSelfInverseMediumForReverse
  , testSuggestScopeStructuredHint
  , testParseShowModulesSimple
  , testParseShowModulesStar
  , testParseBrowseBindings
  , testParseBrowseContinuation
  , testSuggestSiblingsEnablePreservation
  , testSuggestSiblingsEnableSoundness
  ) where

import qualified Data.Aeson as A
import qualified Data.Text as T
import qualified Data.Text.Lazy as TL
import qualified Data.Text.Lazy.Encoding as TLE

import qualified HaskellFlows.Mcp.Envelope as Env
import HaskellFlows.Parser.TypeSignature (parseSignature)
import qualified HaskellFlows.Tool.Suggest as SuggestTool
import HaskellFlows.Suggest.Rules
  ( Confidence (..)
  , RuleContext (..)
  , Suggestion (..)
  , applyRules
  , applyRulesCtx
  , mkRuleContext
  , nameHintsInterpreter
  , nameHintsPrinter
  , namesFormPrinterParserPair
  )
import qualified HaskellFlows.Tool.Regression as RegTool
import qualified HaskellFlows.Tool.Browse as BrowseTool

testInvolutiveLowForNormalizer :: IO Bool
testInvolutiveLowForNormalizer =
  case parseSignature "Expr -> Expr" of
    Nothing  -> pure False
    Just sig -> pure $
      let names = ["simplify", "normalize", "canonicalize"
                  , "fold", "optimize", "reduce", "rewrite"]
          row nm =
            [ s | s <- applyRules nm sig, sLaw s == "Involutive" ]
          low  = Low
      in all (\nm ->
                case row nm of
                  [s] -> sConfidence s == low
                         && "normaliser" `T.isInfixOf` sRationale s
                  _   -> False)
              names

-- | Symmetric: 'reverse :: [a] -> [a]' is a genuine involution,
-- so the suggestion stays 'Medium' + the classical rationale.
testInvolutiveMediumForReverse :: IO Bool
testInvolutiveMediumForReverse =
  case parseSignature "[a] -> [a]" of
    Nothing  -> pure False
    Just sig -> pure $
      let row =
            [ s | s <- applyRules "reverse" sig, sLaw s == "Involutive" ]
      in case row of
           [s] -> sConfidence s == Medium
                  && "involutive" `T.isInfixOf` T.toLower (sRationale s)
                  && not ("normaliser" `T.isInfixOf` sRationale s)
           _   -> False

-- | Issue #73: 'Self-inverse on lists' is the structural twin
-- of 'Involutive' (same f.f==id law, list-shaped surface).
-- The pre-#73 rule ranked it Medium for ANY list signature,
-- including normalisers — making the agent burn a round-trip
-- on the losing law before reaching the (Low) Involutive
-- twin. Same dampening applies: when the function name hints
-- at canonicalisation, the rule must drop to Low with a
-- name-aware rationale.
testSelfInverseLowForNormalizer :: IO Bool
testSelfInverseLowForNormalizer =
  case parseSignature "[a] -> [a]" of
    Nothing  -> pure False
    Just sig -> pure $
      let names = ["simplify", "normalize", "canonicalize"
                  , "fold", "optimize", "reduce", "rewrite"]
          row nm =
            [ s | s <- applyRules nm sig, sLaw s == "Self-inverse on lists" ]
      in all (\nm ->
                case row nm of
                  [s] -> sConfidence s == Low
                         && "normaliser" `T.isInfixOf` sRationale s
                  _   -> False)
              names

-- | Issue #73 — symmetric: 'reverse :: [a] -> [a]' is a real
-- self-inverse, so the rule stays Medium with the original
-- "reverse, rot-k, swap-adjacent-pairs" rationale.
testSelfInverseMediumForReverse :: IO Bool
testSelfInverseMediumForReverse =
  case parseSignature "[a] -> [a]" of
    Nothing  -> pure False
    Just sig -> pure $
      let row =
            [ s | s <- applyRules "reverse" sig, sLaw s == "Self-inverse on lists" ]
      in case row of
           [s] -> sConfidence s == Medium
                  && "reverse" `T.isInfixOf` sRationale s
                  && not ("normaliser" `T.isInfixOf` sRationale s)
           _   -> False

--------------------------------------------------------------------------------
-- BUG-15 — ghc_suggest scope-error goes through a structured hint
--------------------------------------------------------------------------------

-- | BUG-15: the 'outOfScopeResult' helper returns a structured
-- payload with an actionable @hint@ instead of the raw GHC
-- "Variable not in scope" blob. Pin its shape.
testSuggestScopeStructuredHint :: IO Bool
testSuggestScopeStructuredHint =
  let ghcOut = "<interactive>:1:1: error: [GHC-88464] Variable not in scope: simplify"
      tr     = SuggestTool.outOfScopeResult "simplify" ghcOut
      body   = TL.toStrict (TLE.decodeUtf8 (A.encode tr))
  in pure $ Env.reStatus tr `elem` [Env.StatusFailed, Env.StatusRefused]
         && T.isInfixOf "\"reason\":\"function_not_in_scope\"" body
         && T.isInfixOf "\"function\":\"simplify\""            body
         && T.isInfixOf "ghc_load"                             body
         && T.isInfixOf "not in scope" body

--------------------------------------------------------------------------------
-- BUG-03 — sibling-aware suggest pipeline
--------------------------------------------------------------------------------

-- | Typical @:show modules@ output: one line per loaded module
-- with the file path in parens.
testParseShowModulesSimple :: IO Bool
testParseShowModulesSimple =
  let raw = T.unlines
        [ "Expr.Syntax    ( src/Expr/Syntax.hs, interpreted )"
        , "Expr.Simplify  ( src/Expr/Simplify.hs, interpreted )"
        , "Expr.Eval      ( src/Expr/Eval.hs, interpreted )"
        ]
      parsed = SuggestTool.parseShowModules raw
  in pure $ ("Expr.Syntax",   "src/Expr/Syntax.hs")   `elem` parsed
         && ("Expr.Simplify", "src/Expr/Simplify.hs") `elem` parsed
         && ("Expr.Eval",     "src/Expr/Eval.hs")     `elem` parsed

-- | @:show modules@ prefixes the currently-focused module with
-- @*@. The parser must strip it before picking up the name.
testParseShowModulesStar :: IO Bool
testParseShowModulesStar =
  let raw = T.unlines
        [ "* Expr.Simplify  ( src/Expr/Simplify.hs, interpreted )"
        , "  Expr.Syntax    ( src/Expr/Syntax.hs, interpreted )"
        ]
      parsed = SuggestTool.parseShowModules raw
  in pure $ ("Expr.Simplify", "src/Expr/Simplify.hs") `elem` parsed
         && ("Expr.Syntax",   "src/Expr/Syntax.hs")   `elem` parsed

-- | @:browse@ output mixes value bindings (lower-case head) with
-- type / class declarations (upper-case head). The parser keeps
-- only the value bindings with a top-level @::@.
testParseBrowseBindings :: IO Bool
testParseBrowseBindings =
  let raw = T.unlines
        [ "data Expr = Lit Int | Add Expr Expr"
        , "simplify :: Expr -> Expr"
        , "eval :: Env -> Expr -> Either Error Int"
        , "type Env = [(String, Int)]"
        , "class Monad m where"
        ]
      parsed = SuggestTool.parseBrowseBindings raw
  in pure $ ("simplify", "Expr -> Expr") `elem` parsed
         && ("eval",     "Env -> Expr -> Either Error Int") `elem` parsed
         && not (any (\(n, _) -> n `elem` ["Expr", "Env", "Monad"]) parsed)

-- | @:browse@ may break long types across lines with indentation.
-- The parser must skip indented continuation lines rather than
-- treat them as new bindings.
testParseBrowseContinuation :: IO Bool
testParseBrowseContinuation =
  let raw = T.unlines
        [ "prettyWithOptions :: Options"
        , "                  -> Expr"
        , "                  -> String"
        , "simplify :: Expr -> Expr"
        ]
      parsed = SuggestTool.parseBrowseBindings raw
  in pure $ any (\(n, _) -> n == "simplify") parsed

-- | BUG-03 core: when the focal function is @simplify :: Expr -> Expr@
-- and a sibling @eval :: Env -> Expr -> r@ is present (re-export
-- shape that the MCP will discover via @:browse@), the Evaluator
-- Preservation engine MUST fire. Pre-fix it never did because the
-- tool called 'applyRules' (no siblings) instead of 'applyRulesCtx'.
--
-- This test drives 'applyRulesCtx' directly with the sibling set
-- that 'gatherSiblings' would produce — the tool's end-to-end path
-- needs a live GHCi session that the unit test runner doesn't boot.
testSuggestSiblingsEnablePreservation :: IO Bool
testSuggestSiblingsEnablePreservation =
  case (parseSignature "Expr -> Expr", parseSignature "Env -> Expr -> Either Error Int") of
    (Just simpSig, Just evalSig) -> pure $
      let ctx = RuleContext
            { rcName     = "simplify"
            , rcSig      = simpSig
            , rcSiblings = [("eval", evalSig)]
            }
          laws = map sLaw (applyRulesCtx ctx)
      in "Constant-folding soundness" `elem` laws
         || "Evaluator preservation"   `elem` laws
    _ -> pure False

-- | Stricter version: the @simplify@ name hints at optimisation, so
-- 'ruleConstantFoldingSoundness' must fire at High confidence (that's
-- the whole point of the name-based bump). No sibling → no law.
testSuggestSiblingsEnableSoundness :: IO Bool
testSuggestSiblingsEnableSoundness =
  case (parseSignature "Expr -> Expr", parseSignature "Env -> Expr -> Either Error Int") of
    (Just simpSig, Just evalSig) -> pure $
      let withSib = RuleContext
            { rcName     = "simplify"
            , rcSig      = simpSig
            , rcSiblings = [("eval", evalSig)]
            }
          noSib   = withSib { rcSiblings = [] }
          hits s  = [ x | x <- applyRulesCtx s
                        , sLaw x == "Constant-folding soundness" ]
      in case hits withSib of
           (s:_) -> sConfidence s == High && null (hits noSib)
           []    -> False
    _ -> pure False
