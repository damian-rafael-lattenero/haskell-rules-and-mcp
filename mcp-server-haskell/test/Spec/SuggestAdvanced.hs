-- | Unit tests for advanced Suggest rule roundtrips and Either-type
-- rules. All pure except testSuggestRoundtrip* and testSuggestEither*.
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
  ) where

import qualified Data.Text as T
import System.FilePath ((</>))

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

