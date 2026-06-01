-- | Unit tests for 'Parser.TypeSignature' and 'Suggest.Rules': signature
-- parsing, rule matching, and confidence levels. All pure.
--
-- Extracted from the Spec.hs monolith (#271) via the function-export shape.
module Spec.SuggestSig
  ( testSigSimple
  , testSigConstraint
  , testSigList
  , testSuggestInvolutive
  , testSuggestAssoc
  , testSuggestAssocTemplate
  , testSuggestNoMatch
  , testSuggestReverseIdempotentLow
  , testSuggestNormalizeIdempotentMedium
  ) where

import qualified Data.Text as T

import HaskellFlows.Parser.TypeSignature
  ( ParsedSig (..)
  , SigType (..)
  , parseSignature
  )
import HaskellFlows.Suggest.Rules
  ( Confidence (..)
  , Suggestion (..)
  , applyRules
  )

testSigSimple :: IO Bool
testSigSimple = pure $ case parseSignature "a -> a" of
  Just sig -> argCountOf sig == 1
           && isSameTypeThroughout sig
           && null (psConstraints sig)
  _        -> False
  where argCountOf = length . psArgs

testSigConstraint :: IO Bool
testSigConstraint =
  pure $ case parseSignature "Eq a => a -> a -> Bool" of
    Just sig -> psConstraints sig == ["Eq a"]
             && length (psArgs sig) == 2
             && psReturn sig == TyCon "Bool"
    _ -> False

testSigList :: IO Bool
testSigList =
  pure $ case parseSignature "[a] -> [a]" of
    Just sig -> psArgs sig == [TyList (TyVar "a")]
             && psReturn sig == TyList (TyVar "a")
             && isSameTypeThroughout sig
    _ -> False

testSuggestInvolutive :: IO Bool
testSuggestInvolutive =
  case parseSignature "a -> a" of
    Nothing  -> pure False
    Just sig ->
      let suggestions = applyRules "foo" sig
      in pure (any ((== "Involutive") . sLaw) suggestions
             && any ((== "Idempotent") . sLaw) suggestions)

testSuggestAssoc :: IO Bool
testSuggestAssoc =
  case parseSignature "a -> a -> a" of
    Nothing  -> pure False
    Just sig ->
      let suggestions = applyRules "op" sig
      in pure (any ((== "Associative") . sLaw) suggestions
             && any ((== "Commutative") . sLaw) suggestions)

-- | Issue #52: the legacy Associative template emitted
-- @\\x y z -> (op x y) z == op x (op y z)@ — the LHS is
-- @(op x y) z@, which type-checks as \"apply the result of
-- @op x y :: a@ to @z@\" and is a type error whenever @a@ is
-- not a function. Pin that the corrected template applies the
-- outer @op@ on the LHS.
testSuggestAssocTemplate :: IO Bool
testSuggestAssocTemplate =
  case parseSignature "a -> a -> a" of
    Nothing  -> pure False
    Just sig ->
      let assoc = [ s | s <- applyRules "combineSorted" sig
                      , sLaw s == "Associative" ]
      in case assoc of
           [s] ->
             let prop = sProperty s
             in pure $
                  -- Outer call on the LHS must be present.
                  T.isInfixOf "combineSorted (combineSorted x y) z" prop
                  -- And the RHS shape stays the same.
               && T.isInfixOf "combineSorted x (combineSorted y z)" prop
                  -- The malformed bug shape was
                  -- "\\x y z -> (combineSorted x y) z ==" — the
                  -- LHS opening '(' immediately after '-> '. That
                  -- whole prefix must be absent now.
               && not (T.isInfixOf "-> (combineSorted x y) z" prop)
           _   -> pure False

testSuggestNoMatch :: IO Bool
testSuggestNoMatch =
  case parseSignature "Int -> String" of
    Nothing  -> pure False
    Just sig ->
      let suggestions = applyRules "foo" sig
      in pure (null suggestions)

--------------------------------------------------------------------------------
-- | Issue #23: @reverse :: [a] -> [a]@ fits the @a -> a@ shape that
-- 'ruleIdempotent' used to blindly promote to 'Medium'. Dampened
-- heuristic should either skip it or mark it 'Low' for a name with
-- no canonicalisation hint. Must never emit 'Medium' or 'High'.
testSuggestReverseIdempotentLow :: IO Bool
testSuggestReverseIdempotentLow =
  case parseSignature "[a] -> [a]" of
    Nothing  -> pure False
    Just sig ->
      let sugg = [ s | s <- applyRules "reverse" sig, sLaw s == "Idempotent" ]
      in pure $ case sugg of
           []  -> True
           [s] -> sConfidence s == Low
           _   -> False

-- | Issue #23: a name like @normalize@ — a strong canonicalisation
-- hint — should still surface Idempotent at 'Medium' even when
-- the shape is @[a] -> [a]@.
testSuggestNormalizeIdempotentMedium :: IO Bool
testSuggestNormalizeIdempotentMedium =
  case parseSignature "[a] -> [a]" of
    Nothing  -> pure False
    Just sig ->
      let sugg = [ s | s <- applyRules "normalize" sig, sLaw s == "Idempotent" ]
      in pure $ case sugg of
           [s] -> sConfidence s == Medium
           _   -> False
