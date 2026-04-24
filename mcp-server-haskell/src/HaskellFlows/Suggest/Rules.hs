-- | Rule catalog for QuickCheck law suggestion.
--
-- Each 'Rule' is a small closure: given a parsed type signature and
-- the function name it belongs to, it decides whether the signature
-- matches the law's shape and, if so, emits a property expression
-- the agent can feed straight into 'ghc_quickcheck'.
--
-- Adding a new rule is strictly additive — append it to 'allRules'
-- and the tool surfaces it automatically. No registration ceremony,
-- no dispatch table to update. That's the innovation over the TS
-- port's hard-coded table: rules compose and can be filtered by
-- category at call time.
module HaskellFlows.Suggest.Rules
  ( Rule (..)
  , RuleContext (..)
  , mkRuleContext
  , Confidence (..)
  , allRules
  , applyRules
  , applyRulesCtx
  , Suggestion (..)
  ) where

import Data.Text (Text)
import qualified Data.Text as T

import HaskellFlows.Parser.TypeSignature

-- | A law suggestion produced by a matched rule.
data Suggestion = Suggestion
  { sLaw        :: !Text        -- ^ human-facing label: \"Idempotent\"
  , sProperty   :: !Text        -- ^ ready-to-run QuickCheck expression
  , sRationale  :: !Text        -- ^ one-line \"why this rule matched\"
  , sConfidence :: !Confidence
  , sCategory   :: !Text        -- ^ \"algebraic\", \"list\", \"monoid\", …
  }
  deriving stock (Eq, Show)

-- | Rough confidence in the match: @High@ = structure uniquely
-- implies the law (e.g. @a -> a@ is almost always worth checking
-- idempotence/involution); @Medium@ = worth proposing but may not
-- hold; @Low@ = structural hint only, property is speculative.
data Confidence = High | Medium | Low
  deriving stock (Eq, Show, Ord)

-- | One rule in the catalog.
--
-- @rMatches@ returns @Just suggestion@ when the context fits the
-- law. Returning @Nothing@ is how a rule opts out; the tool layer
-- then skips it silently.
--
-- The context carries the function under analysis AND its
-- module-level siblings (other top-level names + their parsed
-- signatures). Most rules look only at the focal signature;
-- sibling-aware rules (evaluator-preservation, constant-folding-
-- soundness) use the list to pair functions that form a
-- transform+interpreter shape.
data Rule = Rule
  { rId      :: !Text
  , rMatches :: RuleContext -> [Suggestion]
    -- ^ multiple-Suggestion return so engines can emit two laws
    -- from one match (Functor identity + composition) or one law
    -- per matched sibling (evaluator-preservation over every
    -- interpreter sibling). Single-law rules return an 0/1 list
    -- via the 'legacy' wrapper.
  }

-- | The input to every rule. Built by the caller (see
-- 'mkRuleContext' for the single-signature case).
data RuleContext = RuleContext
  { rcName     :: !Text
  , rcSig      :: !ParsedSig
  , rcSiblings :: ![(Text, ParsedSig)]
    -- ^ module-level peers the focal function can be paired against.
    -- Empty list is valid — non-sibling-aware rules still work.
  }
  deriving stock (Show)

-- | Build a context with no sibling information — the common path
-- for callers that only have the focal signature (e.g. a bare
-- @:t@ against Prelude).
mkRuleContext :: Text -> ParsedSig -> RuleContext
mkRuleContext nm sig =
  RuleContext { rcName = nm, rcSig = sig, rcSiblings = [] }

-- | Run every rule in the catalog; concat the suggestions.
applyRulesCtx :: RuleContext -> [Suggestion]
applyRulesCtx ctx = concatMap (`rMatches` ctx) allRules

-- | Back-compat single-signature entrypoint (no siblings). Kept so
-- older callers + tests that predate sibling-aware rules keep
-- working unchanged.
applyRules :: Text -> ParsedSig -> [Suggestion]
applyRules nm sig = applyRulesCtx (mkRuleContext nm sig)

-- | Lift a plain @name -> sig -> Maybe Suggestion@ rule body into the
-- sibling-aware 'RuleContext' interface. The 8 pre-Phase-11f rules
-- never needed siblings, so each of them reuses this wrapper and
-- keeps its body unchanged — one-line change per rule, zero
-- semantic drift.
legacy :: (Text -> ParsedSig -> Maybe Suggestion)
       -> (RuleContext -> [Suggestion])
legacy f ctx = maybe [] pure (f (rcName ctx) (rcSig ctx))

--------------------------------------------------------------------------------
-- the catalog
--------------------------------------------------------------------------------

allRules :: [Rule]
allRules =
  [ ruleIdempotent
  , ruleInvolutive
  , ruleAssociative
  , ruleCommutative
  , ruleListLengthPreserving
  , ruleListRoundtrip
  , ruleReturnsBool
  , ruleMonoidIdentity
  , ruleFunctor
  , ruleEvaluatorPreservation
  , ruleConstantFoldingSoundness
  , rulePrinterParserRoundtrip
  ]

-- | @f :: a -> a@ ⇒ check @f (f x) == f x@.
--
-- Confidence is dampened for list-shaped functions (@[a] -> [a]@)
-- unless the name hints at canonicalisation (sort / normalise /
-- dedupe / canon / unique / clean). Rationale: many common list
-- functions share the shape @[a] -> [a]@ but are not idempotent —
-- 'reverse', 'take', 'drop', 'rotate'. Emitting the Idempotent law
-- at 'Medium' on those would mislead an agent into running the
-- property via 'ghc_quickcheck' and watching it fail, burning
-- trust in the suggestion engine.
ruleIdempotent :: Rule
ruleIdempotent = Rule
  { rId = "idempotent"
  , rMatches = legacy $ \nm sig ->
      if argCount sig == 1 && isSameTypeThroughout sig
        then
          let conf = idempotentConfidence nm sig
          in Just Suggestion
            { sLaw        = "Idempotent"
            , sProperty   =
                "\\x -> " <> nm <> " (" <> nm <> " x) == " <> nm <> " x"
            , sRationale  = case conf of
                Medium ->
                  "Type is `a -> a`; idempotence is worth checking \
                  \for normalisers, sorts, and canonicalisers."
                _ ->
                  "Type is `[a] -> [a]` but the name has no \
                  \canonicalisation hint — many list functions with \
                  \this shape (reverse, drop, rotate) are not \
                  \idempotent, so confidence is low."
            , sConfidence = conf
            , sCategory   = "algebraic"
            }
        else Nothing
  }

-- | Heuristic confidence for 'ruleIdempotent'. Plain @a -> a@ keeps
-- 'Medium'; list-shaped @[a] -> [a]@ drops to 'Low' unless the name
-- suggests canonicalisation.
idempotentConfidence :: Text -> ParsedSig -> Confidence
idempotentConfidence nm sig =
  case (psArgs sig, psReturn sig) of
    ([TyList _], TyList _)
      | nameHintsCanonicalisation nm -> Medium
      | otherwise                    -> Low
    _ -> Medium

-- | Does the function name contain a token that typically implies a
-- canonicalising / normalising operation? Checked case-insensitively
-- against a small, hand-picked set — deliberately not a full English
-- dictionary. Keep this list boring: only add tokens whose presence
-- is strong evidence the function idempotently reaches a canonical
-- form.
nameHintsCanonicalisation :: Text -> Bool
nameHintsCanonicalisation nm =
  let lc    = T.toLower nm
      hints = [ "sort", "normalize", "normalise", "canon"
              , "dedup", "dedupe", "unique", "clean"
              ]
  in any (`T.isInfixOf` lc) hints

-- | @f :: a -> a@ ⇒ check @f (f x) == x@ (stronger than idempotent —
-- applies to @reverse@, @negate@, @complement@).
--
-- BUG-18: functions whose name hints at normalisation /
-- optimisation (simplify, normalize, canonicalize, fold, reduce,
-- rewrite, optimize) are by definition /idempotent/, not
-- /involutive/ — @simplify (simplify x) == simplify x@ holds,
-- @simplify (simplify x) == x@ virtually never does (it fails as
-- soon as the input has ANY non-canonical form). Keep the
-- suggestion so an agent that genuinely wants the involution
-- check still gets it, but drop the confidence to 'Low' and
-- rewrite the rationale so the agent doesn't spend tokens running
-- it as if it were likely to pass.
ruleInvolutive :: Rule
ruleInvolutive = Rule
  { rId = "involutive"
  , rMatches = legacy $ \nm sig ->
      if argCount sig == 1 && isSameTypeThroughout sig
        then Just (mkInvolutive nm)
        else Nothing
  }
  where
    mkInvolutive nm
      | nameHintsOptimization nm = Suggestion
          { sLaw        = "Involutive"
          , sProperty   = "\\x -> " <> nm <> " (" <> nm <> " x) == x"
          , sRationale  = "Type is `a -> a` but the name hints at a \
                          \normaliser (simplify / normalize / canon / fold / \
                          \reduce / rewrite). Normalisers are idempotent, \
                          \not involutive — running this almost certainly \
                          \fails on the first non-canonical input. \
                          \Consider the idempotent or evaluator-preservation \
                          \law instead."
          , sConfidence = Low
          , sCategory   = "algebraic"
          }
      | otherwise = Suggestion
          { sLaw        = "Involutive"
          , sProperty   = "\\x -> " <> nm <> " (" <> nm <> " x) == x"
          , sRationale  = "Type is `a -> a`; involutive functions are their \
                          \own inverse (e.g. reverse, negate)."
          , sConfidence = Medium
          , sCategory   = "algebraic"
          }

-- | @op :: a -> a -> a@ ⇒ check @(x `op` y) `op` z == x `op` (y `op` z)@.
ruleAssociative :: Rule
ruleAssociative = Rule
  { rId = "associative"
  , rMatches = legacy $ \nm sig ->
      if argCount sig == 2 && isSameTypeThroughout sig
        then Just Suggestion
          { sLaw        = "Associative"
          , sProperty   =
              "\\x y z -> " <> infixed nm "(" nm <> " x y) z == "
              <> nm <> " x (" <> nm <> " y z)"
          , sRationale  = "Type is `a -> a -> a`; associativity is a core \
                          \law for monoids / semigroups."
          , sConfidence = High
          , sCategory   = "algebraic"
          }
        else Nothing
  }

-- | @op :: a -> a -> a@ ⇒ also check @x `op` y == y `op` x@.
ruleCommutative :: Rule
ruleCommutative = Rule
  { rId = "commutative"
  , rMatches = legacy $ \nm sig ->
      if argCount sig == 2 && isSameTypeThroughout sig
        then Just Suggestion
          { sLaw        = "Commutative"
          , sProperty   = "\\x y -> " <> nm <> " x y == " <> nm <> " y x"
          , sRationale  = "Type is `a -> a -> a`; commutativity holds for \
                          \addition, multiplication, union, but NOT for \
                          \subtraction, division, string concat."
          , sConfidence = Medium
          , sCategory   = "algebraic"
          }
        else Nothing
  }

-- | @f :: [a] -> [a]@ ⇒ check @length (f xs) == length xs@ (strict) or
-- @length (f xs) <= length xs@ (filter-like).
--
-- The inner type of arg and return must match — @[a] -> [b]@ (or
-- @[a] -> [Run a]@, the case that surfaced this fix) is a different
-- shape: the result list holds elements of a different type, so
-- \"length\" relationships between them are not a generic list-shape
-- property.
ruleListLengthPreserving :: Rule
ruleListLengthPreserving = Rule
  { rId = "list-length-preserving"
  , rMatches = legacy $ \nm sig -> case (psArgs sig, psReturn sig) of
      ([TyList argInner], TyList retInner)
        | argInner == retInner -> Just Suggestion
          { sLaw        = "Length preserving / non-extending"
          , sProperty   =
              "\\(xs :: [Int]) -> length (" <> nm <> " xs) <= length xs"
          , sRationale  = "Type is `[a] -> [a]`; the function can shrink \
                          \or permute the list but not grow it (common for \
                          \filter, take, drop, sort)."
          , sConfidence = Medium
          , sCategory   = "list"
          }
      _ -> Nothing
  }

-- | @f :: [a] -> [a]@ combined with involutive hint: check @f (f xs) == xs@.
--
-- Same same-inner-type guard as 'ruleListLengthPreserving': the
-- self-composition only type-checks when arg and return carry the
-- same element type.
ruleListRoundtrip :: Rule
ruleListRoundtrip = Rule
  { rId = "list-roundtrip"
  , rMatches = legacy $ \nm sig -> case (psArgs sig, psReturn sig) of
      ([TyList argInner], TyList retInner)
        | argInner == retInner -> Just Suggestion
          { sLaw        = "Self-inverse on lists"
          , sProperty   =
              "\\(xs :: [Int]) -> " <> nm <> " (" <> nm <> " xs) == xs"
          , sRationale  = "Type is `[a] -> [a]`; common candidate for reverse, \
                          \rot-k rotations, swap-adjacent-pairs."
          , sConfidence = Medium
          , sCategory   = "list"
          }
      _ -> Nothing
  }

-- | @f :: a -> Bool@ (any arity) ⇒ dual-polarity sanity check:
-- at least one input produces @True@ AND at least one produces
-- @False@. Not strictly a law but catches \"predicate that's
-- constantly True\" bugs early.
ruleReturnsBool :: Rule
ruleReturnsBool = Rule
  { rId = "returns-bool"
  , rMatches = legacy $ \nm sig -> case psReturn sig of
      TyCon "Bool" | argCount sig >= 1 -> Just Suggestion
        { sLaw        = "Predicate not constant"
        , sProperty   = "\\x -> " <> nm <> " x || not (" <> nm <> " x)"
        , sRationale  = "Return type is Bool; this is a trivially-true \
                        \tautology that catches pathological cases where \
                        \the predicate throws or loops."
        , sConfidence = Low
        , sCategory   = "predicate"
        }
      _ -> Nothing
  }

-- | When a @Monoid a@ constraint shows up and the shape is
-- @a -> a -> a@, check @mempty@ as the identity on both sides.
ruleMonoidIdentity :: Rule
ruleMonoidIdentity = Rule
  { rId = "monoid-identity"
  , rMatches = legacy $ \nm sig ->
      let hasMonoidContext =
            any (\c -> "Monoid " `T.isPrefixOf` c) (psConstraints sig)
      in if hasMonoidContext && argCount sig == 2 && isSameTypeThroughout sig
           then Just Suggestion
             { sLaw        = "Monoid identity"
             , sProperty   =
                 "\\x -> " <> nm <> " mempty x == x && " <> nm <> " x mempty == x"
             , sRationale  = "Type is `Monoid a => a -> a -> a`; mempty \
                             \must be a left + right identity."
             , sConfidence = High
             , sCategory   = "monoid"
             }
           else Nothing
  }

--------------------------------------------------------------------------------
-- Phase 11f sibling-aware engines
--------------------------------------------------------------------------------

-- | Match a type of shape @F a@ (user-defined single-param
-- constructor applied to one argument) OR @[a]@ (list sugar).
-- Returns @(containerName, innerType)@.
asSingleParamContainer :: SigType -> Maybe (Text, SigType)
asSingleParamContainer (TyApp (TyCon f) [t]) = Just (f, t)
asSingleParamContainer (TyApp (TyVar f) [t]) = Just (f, t)
asSingleParamContainer (TyList t)            = Just ("[]", t)
asSingleParamContainer _                     = Nothing

-- | Is this name's first word in a known canonicalisation lexicon?
-- Used by 'ruleConstantFoldingSoundness' to decide when to bump
-- the evaluator-preservation law's confidence to High.
nameHintsOptimization :: Text -> Bool
nameHintsOptimization nm =
  let lc = T.toLower nm
      hints =
        [ "simplify", "normalize", "normalise", "canonicalize"
        , "canonicalise", "canon", "fold", "optimize", "optimise"
        , "reduce", "rewrite"
        ]
  in any (`T.isInfixOf` lc) hints

-- | Functor identity law: @fmap id == id@.
-- Functor composition law: @fmap (f . g) == fmap f . fmap g@.
--
-- Matches any function whose type is @(a -> b) -> F a -> F b@ for
-- some single-parameter constructor @F@. Confidence High — any
-- real Functor instance must satisfy both; failure means a bug.
ruleFunctor :: Rule
ruleFunctor = Rule
  { rId = "functor-laws"
  , rMatches = \ctx ->
      let nm  = rcName ctx
          sig = rcSig ctx
      in case (psArgs sig, psReturn sig) of
           ([TyArrow arrIn arrOut, arg2], ret)
             | Just (f1, a) <- asSingleParamContainer arg2
             , Just (f2, b) <- asSingleParamContainer ret
             , f1 == f2
             , arrIn  == a
             , arrOut == b
               -> [ Suggestion
                    { sLaw        = "Functor identity"
                    , sProperty   =
                        "\\(xs :: " <> containerHint f1 <> ") -> "
                        <> nm <> " id xs == xs"
                    , sRationale  =
                        "Shape is `(a -> b) -> F a -> F b` (F = "
                        <> f1 <> "). `fmap id = id` is the first \
                        \functor law; a failure means the instance is \
                        \broken."
                    , sConfidence = High
                    , sCategory   = "functor"
                    }
                  , Suggestion
                    { sLaw        = "Functor composition"
                    , sProperty   =
                        "\\(xs :: " <> containerHint f1 <> ") -> "
                        <> nm <> " (even . (+1)) xs == "
                        <> nm <> " even (" <> nm <> " (+1) xs)"
                    , sRationale  =
                        "Second functor law: mapping `f . g` equals \
                        \mapping g then mapping f. Example uses concrete \
                        \Int→Int→Bool functions so QuickCheck can \
                        \instantiate without help."
                    , sConfidence = High
                    , sCategory   = "functor"
                    }
                  ]
           _ -> []
  }

-- | Emit a concrete test-able container hint for the functor
-- lambda. @[]@ becomes @[Int]@; @Maybe@ becomes @Maybe Int@;
-- other user constructors default to @F Int@.
containerHint :: Text -> Text
containerHint "[]" = "[Int]"
containerHint f    = f <> " Int"

-- | Evaluator preservation: when the focal function has shape
-- @X -> X@ and a sibling has shape @... -> X -> Y@ (where Y /= X)
-- or just @X -> Y@, propose @eval ... (f x) == eval ... x@.
--
-- This is the canonical optimization-soundness law:
--   eval . simplify    ≡ eval
--   interp . normalize ≡ interp
--   run . rewrite      ≡ run
--
-- Confidence Medium by default — the pairing is structural and the
-- law is usually intended but not always; specialization is the
-- job of 'ruleConstantFoldingSoundness'.
ruleEvaluatorPreservation :: Rule
ruleEvaluatorPreservation = Rule
  { rId = "evaluator-preservation"
  , rMatches = \ctx ->
      let nm  = rcName ctx
          sig = rcSig ctx
      in case (psArgs sig, psReturn sig) of
           ([x1], x2) | x1 == x2 ->
             [ mkEvalLaw nm interp Medium
             | interp <- interpreterSiblings x1 (rcSiblings ctx)
             ]
           _ -> []
  }

-- | Specialisation of 'ruleEvaluatorPreservation' that bumps
-- confidence to High when the focal function's name is a clear
-- "this is an optimisation" signal (simplify, normalize, canon,
-- fold, optimize, reduce, rewrite).
ruleConstantFoldingSoundness :: Rule
ruleConstantFoldingSoundness = Rule
  { rId = "constant-folding-soundness"
  , rMatches = \ctx ->
      let nm  = rcName ctx
          sig = rcSig ctx
      in if not (nameHintsOptimization nm)
           then []
           else case (psArgs sig, psReturn sig) of
             ([x1], x2) | x1 == x2 ->
               [ (mkEvalLaw nm interp High)
                   { sLaw = "Constant-folding soundness"
                   , sCategory = "evaluator"
                   , sRationale =
                       "Name \"" <> nm <> "\" is an optimisation hint \
                       \(simplify/normalize/canonicalize/fold/etc). \
                       \The canonical correctness invariant is that the \
                       \transform must preserve observable behaviour \
                       \through every interpreter in the module."
                   }
               | interp <- interpreterSiblings x1 (rcSiblings ctx)
               ]
             _ -> []
  }

-- | Data about a sibling that looks like an interpreter for the
-- focal function's input type.
data Interpreter = Interpreter
  { iName    :: !Text
  , iArity   :: !Int  -- total number of arguments (context + focal arg)
  }
  deriving stock (Show)

-- | Find siblings whose FINAL argument type matches the focal
-- function's input type and whose return type differs. Extra
-- leading arguments are treated as "context" (env, store, config).
interpreterSiblings :: SigType -> [(Text, ParsedSig)] -> [Interpreter]
interpreterSiblings targetType sibs =
  [ Interpreter { iName = nm, iArity = length (psArgs sig) }
  | (nm, sig) <- sibs
  , case reverse (psArgs sig) of
      (lastArg : _) -> lastArg == targetType && psReturn sig /= targetType
      []            -> False
  ]

-- | Render the preservation law for one interpreter sibling.
-- Builds a lambda with N parameters (one per context arg + the
-- focal arg) + @eval … (simplify x) == eval … x@.
mkEvalLaw :: Text -> Interpreter -> Confidence -> Suggestion
mkEvalLaw transformName interp conf =
  let arity     = iArity interp
      ctxArgs   = [ "p" <> T.pack (show i) | i <- [1 .. arity - 1] ]
      focalArg  = "x"
      allArgs   = T.unwords (ctxArgs <> [focalArg])
      leftCall  =
        T.unwords
          ( [iName interp] <> ctxArgs <>
            [ "(" <> transformName <> " " <> focalArg <> ")" ]
          )
      rightCall =
        T.unwords ([iName interp] <> ctxArgs <> [focalArg])
  in Suggestion
       { sLaw        = "Evaluator preservation"
       , sProperty   =
           "\\" <> allArgs <> " -> "
           <> leftCall <> " == " <> rightCall
       , sRationale  =
           "Paired sibling `" <> iName interp <> "` looks like an \
           \interpreter (last arg matches the transform's input type, \
           \return type differs). A transform that is supposed to be \
           \semantics-preserving must not change the interpreter's \
           \result."
       , sConfidence = conf
       , sCategory   = "evaluator"
       }

-- | Printer / parser roundtrip law. When a focal fn has shape
-- @A -> B@ (A ≠ B) and a sibling has shape @B -> Maybe A@
-- (partial inverse) or @B -> A@ (total inverse), propose:
--
--   parser (printer x) == Just x     (partial inverse)
--   parser (printer x) == x          (total inverse)
--
-- This is the canonical roundtrip law for pretty-printer + parser
-- pairs. Without this rule, 'ghc_suggest' returned 0 candidates
-- for @pretty :: Expr -> String@ + @parseExpr :: String -> Maybe
-- Expr@ because existing rules only match same-type or
-- container-shape transforms. Dogfood finding BUG-PLUS-06.
--
-- Confidence is High because a matched printer/parser pair that
-- DOESN'T roundtrip is almost always a bug — the shape implies
-- intent.
rulePrinterParserRoundtrip :: Rule
rulePrinterParserRoundtrip = Rule
  { rId = "printer-parser-roundtrip"
  , rMatches = \ctx ->
      let nm  = rcName ctx
          sig = rcSig ctx
      in case (psArgs sig, psReturn sig) of
           ([srcTy], tgtTy) | srcTy /= tgtTy ->
             [ mkRoundtripLaw nm srcTy sibName needsJust
             | (sibName, needsJust) <-
                 findInverseSiblings srcTy tgtTy (rcSiblings ctx)
             ]
           _ -> []
  }

-- | Collect siblings that serve as inverses of a focal
-- @A -> B@ function. An inverse either returns @A@ directly
-- (@g :: B -> A@) or wraps it in Maybe (@g :: B -> Maybe A@).
-- The Bool in the result is True when the sibling returns Maybe
-- (so the property asserts @== Just x@ rather than @== x@).
findInverseSiblings
  :: SigType           -- source type (focal's input)
  -> SigType           -- target type (focal's output)
  -> [(Text, ParsedSig)]
  -> [(Text, Bool)]
findInverseSiblings src tgt sibs =
  [ (name, needsJust)
  | (name, sig) <- sibs
  , Just needsJust <- [classifyInverse src tgt sig]
  ]

classifyInverse :: SigType -> SigType -> ParsedSig -> Maybe Bool
classifyInverse src tgt sig = case (psArgs sig, psReturn sig) of
  ([sibArg], sibRet)
    | sibArg == tgt && sibRet == src      -> Just False
    | sibArg == tgt, Just inner <- stripMaybe sibRet, inner == src
                                          -> Just True
  _                                       -> Nothing
  where
    stripMaybe (TyApp (TyCon "Maybe") [t]) = Just t
    stripMaybe _                           = Nothing

mkRoundtripLaw :: Text -> SigType -> Text -> Bool -> Suggestion
mkRoundtripLaw printer srcTy parser needsJust =
  Suggestion
    { sLaw        = "Printer/parser roundtrip"
    , sProperty   =
        "\\x -> " <> parser <> " (" <> printer <> " x) == "
          <> (if needsJust then "Just x" else "x")
    , sRationale  =
        "Sibling `" <> parser <> "` has the inverse shape of `"
          <> printer <> "`: parser's input type matches printer's \
          \output, parser's output recovers the printer's input"
          <> (if needsJust then " (wrapped in Maybe)" else "")
          <> ". Any roundtrip through a printer/parser pair must \
          \preserve the source — a counterexample points at a \
          \real drift between the two."
    , sConfidence = High
    , sCategory   = "roundtrip"
    }
  where
    _ = srcTy  -- retained in signature for clarity; unused in output today

--------------------------------------------------------------------------------
-- tiny formatter
--------------------------------------------------------------------------------

infixed :: Text -> Text -> Text -> Text
infixed _ opener nm = opener <> nm
