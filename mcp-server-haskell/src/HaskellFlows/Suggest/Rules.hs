-- | Rule catalog for QuickCheck law suggestion.
--
-- Each 'Rule' is a small closure: given a parsed type signature and
-- the function name it belongs to, it decides whether the signature
-- matches the law's shape and, if so, emits a property expression
-- the agent can feed straight into 'ghci_quickcheck'.
--
-- Adding a new rule is strictly additive — append it to 'allRules'
-- and the tool surfaces it automatically. No registration ceremony,
-- no dispatch table to update. That's the innovation over the TS
-- port's hard-coded table: rules compose and can be filtered by
-- category at call time.
module HaskellFlows.Suggest.Rules
  ( Rule (..)
  , Confidence (..)
  , allRules
  , applyRules
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
-- @rMatches@ returns @Just suggestion@ when the signature fits the
-- law. Returning @Nothing@ is how a rule opts out; the tool layer
-- then skips it silently.
data Rule = Rule
  { rId         :: !Text   -- ^ stable identifier for filtering
  , rMatches    :: Text -> ParsedSig -> Maybe Suggestion
                           --   ^ function name -> sig -> maybe a suggestion
  }

-- | Run every rule in the catalog; concat the suggestions.
applyRules :: Text -> ParsedSig -> [Suggestion]
applyRules fnName sig =
  [ s | r <- allRules, Just s <- [rMatches r fnName sig] ]

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
  ]

-- | @f :: a -> a@ ⇒ check @f (f x) == f x@.
--
-- Confidence is dampened for list-shaped functions (@[a] -> [a]@)
-- unless the name hints at canonicalisation (sort / normalise /
-- dedupe / canon / unique / clean). Rationale: many common list
-- functions share the shape @[a] -> [a]@ but are not idempotent —
-- 'reverse', 'take', 'drop', 'rotate'. Emitting the Idempotent law
-- at 'Medium' on those would mislead an agent into running the
-- property via 'ghci_quickcheck' and watching it fail, burning
-- trust in the suggestion engine.
ruleIdempotent :: Rule
ruleIdempotent = Rule
  { rId = "idempotent"
  , rMatches = \nm sig ->
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
ruleInvolutive :: Rule
ruleInvolutive = Rule
  { rId = "involutive"
  , rMatches = \nm sig ->
      if argCount sig == 1 && isSameTypeThroughout sig
        then Just Suggestion
          { sLaw        = "Involutive"
          , sProperty   = "\\x -> " <> nm <> " (" <> nm <> " x) == x"
          , sRationale  = "Type is `a -> a`; involutive functions are their \
                          \own inverse (e.g. reverse, negate)."
          , sConfidence = Medium
          , sCategory   = "algebraic"
          }
        else Nothing
  }

-- | @op :: a -> a -> a@ ⇒ check @(x `op` y) `op` z == x `op` (y `op` z)@.
ruleAssociative :: Rule
ruleAssociative = Rule
  { rId = "associative"
  , rMatches = \nm sig ->
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
  , rMatches = \nm sig ->
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
  , rMatches = \nm sig -> case (psArgs sig, psReturn sig) of
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
  , rMatches = \nm sig -> case (psArgs sig, psReturn sig) of
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
  , rMatches = \nm sig -> case psReturn sig of
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
  , rMatches = \nm sig ->
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
-- tiny formatter
--------------------------------------------------------------------------------

infixed :: Text -> Text -> Text -> Text
infixed _ opener nm = opener <> nm
