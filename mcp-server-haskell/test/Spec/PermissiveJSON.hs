-- | Unit tests for 'Mcp.PermissiveJSON' IntField and BoolField coercions
-- (#88). All pure.
--
-- Extracted from the Spec.hs monolith (#271) via the function-export shape.
module Spec.PermissiveJSON
  ( testIntFieldNumber
  , testIntFieldNumericString
  , testIntFieldSignedString
  , testIntFieldStrippedString
  , testIntFieldRejectsNonNumeric
  , testIntFieldRejectsTrailingGarbage
  , testIntFieldRejectsFractional
  , testBoolFieldNative
  , testBoolFieldStringForms
  , testBoolFieldRejectsTruthy
  ) where

import qualified Data.Aeson as A

import HaskellFlows.Mcp.PermissiveJSON
  ( BoolField (..)
  , IntField (..)
  )

testIntFieldNumber :: IO Bool
testIntFieldNumber =
  pure (A.decode "42" == Just (IntField 42))

-- | Issue #88's bug shape: an MCP host wrapper stringifies the
-- number. The newtype must accept @\"42\"@ and produce IntField 42.
testIntFieldNumericString :: IO Bool
testIntFieldNumericString =
  pure (A.decode "\"42\"" == Just (IntField 42))

-- | Negative integers and a leading sign survive the string path.
testIntFieldSignedString :: IO Bool
testIntFieldSignedString =
  pure ( A.decode "\"-17\"" == Just (IntField (-17))
      && A.decode "\"+17\"" == Just (IntField 17) )

-- | Whitespace trim — common when shells stringify with padding.
testIntFieldStrippedString :: IO Bool
testIntFieldStrippedString =
  pure (A.decode "\"  42  \"" == Just (IntField 42))

-- | A clearly non-numeric string is rejected (not silently
-- coerced to 0). Loud failure beats a magic-default footgun.
testIntFieldRejectsNonNumeric :: IO Bool
testIntFieldRejectsNonNumeric =
  pure (A.decode "\"hello\"" == (Nothing :: Maybe IntField))

-- | A trailing non-digit after the integer is rejected: a typo
-- like @\"42x\"@ shouldn't silently parse as 42.
testIntFieldRejectsTrailingGarbage :: IO Bool
testIntFieldRejectsTrailingGarbage =
  pure (A.decode "\"42 lines\"" == (Nothing :: Maybe IntField))

-- | Float / fractional numbers are rejected — the field is
-- documented as @integer@ in the schema and the newtype must
-- enforce it.
testIntFieldRejectsFractional :: IO Bool
testIntFieldRejectsFractional =
  pure (A.decode "1.5" == (Nothing :: Maybe IntField))

-- | Canonical Bool wire — JSON boolean — keeps parsing.
testBoolFieldNative :: IO Bool
testBoolFieldNative =
  pure ( A.decode "true"  == Just (BoolField True)
      && A.decode "false" == Just (BoolField False) )

-- | The bug shape: stringified booleans, all four documented
-- forms (case-insensitive).
testBoolFieldStringForms :: IO Bool
testBoolFieldStringForms =
  pure ( A.decode "\"true\""  == Just (BoolField True)
      && A.decode "\"false\"" == Just (BoolField False)
      && A.decode "\"TRUE\""  == Just (BoolField True)
      && A.decode "\"False\"" == Just (BoolField False)
      && A.decode "\"1\""     == Just (BoolField True)
      && A.decode "\"0\""     == Just (BoolField False) )

-- | We do NOT do JavaScript truthiness — empty string and
-- arbitrary non-empty strings must be rejected. A non-truth
-- value should never land in a boolean field by accident.
testBoolFieldRejectsTruthy :: IO Bool
testBoolFieldRejectsTruthy =
  pure ( A.decode "\"\""    == (Nothing :: Maybe BoolField)
      && A.decode "\"yes\"" == (Nothing :: Maybe BoolField)
      && A.decode "1"       == (Nothing :: Maybe BoolField) )
