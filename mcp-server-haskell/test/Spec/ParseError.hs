-- | Unit tests for the parse-error rewriter (#85 / interpretParseError):
-- missing-key, type-mismatch (dotted / bracketed / no-field), unrecognised
-- fall-through, and the raw-preservation invariant.
--
-- Extracted from the Spec.hs monolith (#271) via the function-export shape:
-- the driver keeps the registrations and imports these functions.
module Spec.ParseError
  ( testParseErrorMissingKey
  , testParseErrorTypeMismatchDotted
  , testParseErrorTypeMismatchBracketed
  , testParseErrorTypeMismatchNoField
  , testParseErrorUnrecognisedFalls
  , testParseErrorRawAlwaysPreserved
  ) where

import Data.Maybe (isNothing)
import qualified Data.Text as T

import qualified HaskellFlows.Mcp.Envelope as Env
import HaskellFlows.Mcp.ParseError
  ( interpretParseError
  , ipField
  , ipKind
  , ipMessage
  , ipRaw
  )

-- | The canonical \"missing required key\" shape from Aeson.
testParseErrorMissingKey :: IO Bool
testParseErrorMissingKey =
  let raw = "Error in $: key \"expression\" not found"
      ip  = interpretParseError raw
  in pure ( ipKind    ip == Env.MissingArg
         && ipField   ip == Just "expression"
         && T.isInfixOf "expression" (ipMessage ip)
         && T.isInfixOf "missing"    (ipMessage ip)
         && ipRaw     ip == T.pack raw )

-- | The dotted-path shape: @Error in $.field: <reason>@.
testParseErrorTypeMismatchDotted :: IO Bool
testParseErrorTypeMismatchDotted =
  let raw = "Error in $.line: parsing Int failed, expected Number, but encountered String"
      ip  = interpretParseError raw
  in pure ( ipKind    ip == Env.TypeMismatch
         && ipField   ip == Just "line"
         && T.isInfixOf "line"  (ipMessage ip)
         && T.isInfixOf "wrong" (ipMessage ip) )

-- | Bracket-quoted field name: @Error in $['delete_files']: ...@.
testParseErrorTypeMismatchBracketed :: IO Bool
testParseErrorTypeMismatchBracketed =
  let raw = "Error in $['delete_files']: expected Bool, but encountered String"
      ip  = interpretParseError raw
  in pure ( ipKind  ip == Env.TypeMismatch
         && ipField ip == Just "delete_files" )

-- | Type-mismatch signal but no field path — falls through to Validation.
testParseErrorTypeMismatchNoField :: IO Bool
testParseErrorTypeMismatchNoField =
  let raw = "Error in something else: expected Bool, but encountered String"
      ip  = interpretParseError raw
  in pure ( ipKind  ip == Env.TypeMismatch
         && isNothing (ipField ip)
         && T.isInfixOf "wrong" (ipMessage ip) )

-- | An unrecognised parse-error string must NOT be silently mis-categorised.
testParseErrorUnrecognisedFalls :: IO Bool
testParseErrorUnrecognisedFalls =
  let raw = "some Aeson shape we never saw before"
      ip  = interpretParseError raw
  in pure ( ipKind  ip == Env.Validation
         && isNothing (ipField ip)
         && T.isInfixOf (T.pack raw) (ipMessage ip)
         && ipRaw   ip == T.pack raw )

-- | The raw text must always be preserved on 'ipRaw' regardless of branch.
testParseErrorRawAlwaysPreserved :: IO Bool
testParseErrorRawAlwaysPreserved =
  let cases = [ "Error in $: key \"foo\" not found"
              , "Error in $.bar: parsing Int failed"
              , "weird unrecognised shape"
              ]
      results = map interpretParseError cases
  in pure (and (zipWith (\raw r -> ipRaw r == T.pack raw) cases results))
