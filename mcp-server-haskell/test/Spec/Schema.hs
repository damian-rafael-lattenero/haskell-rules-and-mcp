-- | Unit tests for the discriminated-schema builder helpers (#92 Phase A):
-- top-level shape, discriminant enum, required sets, additionalProperties,
-- flatObjectSchema, and field-type builders.
--
-- Extracted from the Spec.hs monolith (#271) via the function-export shape:
-- the driver keeps the registrations and imports these functions.
module Spec.Schema
  ( testSchemaTopLevelOneOf
  , testSchemaDiscriminantInEveryBranch
  , testSchemaDiscriminantConstMatchesValue
  , testSchemaRequiredSetsAreCorrect
  , testSchemaAdditionalPropertiesFalse
  , testSchemaFlatObject
  , testSchemaFieldBuilders
  ) where

import qualified Data.Aeson as A
import qualified Data.Aeson.KeyMap as AKM
import Data.Maybe (isNothing)
import qualified Data.Vector as Vector

import qualified HaskellFlows.Mcp.Schema as Schema

-- ---------------------------------------------------------------------------
-- Shared fixture: two-branch discriminated schema for ghc_refactor shape
-- ---------------------------------------------------------------------------

sampleRefactorSchema :: A.Value
sampleRefactorSchema = Schema.discriminatedSchema "action"
  [ Schema.SchemaBranch
      { Schema.sbDiscriminantValue = "rename_local"
      , Schema.sbDescription       = "Scoped identifier rename."
      , Schema.sbProperties        =
          [ ("module_path",      Schema.stringField  "Module path.")
          , ("new_name",         Schema.stringField  "New identifier.")
          , ("old_name",         Schema.stringField  "Existing identifier.")
          , ("scope_line_start", Schema.integerField "Inclusive start line.")
          , ("scope_line_end",   Schema.integerField "Inclusive end line.")
          , ("dry_run",          Schema.booleanField "Compute without writing.")
          ]
      , Schema.sbRequired
          = ["module_path", "new_name", "old_name"
            , "scope_line_start", "scope_line_end"]
      }
  , Schema.SchemaBranch
      { Schema.sbDiscriminantValue = "extract_binding"
      , Schema.sbDescription       = "Extract expression."
      , Schema.sbProperties        =
          [ ("module_path",      Schema.stringField  "Module path.")
          , ("new_name",         Schema.stringField  "New binding name.")
          , ("scope_line_start", Schema.integerField "Inclusive start line.")
          , ("scope_line_end",   Schema.integerField "Inclusive end line.")
          ]
      , Schema.sbRequired
          = ["module_path", "new_name"
            , "scope_line_start", "scope_line_end"]
      }
  ]

-- ---------------------------------------------------------------------------
-- Tests
-- ---------------------------------------------------------------------------

-- | The published top-level shape MUST be a flat object — no
-- 'oneOf' / 'allOf' / 'anyOf' at the root. The Claude API rejects
-- those keywords with HTTP 400.
testSchemaTopLevelOneOf :: IO Bool
testSchemaTopLevelOneOf = pure $ case sampleRefactorSchema of
  A.Object km ->
       AKM.lookup "type" km == Just (A.String "object")
    && isNothing (AKM.lookup "oneOf" km)
    && isNothing (AKM.lookup "allOf" km)
    && isNothing (AKM.lookup "anyOf" km)
    && case AKM.lookup "properties" km of
         Just (A.Object _) -> True
         _                 -> False
  _ -> False

-- | The discriminant must be present in the top-level 'properties'
-- as an 'enum' field listing every branch's value.
testSchemaDiscriminantInEveryBranch :: IO Bool
testSchemaDiscriminantInEveryBranch = pure $
  case sampleRefactorSchema of
    A.Object km -> case AKM.lookup "properties" km of
      Just (A.Object props) -> case AKM.lookup "action" props of
        Just (A.Object actObj) ->
             AKM.lookup "type" actObj == Just (A.String "string")
          && case AKM.lookup "enum" actObj of
               Just (A.Array _) -> True
               _                -> False
        _ -> False
      _ -> False
    _ -> False

-- | The discriminant's 'enum' must list every branch value in order.
testSchemaDiscriminantConstMatchesValue :: IO Bool
testSchemaDiscriminantConstMatchesValue = pure $
  case sampleRefactorSchema of
    A.Object km -> case AKM.lookup "properties" km of
      Just (A.Object props) -> case AKM.lookup "action" props of
        Just (A.Object actObj) -> case AKM.lookup "enum" actObj of
          Just (A.Array xs) ->
            [ s | A.String s <- Vector.toList xs ]
              == ["rename_local", "extract_binding"]
          _ -> False
        _ -> False
      _ -> False
    _ -> False

-- | At the top level the only required field is the discriminant.
testSchemaRequiredSetsAreCorrect :: IO Bool
testSchemaRequiredSetsAreCorrect = pure $
  case sampleRefactorSchema of
    A.Object km -> case AKM.lookup "required" km of
      Just (A.Array reqs) ->
        [ s | A.String s <- Vector.toList reqs ] == ["action"]
      _ -> False
    _ -> False

-- | additionalProperties: false at the top level keeps unknown fields out.
testSchemaAdditionalPropertiesFalse :: IO Bool
testSchemaAdditionalPropertiesFalse = pure $
  case sampleRefactorSchema of
    A.Object km ->
      AKM.lookup "additionalProperties" km == Just (A.Bool False)
    _ -> False

-- | 'flatObjectSchema' produces a valid flat schema with the right
-- property + required set (for non-discriminated tools).
testSchemaFlatObject :: IO Bool
testSchemaFlatObject =
  let s = Schema.flatObjectSchema
            [ ("expression", Schema.stringField "Haskell expression to eval.") ]
            [ "expression" ]
  in pure $ case s of
       A.Object km ->
            AKM.lookup "type" km == Just (A.String "object")
         && AKM.lookup "additionalProperties" km == Just (A.Bool False)
         && case AKM.lookup "required" km of
              Just (A.Array reqs) -> reqs == Vector.fromList [A.String "expression"]
              _                   -> False
       _ -> False

-- | Each field-builder helper emits the right 'type' + 'description'.
testSchemaFieldBuilders :: IO Bool
testSchemaFieldBuilders =
  let cases = [ ("string",  Schema.stringField  "x")
              , ("integer", Schema.integerField "x")
              , ("boolean", Schema.booleanField "x")
              , ("array",   Schema.arrayField   "x")
              ]
      ok (expected, v) = case v of
        A.Object km -> AKM.lookup "type" km == Just (A.String expected)
                    && AKM.lookup "description" km == Just (A.String "x")
        _           -> False
  in pure (all ok cases)
