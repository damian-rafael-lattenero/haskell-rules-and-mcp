-- | Unit tests for 'Tool.Info' constructor/class-method block parsing,
-- instance-cap truncation, and 'Tool.CheckModule' gate logic.
--
-- Extracted from the Spec.hs monolith (#271) via the function-export shape.
module Spec.InfoParse
  ( testCodeToolsRegistered
  , testAddImportQualified
  , testInfoCtorBlockEmpty
  , testInfoCtorBlockMaybe
  , testInfoSuccessIncludesCtors
  , testInfoSuccessDropsCtorField
  , testInfoClassMethodsBlock
  , testInfoSuccessClassMethods
  , testInfoSuccessDropsClassMethods
  , testInfoInstanceCap
  , testInfoInstancesNotTruncated
  , testCheckGateEmpty
  , testCheckGatePass
  , testCheckGateRegressed
  , testCheckGateSkipped
  ) where

import qualified Data.Aeson as A
import qualified Data.Aeson.Key as AKey
import qualified Data.Aeson.KeyMap as AKM
import qualified Data.Text as T
import Data.Maybe (isNothing)

import qualified HaskellFlows.Mcp.Envelope as Env
import HaskellFlows.Mcp.Server (allToolNameTexts)
import qualified HaskellFlows.Tool.AddImport as AddImport
import qualified HaskellFlows.Tool.CheckModule as CheckModule
import qualified HaskellFlows.Tool.Info as InfoTool
import HaskellFlows.Parser.Type
  ( InfoKind (..)
  , ParsedInfo (..)
  )

-- ---------------------------------------------------------------------------
-- Local helpers
-- ---------------------------------------------------------------------------

gateField :: T.Text -> A.Value -> Maybe A.Value
gateField k (A.Object o) = AKM.lookup (AKey.fromText k) o
gateField _ _            = Nothing

-- ---------------------------------------------------------------------------
-- Tests
-- ---------------------------------------------------------------------------

testCodeToolsRegistered :: IO Bool
testCodeToolsRegistered = pure $
  all (`elem` allToolNameTexts)
    [ "ghc_add_import"
    , "ghc_modules"
    , "ghc_apply_exports"
    , "ghc_fix_warning"
    , "ghc_imports"
    ]

testAddImportQualified :: IO Bool
testAddImportQualified = pure $
     AddImport.renderImportLine False "Data.Map"
       == "import Data.Map"
  && AddImport.renderImportLine True  "Data.Map"
       == "import qualified Data.Map as M"

-- | Issue #54: empty constructor list → empty array, not a
-- one-element block of @null@s.
testInfoCtorBlockEmpty :: IO Bool
testInfoCtorBlockEmpty =
  pure (null (InfoTool.renderConstructorsBlock []))

-- | Issue #54: each constructor pair becomes one
-- @{name, args}@ object. Verify shape with the canonical
-- 'Maybe' example: @Nothing | Just a@.
testInfoCtorBlockMaybe :: IO Bool
testInfoCtorBlockMaybe =
  let block = InfoTool.renderConstructorsBlock
                [ ("Nothing", [])
                , ("Just",    ["a"])
                ]
  in pure $ length block == 2
        && hasName "Nothing" block
        && hasName "Just"    block
  where
    hasName n = any $ \case
      A.Object o -> AKM.lookup (AKey.fromText "name") o
                      == Just (A.String n)
      _          -> False

-- | Issue #54: 'successResult' must embed the @constructors@
-- field when the type is algebraic, so JSON consumers see the
-- structured constructor list alongside the legacy 'definition'.
testInfoSuccessIncludesCtors :: IO Bool
testInfoSuccessIncludesCtors =
  let parsed = ParsedInfo
        { piName       = "Maybe"
        , piKind       = IkData
        , piDefinition = "data Maybe a = Nothing | Just a"
        , piInstances  = []
        }
      ctors  = [("Nothing", []), ("Just", ["a"])]
      result = InfoTool.successResult parsed ctors []
  in pure $ case Env.reResult result of
       -- Issue #90 Phase B: 'constructors' moved under 'result'.
       Just (A.Object o) -> case AKM.lookup (AKey.fromText "constructors") o of
         Just (A.Array xs) -> length xs == 2
         _                 -> False
       _ -> False

-- | Issue #54: when no constructors apply (class / function /
-- type-synonym), the 'constructors' field must be absent — not
-- present-with-empty-array. Preserves wire-format compatibility
-- for consumers that didn't ask for the field.
testInfoSuccessDropsCtorField :: IO Bool
testInfoSuccessDropsCtorField =
  let parsed = ParsedInfo
        { piName       = "Functor"
        , piKind       = IkClass
        , piDefinition = "class Functor f"
        , piInstances  = []
        }
      result = InfoTool.successResult parsed [] []
  in pure $ case Env.reResult result of
       Just (A.Object o) ->
         isNothing (AKM.lookup (AKey.fromText "constructors") o)
       _ -> False

-- | Issue #70: 'renderClassMethodsBlock' produces one
-- @{name, type}@ object per method, in declaration order.
testInfoClassMethodsBlock :: IO Bool
testInfoClassMethodsBlock =
  let methods = [ ("fmap", "(a -> b) -> f a -> f b")
                , ("(<$)", "a -> f b -> f a")
                ]
      block = InfoTool.renderClassMethodsBlock methods
  in pure $ length block == 2
         && case block of
              [A.Object o1, A.Object o2] ->
                AKM.lookup (AKey.fromText "name") o1 == Just (A.String "fmap")
                && AKM.lookup (AKey.fromText "name") o2 == Just (A.String "(<$)")
              _ -> False

-- | Issue #70: when methods are present, the response carries
-- a top-level @class_methods@ array — the symmetric companion
-- to @constructors@ for data types.
testInfoSuccessClassMethods :: IO Bool
testInfoSuccessClassMethods =
  let parsed = ParsedInfo
        { piName       = "Functor"
        , piKind       = IkClass
        , piDefinition = "class Functor f where\n  fmap :: (a -> b) -> f a -> f b"
        , piInstances  = []
        }
      methods = [ ("fmap", "(a -> b) -> f a -> f b") ]
      result = InfoTool.successResult parsed [] methods
  in pure $ case Env.reResult result of
       Just (A.Object o) ->
         case AKM.lookup (AKey.fromText "class_methods") o of
           Just (A.Array xs) -> length xs == 1
           _                 -> False
       _ -> False

-- | Issue #70: a data type's response must NOT carry an empty
-- @class_methods@ array — the field should be absent. Wire-format
-- compatibility with consumers that didn't ask.
testInfoSuccessDropsClassMethods :: IO Bool
testInfoSuccessDropsClassMethods =
  let parsed = ParsedInfo
        { piName       = "Maybe"
        , piKind       = IkData
        , piDefinition = "data Maybe a = Nothing | Just a"
        , piInstances  = []
        }
      ctors = [("Nothing", []), ("Just", ["a"])]
      result = InfoTool.successResult parsed ctors []
  in pure $ case Env.reResult result of
       Just (A.Object o) ->
         isNothing (AKM.lookup (AKey.fromText "class_methods") o)
       _ -> False

-- | #142: when piInstances has more than 30 entries, successResult
-- caps the 'instances' list and emits instance_count with the total.
testInfoInstanceCap :: IO Bool
testInfoInstanceCap =
  let insts  = map (\i -> "instance Foo Type" <> T.pack (show (i :: Int)))
                   [1 .. 50]
      parsed = ParsedInfo
        { piName       = "Foo"
        , piKind       = IkClass
        , piDefinition = "class Foo a"
        , piInstances  = insts
        }
      result = InfoTool.successResult parsed [] []
  in pure $ case Env.reResult result of
       Just (A.Object o) ->
         let instList      = AKM.lookup (AKey.fromText "instances") o
             instCount     = AKM.lookup (AKey.fromText "instance_count") o
             instTruncated = AKM.lookup (AKey.fromText "instances_truncated") o
         in case (instList, instCount, instTruncated) of
              (Just (A.Array arr), Just (A.Number n), Just (A.Bool b)) ->
                   length arr == 30
                && floor n == (50 :: Int)
                && b
              _ -> False
       _ -> False

-- | #142: when piInstances is under the cap, instances_truncated is
-- false and instance_count equals the list length.
testInfoInstancesNotTruncated :: IO Bool
testInfoInstancesNotTruncated =
  let insts  = ["instance Foo A", "instance Foo B"]
      parsed = ParsedInfo
        { piName       = "Foo"
        , piKind       = IkClass
        , piDefinition = "class Foo a"
        , piInstances  = insts
        }
      result = InfoTool.successResult parsed [] []
  in pure $ case Env.reResult result of
       Just (A.Object o) ->
         let instCount     = AKM.lookup (AKey.fromText "instance_count") o
             instTruncated = AKM.lookup (AKey.fromText "instances_truncated") o
         in case (instCount, instTruncated) of
              (Just (A.Number n), Just (A.Bool b)) ->
                   floor n == (2 :: Int)
                && not b
              _ -> False
       _ -> False

-- | Issue #42: empty store → status="empty", ok=true.
testCheckGateEmpty :: IO Bool
testCheckGateEmpty =
  let g = CheckModule.propertiesGate 0 0 0 0
  in pure $ gateField "ok" g == Just (A.Bool True)
        && gateField "status" g == Just (A.String "empty")

-- | Issue #42: every stored prop passed → status="pass", ok=true,
-- reason matches.
testCheckGatePass :: IO Bool
testCheckGatePass =
  let g = CheckModule.propertiesGate 3 3 0 0
  in pure $ gateField "ok" g == Just (A.Bool True)
        && gateField "status" g == Just (A.String "pass")
        && case gateField "reason" g of
             Just (A.String r) -> "pass" `T.isInfixOf` r
             _                 -> False

-- | Issue #42: at least one regressed → status="regressed", ok=false,
-- reason contains "regressed". The bug shape was reason="N pass"
-- with ok=false; pin the new contract.
testCheckGateRegressed :: IO Bool
testCheckGateRegressed =
  let g = CheckModule.propertiesGate 3 1 2 0
  in pure $ gateField "ok" g == Just (A.Bool False)
        && gateField "status" g == Just (A.String "regressed")
        && case gateField "reason" g of
             Just (A.String r) ->
                  "regressed" `T.isInfixOf` r
               && not ("pass" `T.isInfixOf` r)
             _ -> False

-- | Issue #42 + #51: load-failures → status="skipped", ok=false,
-- reason calls out the load failure (not "regressed").
testCheckGateSkipped :: IO Bool
testCheckGateSkipped =
  let g = CheckModule.propertiesGate 2 0 0 2
  in pure $ gateField "ok" g == Just (A.Bool False)
        && gateField "status" g == Just (A.String "skipped")
        && case gateField "reason" g of
             Just (A.String r) ->
               "load" `T.isInfixOf` T.toLower r
             _ -> False
