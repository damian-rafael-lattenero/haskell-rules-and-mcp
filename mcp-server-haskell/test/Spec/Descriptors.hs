-- | Unit tests for tool-descriptor invariants (#92D, #95): schema validity,
-- nextStep references, name/description constraints, and chain shape.
--
-- Extracted from the Spec.hs monolith (#271) via the function-export shape.
module Spec.Descriptors
  ( testEveryToolPublishesValidSchema
  , testNextStepReferencesRegisteredToolsOnly
  , testEveryToolHasNonEmptyDescription
  , testEveryToolNameIsCanonical
  , testEveryToolNameIsShort
  , testEveryToolDescriptionIsSubstantive
  , testNextStepExampleIsObjectWhenPresent
  , testNextStepChainStepsCarryObjectArgs
  ) where

import qualified Data.Aeson as A
import Data.Aeson (object)
import qualified Data.Aeson.Key as AKey
import qualified Data.Aeson.KeyMap as AKM
import qualified Data.Set as Set
import qualified Data.Text as T
import qualified Data.Vector as Vector
import Data.Maybe (isNothing, maybeToList)
import Control.Monad (unless)

import HaskellFlows.Mcp.Server (allToolDescriptors, allToolNameTexts)
import HaskellFlows.Mcp.Protocol (ToolDescriptor (..))
import HaskellFlows.Mcp.NextStep
import qualified HaskellFlows.Mcp.NextStep as NextStep
import HaskellFlows.Mcp.ToolName
  ( ToolName (..)
  , allToolNames
  , toolNameText
  )

testEveryToolPublishesValidSchema :: IO Bool
testEveryToolPublishesValidSchema = do
  let invalid =
        [ tdName d
        | d <- allToolDescriptors
        , not (isValidSchema (tdInputSchema d))
        ]
  unless (null invalid) $
    putStrLn ("Tools with malformed schemas: " <> show invalid)
  pure (null invalid)
  where
    -- Post-flat-schema fix (Claude API top-level oneOf rejection):
    -- every tool must publish a flat object schema. Top-level
    -- 'oneOf' / 'allOf' / 'anyOf' fail HTTP registration with 400.
    isValidSchema (A.Object km) =
         AKM.lookup "type" km == Just (A.String "object")
      && isNothing (AKM.lookup "oneOf" km)
      && isNothing (AKM.lookup "allOf" km)
      && isNothing (AKM.lookup "anyOf" km)
      && case AKM.lookup "properties" km of
           Just (A.Object _) -> True
           _                 -> False
    isValidSchema _ = False

--------------------------------------------------------------------------------
-- Issue #95 Phase A (lite): nextStep dangling-reference detector
--
-- The full nextStep audit (suppression rules, signal-to-noise gates,
-- per-tool golden tests) is a multi-PR effort tracked on the
-- meta-issue. This commit lands one of its load-bearing anchors:
-- whenever 'suggestNext' returns a NextStep recommending some
-- tool, that tool MUST exist in 'allToolNames'. The type system
-- already enforces this at compile time (nsTool :: ToolName, an
-- ADT), but a runtime test is still useful as documentation of
-- the contract — a contributor renaming a tool sees the link
-- between the registry and the nextStep recommendations.
--
-- The chain (nsChain) is also covered: every step in a multi-step
-- plan must point at a registered tool.
--------------------------------------------------------------------------------

-- | Drive 'suggestNext' across all 46 tools with a generic
-- payload and assert every recommended tool (primary + chain) is
-- in 'allToolNames'.
testNextStepReferencesRegisteredToolsOnly :: IO Bool
testNextStepReferencesRegisteredToolsOnly = do
  let nameSet  = Set.fromList allToolNames
      payload  = object []
      attempts = [ suggestNext n True payload | n <- allToolNames ]
      bad      =
        [ recName
        | Just ns <- attempts
        , recName <- toolsReferencedBy ns
        , recName `Set.notMember` nameSet
        ]
  unless (null bad) $
    putStrLn ("Dangling nextStep tool refs: " <> show bad)
  pure (null bad)
  where
    toolsReferencedBy ns =
      nsTool ns
        : maybe [] (map csTool) (nsChain ns)

-- | Every registered tool descriptor must carry a non-empty
-- 'tdDescription'. The string surfaces in 'tools/list' and is
-- the primary affordance an LLM agent has for picking the right
-- tool — an empty / whitespace-only description is a real bug
-- (a host's selector menu would just show the bare tool name
-- with no context). This anchor catches a future contributor
-- who adds a tool with an empty 'tdDescription'.
testEveryToolHasNonEmptyDescription :: IO Bool
testEveryToolHasNonEmptyDescription = do
  let bad =
        [ tdName d
        | d <- allToolDescriptors
        , T.null (T.strip (tdDescription d))
        ]
  unless (null bad) $
    putStrLn ("Tools with empty description: " <> show bad)
  pure (null bad)

-- | Every tool's name must be in the canonical 'allToolNames'
-- ADT enumeration — protects against a tool whose descriptor
-- references a non-canonical or hand-stringified name. With
-- 'tdName' currently typed as 'Text' (rather than 'ToolName'
-- directly), this is the runtime invariant the type system
-- doesn't enforce on its own.
testEveryToolNameIsCanonical :: IO Bool
testEveryToolNameIsCanonical = do
  let nameSet = Set.fromList (map toolNameText allToolNames)
      bad =
        [ tdName d
        | d <- allToolDescriptors
        , tdName d `Set.notMember` nameSet
        ]
  unless (null bad) $
    putStrLn ("Tools with non-canonical name: " <> show bad)
  pure (null bad)

-- | Tool names should be reasonably short — they appear in
-- 'tools/list', in selector menus, and in nextStep
-- recommendations. Anchor: nothing should exceed 50 chars.
-- The current longest is well under that (around 24 chars
-- for 'ghc_quickcheck_export'); the invariant catches a
-- runaway case like a future @ghc_some_extremely_long_name@
-- before it ships.
testEveryToolNameIsShort :: IO Bool
testEveryToolNameIsShort = do
  let bad =
        [ (tdName d, T.length (tdName d))
        | d <- allToolDescriptors
        , T.length (tdName d) > 50
        ]
  unless (null bad) $
    putStrLn ("Tool names exceeding 50 chars: " <> show bad)
  pure (null bad)

-- | Tool descriptions should be substantive — at least 20
-- characters after stripping. Protects against a future
-- placeholder like 'tdDescription = "TODO"'.
testEveryToolDescriptionIsSubstantive :: IO Bool
testEveryToolDescriptionIsSubstantive = do
  let bad =
        [ tdName d
        | d <- allToolDescriptors
        , T.length (T.strip (tdDescription d)) < 20
        ]
  unless (null bad) $
    putStrLn ("Tools with too-short descriptions: " <> show bad)
  pure (null bad)

-- | When 'suggestNext' attaches an 'nsExample', it must be a
-- JSON Object — the @ghc_batch@ / direct-call shape an agent
-- can pass straight to the recommended tool. An Array or
-- primitive would never decode as args.
testNextStepExampleIsObjectWhenPresent :: IO Bool
testNextStepExampleIsObjectWhenPresent = do
  let payload  = object []
      attempts = [ (n, suggestNext n True payload) | n <- allToolNames ]
      bad =
        [ toolNameText n
        | (n, Just ns) <- attempts
        , Just v <- [nsExample ns]
        , not (isObject v)
        ]
  unless (null bad) $
    putStrLn ("Tools whose nextStep.example isn't an Object: " <> show bad)
  pure (null bad)
  where
    isObject (A.Object _) = True
    isObject _            = False

-- | Every 'nsChain' step must carry an Object as its args. A
-- non-Object would crash 'ghc_batch' when the agent forwards
-- the chain. This is the chain-time analogue of the
-- nsExample invariant above.
testNextStepChainStepsCarryObjectArgs :: IO Bool
testNextStepChainStepsCarryObjectArgs = do
  let payload  = object []
      attempts = [ (n, suggestNext n True payload) | n <- allToolNames ]
      bad =
        [ (toolNameText n, toolNameText (csTool s))
        | (n, Just ns) <- attempts
        , chain        <- maybeToList (nsChain ns)
        , s            <- chain
        , not (isObject (csArgs s))
        ]
  unless (null bad) $
    putStrLn ("Chain steps with non-Object args: " <> show bad)
  pure (null bad)
  where
    isObject (A.Object _) = True
    isObject _            = False
    maybeToList Nothing   = []
    maybeToList (Just xs) = [xs]
