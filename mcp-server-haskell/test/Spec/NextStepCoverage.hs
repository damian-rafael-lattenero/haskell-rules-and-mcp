-- | Unit tests for nextStep routing: info/doc/goto no-match routes,
-- coverage exhaustiveness, action coverage, suppress guards, and
-- JSON-inject splices.
--
-- Extracted from the Spec.hs monolith (#271) via the function-export shape.
module Spec.NextStepCoverage where

import qualified Data.Aeson as A
import Data.Aeson (Value, object, (.=))
import qualified Data.Aeson.Key as AKey
import qualified Data.Aeson.KeyMap as AKM
import qualified Data.Text.Lazy as TL
import qualified Data.Text.Lazy.Encoding as TLE
import Data.Text (Text)
import qualified Data.Text as T

import Data.Maybe (isNothing, isJust)
import qualified Data.Set as Set
import Control.Monad (unless)
import HaskellFlows.Mcp.NextStep
import qualified HaskellFlows.Mcp.NextStep as NextStep
import HaskellFlows.Mcp.Protocol (ToolContent (..), ToolResult (..))
import HaskellFlows.Mcp.ToolName (allToolNames, ToolName (..))
-- injectNextStep is re-exported from HaskellFlows.Mcp.NextStep (already imported above)

-- | #185: ghc_info on a name not found (status=no_match) must route to
-- hoogle_search, not ghc_doc (which will also no_match on the same name).

