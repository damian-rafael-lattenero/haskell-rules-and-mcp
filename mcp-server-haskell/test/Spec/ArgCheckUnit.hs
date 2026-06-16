-- | B-1: unit tests for 'HaskellFlows.Mcp.ArgCheck' — the non-blocking
-- unknown-argument detector + did-you-mean.
module Spec.ArgCheckUnit
  ( testSchemaPropertyNames
  , testUnknownArgKeys
  , testDidYouMeanBaseDir
  , testUnknownArgsWarningFires
  , testUnknownArgsWarningSilentWhenClean
  ) where

import qualified Data.Aeson as A
import Data.Aeson ((.=))
import Data.List (sort)
import qualified Data.Text as T

import qualified HaskellFlows.Mcp.Envelope as Env
import qualified HaskellFlows.Mcp.ArgCheck as ArgCheck

-- A schema shaped like a real tdInputSchema (flat 'properties' map —
-- discriminatedSchema already merges action branches into one).
sampleSchema :: A.Value
sampleSchema = A.object
  [ "type" .= ("object" :: T.Text)
  , "properties" .= A.object
      [ "name"      .= A.object [ "type" .= ("string" :: T.Text) ]
      , "path"      .= A.object [ "type" .= ("string" :: T.Text) ]
      , "overwrite" .= A.object [ "type" .= ("boolean" :: T.Text) ]
      ]
  , "required" .= (["name"] :: [T.Text])
  ]

testSchemaPropertyNames :: IO Bool
testSchemaPropertyNames = pure $
     sort (ArgCheck.schemaPropertyNames sampleSchema) == ["name", "overwrite", "path"]
  && ArgCheck.schemaPropertyNames (A.object []) == []

testUnknownArgKeys :: IO Bool
testUnknownArgKeys =
  let declared = ArgCheck.schemaPropertyNames sampleSchema
      args = A.object [ "name" .= ("x" :: T.Text), "base_dir" .= ("/tmp" :: T.Text) ]
  in pure $ ArgCheck.unknownArgKeys declared args == ["base_dir"]
         -- a fully-recognised call yields no unknown keys
         && ArgCheck.unknownArgKeys declared (A.object [ "name" .= ("x" :: T.Text) ]) == []

-- | The exact dogfooding case: 'base_dir' is one edit-cluster away from
-- 'path'? No — but it should still suggest the closest, and crucially
-- 'pat' → 'path'. Pin the realistic near-miss.
testDidYouMeanBaseDir :: IO Bool
testDidYouMeanBaseDir =
  let declared = ["name", "path", "overwrite"]
  in pure $ ArgCheck.didYouMean declared "pathh"  == Just "path"
         && ArgCheck.didYouMean declared "nam"    == Just "name"
         -- something with no close match returns Nothing
         && ArgCheck.didYouMean declared "xyzzyqwer" == Nothing

testUnknownArgsWarningFires :: IO Bool
testUnknownArgsWarningFires =
  let declared = ArgCheck.schemaPropertyNames sampleSchema
      args = A.object [ "name" .= ("x" :: T.Text), "bogus" .= (1 :: Int) ]
  in pure $ case ArgCheck.unknownArgsWarning declared args of
       Just w  -> Env.wKind w == Env.SlowPath
               && "bogus" `T.isInfixOf` Env.wMessage w
       Nothing -> False

testUnknownArgsWarningSilentWhenClean :: IO Bool
testUnknownArgsWarningSilentWhenClean =
  let declared = ArgCheck.schemaPropertyNames sampleSchema
      args = A.object [ "name" .= ("x" :: T.Text), "path" .= ("/tmp" :: T.Text) ]
  in pure $ case ArgCheck.unknownArgsWarning declared args of
       Nothing -> True
       Just _  -> False
