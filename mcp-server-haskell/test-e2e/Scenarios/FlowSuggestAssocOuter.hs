-- | Flow: 'ghc_suggest' Associative law template applies the
-- function at the outer LHS call (#52).
--
-- Pre-fix behaviour
-- -----------------
-- The Associative rule emitted
-- @\\x y z -> (op x y) z == op x (op y z)@. The LHS \"@(op x y) z@\"
-- type-checks as \"apply the result of @op x y :: a@ to @z@\",
-- which is a type error whenever @a@ is not a function — the
-- common case. Agents that fed the suggestion straight into
-- 'ghc_quickcheck' got a parse / type error instead of the law.
--
-- New contract
-- ------------
-- The template applies the outer @op@ explicitly:
-- @\\x y z -> op (op x y) z == op x (op y z)@. This is the
-- canonical Associative law and compiles for every
-- @op :: a -> a -> a@ sig the rule fires on.
module Scenarios.FlowSuggestAssocOuter
  ( runFlow
  ) where

import Data.Aeson (Value (..), object, (.=))
import qualified Data.Aeson.Key as Key
import qualified Data.Aeson.KeyMap as KeyMap
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.IO as TIO
import qualified Data.Vector as V
import System.Directory (createDirectoryIfMissing)
import System.FilePath ((</>))

import E2E.Assert
  ( Check (..)
  , checkPure
  , liveCheck
  , stepFooter
  , stepHeader
  )
import qualified E2E.Client as Client
import HaskellFlows.Mcp.ToolName (ToolName (..))

assocSrc :: Text
assocSrc = T.unlines
  [ "module AssocDemo where"
  , ""
  , "import Data.List (sort)"
  , ""
  , "-- combineSorted :: Ord a => [a] -> [a] -> [a]"
  , "-- Binary list combinator that should be associative."
  , "combineSorted :: Ord a => [a] -> [a] -> [a]"
  , "combineSorted xs ys = sort (xs ++ ys)"
  ]

runFlow :: Client.McpClient -> FilePath -> IO [Check]
runFlow c projectDir = do
  -- Step 1 — scaffold + write the source so 'ghc_suggest' has a
  -- real type signature to read.
  _ <- Client.callTool c GhcCreateProject
         (object [ "name" .= ("assoc-demo" :: Text) ])
  _ <- Client.callTool c GhcAddModules
         (object [ "modules" .= (["AssocDemo"] :: [Text]) ])
  createDirectoryIfMissing True (projectDir </> "src")
  TIO.writeFile (projectDir </> "src" </> "AssocDemo.hs") assocSrc
  _ <- Client.callTool c GhcLoad
         (object [ "module_path" .= ("src/AssocDemo.hs" :: Text) ])

  -- Step 2 — ghc_suggest must include an Associative entry whose
  -- property string applies @combineSorted@ at the outer LHS.
  t0 <- stepHeader 1 "ghc_suggest emits valid Associative template (#52)"
  r <- Client.callTool c GhcSuggest
         (object [ "function_name" .= ("combineSorted" :: Text) ])
  let assocProp = findAssociativeProperty r
      hasOuter  = case assocProp of
        Just p  -> T.isInfixOf "combineSorted (combineSorted x y) z" p
        Nothing -> False
      noBugLhs = case assocProp of
        Just p  -> not (T.isInfixOf "-> (combineSorted x y) z" p)
        Nothing -> False
  cTemplate <- liveCheck $ checkPure
    "Associative property applies combineSorted at outer LHS"
    (hasOuter && noBugLhs)
    ( "Expected: 'combineSorted (combineSorted x y) z' on LHS, NOT \
      \'-> (combineSorted x y) z'. \
      \Got property: " <> maybe "<no Associative suggestion>" id assocProp
      <> ". Raw: " <> truncRender r )
  stepFooter 1 t0

  pure [cTemplate]

--------------------------------------------------------------------------------
-- helpers
--------------------------------------------------------------------------------

-- | Walk the @suggestions@ array of a @ghc_suggest@ response and
-- return the first @property@ whose @law@ field is "Associative".
findAssociativeProperty :: Value -> Maybe Text
findAssociativeProperty v = case lookupField "suggestions" v of
  Just (Array xs) ->
    let assoc = [ obj | obj <- V.toList xs
                      , objField "law" obj == Just (String "Associative") ]
    in case assoc of
         (a : _) -> case objField "property" a of
                      Just (String p) -> Just p
                      _               -> Nothing
         []      -> Nothing
  _ -> Nothing
  where
    objField k (Object o) = KeyMap.lookup (Key.fromText k) o
    objField _ _          = Nothing

lookupField :: Text -> Value -> Maybe Value
lookupField k (Object o) = KeyMap.lookup (Key.fromText k) o
lookupField _ _          = Nothing

truncRender :: Value -> Text
truncRender v =
  let raw = T.pack (show v)
      cap = 600
  in if T.length raw > cap then T.take cap raw <> "…(truncated)" else raw
