-- | Flow: @ghc_arbitrary@ template generation across three
-- canonical input shapes.
--
-- Exercises each branch of 'renderTemplate':
--
--   (1) Non-recursive ADT → classical 'oneof' template
--       ('pure Ctor' for nullary, 'Ctor <$> arbitrary <*> …'
--       for n-ary).
--   (2) Recursive ADT → 'sized' template with 'frequency',
--       half-size on every recursive arg position (BUG-17
--       fix).
--   (3) Polymorphic recursive ADT → 'Arbitrary a => Arbitrary
--       (Tree a)' constraint context PLUS the sized body.
--
-- Tools exercised:
--
--   ghc_arbitrary   (pure-query; no state mutation)
--
-- Indirectly: ghc_create_project, ghc_add_modules, ghc_load.
module Scenarios.FlowArbitrary
  ( runFlow
  ) where

import Data.Aeson (Value (..), object, (.=))
import qualified Data.Aeson.Key as Key
import qualified Data.Aeson.KeyMap as KeyMap
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.IO as TIO
import System.Directory (createDirectoryIfMissing)
import System.FilePath ((</>))

import E2E.Assert
  ( Check (..)
  , checkJsonField
  , checkJsonFieldMatches
  , liveCheck
  , stepFooter
  , stepHeader
  )
import qualified E2E.Client as Client
import E2E.Envelope (statusOk, lookupField)
import HaskellFlows.Mcp.ToolName (ToolName (..))

--------------------------------------------------------------------------------
-- source with 3 data types — one per template variant
--------------------------------------------------------------------------------

shapesSrc :: Text
shapesSrc =
  "-- | Three ADTs exercising each branch of the ghc_arbitrary\n\
  \-- template engine.\n\
  \module Shapes\n\
  \  ( Status (..)\n\
  \  , Expr (..)\n\
  \  , Tree (..)\n\
  \  ) where\n\
  \\n\
  \-- Flat sum: nullary + unary. Goes through the classical\n\
  \-- 'oneof' branch with 'pure Ok' + 'Err <$> arbitrary'.\n\
  \data Status = Ok | Err String\n\
  \  deriving stock (Eq, Show)\n\
  \\n\
  \-- Recursive monotype: at least one constructor carries the\n\
  \-- focal type name. Triggers the 'sized' template.\n\
  \data Expr\n\
  \  = Lit Int\n\
  \  | Add Expr Expr\n\
  \  | Mul Expr Expr\n\
  \  deriving stock (Eq, Show)\n\
  \\n\
  \-- Polymorphic recursive: requires the 'Arbitrary a =>'\n\
  \-- constraint context alongside the sized body.\n\
  \data Tree a = Leaf | Node a (Tree a) (Tree a)\n\
  \  deriving stock (Eq, Show)\n"

--------------------------------------------------------------------------------
-- runFlow
--------------------------------------------------------------------------------

runFlow :: Client.McpClient -> FilePath -> IO [Check]
runFlow c projectDir = do
  ----------------------------------------------------------------
  -- setup
  ----------------------------------------------------------------
  t0 <- stepHeader 1 "scaffold + write Shapes + load"
  _ <- Client.callTool c GhcCreateProject
         (object [ "name" .= ("arbitrary-demo" :: Text) ])
  _ <- Client.callTool c GhcAddModules
         (object [ "modules" .= (["Shapes"] :: [Text]) ])
  createDirectoryIfMissing True (projectDir </> "src")
  TIO.writeFile (projectDir </> "src" </> "Shapes.hs") shapesSrc
  _ <- Client.callTool c GhcLoad
         (object [ "module_path" .= ("src/Shapes.hs" :: Text) ])
  stepFooter 1 t0

  ----------------------------------------------------------------
  -- (1) Status: flat ADT → classical oneof template
  ----------------------------------------------------------------
  t1 <- stepHeader 2 "ghc_arbitrary(Status) — flat oneof"
  r1 <- Client.callTool c GhcArbitrary
          (object [ "type_name" .= ("Status" :: Text) ])
  c1 <- liveCheck $ checkJsonField
          "Status · success" r1 "success" (Bool True)
  c2 <- liveCheck $ checkJsonFieldMatches
          "Status · template uses 'oneof'"
          r1 "template" (containsStr "arbitrary = oneof")
          "flat ADT should emit the classical oneof template"
  c3 <- liveCheck $ checkJsonFieldMatches
          "Status · template has 'pure Ok' for nullary ctor"
          r1 "template" (containsStr "pure Ok")
          "nullary constructor Ok must render as 'pure Ok'"
  c4 <- liveCheck $ checkJsonFieldMatches
          "Status · template has 'Err <$> arbitrary' for unary"
          r1 "template" (containsStr "Err <$> arbitrary")
          "unary constructor Err String must render as 'Err <$> arbitrary'"
  c5 <- liveCheck $ checkJsonFieldMatches
          "Status · NO 'sized' on a flat ADT"
          r1 "template" (notContaining "sized")
          "flat ADT must NOT use the sized template"
  stepFooter 2 t1

  ----------------------------------------------------------------
  -- (2) Expr: recursive → sized template (BUG-17 core)
  ----------------------------------------------------------------
  t2 <- stepHeader 3 "ghc_arbitrary(Expr) — sized recursive (BUG-17)"
  r2 <- Client.callTool c GhcArbitrary
          (object [ "type_name" .= ("Expr" :: Text) ])
  c6 <- liveCheck $ checkJsonField
          "Expr · success" r2 "success" (Bool True)
  c7 <- liveCheck $ checkJsonFieldMatches
          "Expr · template uses 'sized go'"
          r2 "template" (containsStr "arbitrary = sized go")
          "recursive ADT must use the sized template"
  c8 <- liveCheck $ checkJsonFieldMatches
          "Expr · template has 'go 0 = oneof' base case"
          r2 "template" (containsStr "go 0 = oneof")
          "size-zero base case must be an oneof of leaves"
  c9 <- liveCheck $ checkJsonFieldMatches
          "Expr · template has 'go n = frequency' recursive case"
          r2 "template" (containsStr "go n = frequency")
          "size-n case must use frequency to bias toward leaves"
  c10 <- liveCheck $ checkJsonFieldMatches
          "Expr · recursive positions use 'go (n `div` 2)'"
          r2 "template" (containsStr "go (n `div` 2)")
          "each recursive arg position must halve the size — the \
          \logarithmic-depth guarantee that prevents QC size-bombs"
  stepFooter 3 t2

  ----------------------------------------------------------------
  -- (3) Tree a: polymorphic recursive → Arbitrary constraint
  ----------------------------------------------------------------
  t3 <- stepHeader 4 "ghc_arbitrary(Tree) — polymorphic + sized"
  r3 <- Client.callTool c GhcArbitrary
          (object [ "type_name" .= ("Tree" :: Text) ])
  c11 <- liveCheck $ checkJsonField
          "Tree · success" r3 "success" (Bool True)
  c12 <- liveCheck $ checkJsonFieldMatches
          "Tree · polymorphic instance header"
          r3 "template"
          (containsStr "instance Arbitrary a => Arbitrary (Tree a)")
          "polymorphic instance must carry the 'Arbitrary a =>' context"
  c13 <- liveCheck $ checkJsonFieldMatches
          "Tree · ALSO uses sized (recursive)"
          r3 "template" (containsStr "arbitrary = sized go")
          "Tree is recursive AND polymorphic — both should compose"
  c14 <- liveCheck $ checkJsonFieldMatches
          "Tree · recursive branches half the size"
          r3 "template" (containsStr "go (n `div` 2)")
          "recursive branches of a polymorphic type must still halve"
  stepFooter 4 t3

  ----------------------------------------------------------------
  -- (4) BUG-FINDING ORACLE: paste the 3 generated templates into a
  -- real module and ghc_load it. If ANY template has a typo, a
  -- missing import, an unbalanced paren, or renders a constructor
  -- that doesn't exist, load will fail.
  --
  -- The earlier steps check substring patterns in the template
  -- string; this step closes the "string looks right but code
  -- doesn't compile" hole. The test now catches:
  --   * missing newline between ctor lines
  --   * generated code that references an undefined symbol
  --   * 'Arbitrary a =>' constraint dropped on polymorphic types
  --   * misrendered operator precedence in the sized body
  ----------------------------------------------------------------
  t4 <- stepHeader 5 "compile oracle · paste 3 templates + ghc_load"

  -- Pull the template strings out of the three responses. If any
  -- of them returned Null, the file write uses an empty stub and
  -- load will fail loudly (which is the right signal).
  let tpl1 = extractTemplate r1
      tpl2 = extractTemplate r2
      tpl3 = extractTemplate r3
      genSrc = T.unlines
        [ "{-# OPTIONS_GHC -Wno-orphans -Wno-missing-signatures #-}"
        , "module ShapesGen () where"
        , ""
        , "import Shapes"
        , "import Test.QuickCheck"
        , ""
        , tpl1
        , ""
        , tpl2
        , ""
        , tpl3
        ]
  TIO.writeFile (projectDir </> "src" </> "ShapesGen.hs") genSrc

  _ <- Client.callTool c GhcAddModules
         (object [ "modules" .= (["ShapesGen"] :: [Text]) ])
  -- The session needs QuickCheck in scope to resolve 'Arbitrary',
  -- 'arbitrary', 'oneof', 'sized', 'frequency'. It's already a
  -- build-depends in the library stanza (cabal repl injects it),
  -- but we add it to the library explicitly so the generated
  -- module loads under its own power, not via the test-only scope.
  _ <- Client.callTool c GhcDeps
         (object
           [ "action"  .= ("add" :: Text)
           , "package" .= ("QuickCheck" :: Text)
           , "stanza"  .= ("library" :: Text)
           ])
  loadGen <- Client.callTool c GhcLoad
               (object [ "module_path" .= ("src/ShapesGen.hs" :: Text) ])
  c15 <- liveCheck $ Check
    { cName   = "3 generated Arbitrary instances compile together"
    , cOk     = statusOk loadGen == Just True
             && case lookupField "errors" loadGen of
                  Just (Array xs) -> null xs
                  _               -> True  -- missing errors field = ok
    , cDetail = "If this fails, at least one template from \
                \ghc_arbitrary produced non-compiling code. That's \
                \a real bug — the template string can look correct \
                \to substring matchers (steps 2-4 above) yet not \
                \type-check. Raw: "
                <> renderShort loadGen
    }
  stepFooter 5 t4

  pure [c1, c2, c3, c4, c5, c6, c7, c8, c9, c10, c11, c12, c13, c14, c15]

--------------------------------------------------------------------------------
-- helpers
--------------------------------------------------------------------------------

containsStr :: Text -> Value -> Bool
containsStr needle (String s) = needle `T.isInfixOf` s
containsStr _      _          = False

notContaining :: Text -> Value -> Bool
notContaining needle (String s) = not (needle `T.isInfixOf` s)
notContaining _      _          = False

-- | Pull the 'template' string out of a ghc_arbitrary response.
-- Empty text on missing — ghc_load will then fail on an empty
-- stub, which is the right signal.
extractTemplate :: Value -> Text
extractTemplate v = case lookupField "template" v of
  Just (String s) -> s
  _               -> T.empty

renderShort :: Value -> Text
renderShort v =
  let s = T.pack (show v)
  in if T.length s > 400 then T.take 400 s <> "…(truncated)" else s
