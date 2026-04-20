-- | Flow: @ghci_arbitrary@ template generation across three
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
--   ghci_arbitrary   (pure-query; no state mutation)
--
-- Indirectly: ghci_create_project, ghci_add_modules, ghci_load.
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

--------------------------------------------------------------------------------
-- source with 3 data types — one per template variant
--------------------------------------------------------------------------------

shapesSrc :: Text
shapesSrc =
  "-- | Three ADTs exercising each branch of the ghci_arbitrary\n\
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
  _ <- Client.callTool c "ghci_create_project"
         (object [ "name" .= ("arbitrary-demo" :: Text) ])
  _ <- Client.callTool c "ghci_add_modules"
         (object [ "modules" .= (["Shapes"] :: [Text]) ])
  createDirectoryIfMissing True (projectDir </> "src")
  TIO.writeFile (projectDir </> "src" </> "Shapes.hs") shapesSrc
  _ <- Client.callTool c "ghci_load"
         (object [ "module_path" .= ("src/Shapes.hs" :: Text) ])
  stepFooter 1 t0

  ----------------------------------------------------------------
  -- (1) Status: flat ADT → classical oneof template
  ----------------------------------------------------------------
  t1 <- stepHeader 2 "ghci_arbitrary(Status) — flat oneof"
  r1 <- Client.callTool c "ghci_arbitrary"
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
  t2 <- stepHeader 3 "ghci_arbitrary(Expr) — sized recursive (BUG-17)"
  r2 <- Client.callTool c "ghci_arbitrary"
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
  t3 <- stepHeader 4 "ghci_arbitrary(Tree) — polymorphic + sized"
  r3 <- Client.callTool c "ghci_arbitrary"
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

  pure [c1, c2, c3, c4, c5, c6, c7, c8, c9, c10, c11, c12, c13, c14]

--------------------------------------------------------------------------------
-- helpers
--------------------------------------------------------------------------------

containsStr :: Text -> Value -> Bool
containsStr needle (String s) = needle `T.isInfixOf` s
containsStr _      _          = False

notContaining :: Text -> Value -> Bool
notContaining needle (String s) = not (needle `T.isInfixOf` s)
notContaining _      _          = False

_unused :: KeyMap.KeyMap Value -> Key.Key
_unused _ = Key.fromText ""
