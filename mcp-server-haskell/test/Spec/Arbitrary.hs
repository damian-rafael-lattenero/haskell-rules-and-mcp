-- | Unit tests for 'Tool.Arbitrary' constructor parsing, template rendering,
-- recursion detection, and the ghc_goto descriptor check.
--
-- Extracted from the Spec.hs monolith (#271) via the function-export shape.
module Spec.Arbitrary
  ( testArbitraryDetectsRecursion
  , testArbitraryExprSized
  , testArbitraryTreeSized
  , testArbitraryFlatTemplate
  , testArbitraryRecursionTokens
  , testArbitraryCompileFailShape
  , testArbitraryWiredInMessage
  , testArbitraryHasUnboxedConstructor
  , testArbitraryFirstJustSource
  , testArbitraryTwoParamTemplate
  , testGhcGotoDescriptorAccurate
  ) where

import qualified Data.Aeson as A
import qualified Data.Aeson.KeyMap as AKM
import qualified Data.ByteString.Lazy as BL
import qualified Data.Text as T
import qualified Data.Text.IO as TIO
import qualified Data.Text.Lazy as TL
import qualified Data.Text.Lazy.Encoding as TLE

import HaskellFlows.Mcp.Protocol (ToolContent (..), ToolResult (..))
import HaskellFlows.Tool.Arbitrary
  ( Constructor (..)
  , compileFailedErr
  , hasRecursiveConstructor
  , hasUnboxedConstructor
  , isRecursiveArg
  , parseConstructors
  , parseTypeParams
  , renderArbitraryModule
  , renderTemplate
  )

import Spec.Helpers (withTempProject)

testArbitraryDetectsRecursion :: IO Bool
testArbitraryDetectsRecursion =
  let expr =
        [ Constructor "Lit" ["Int"]
        , Constructor "Neg" ["Expr"]
        , Constructor "Add" ["Expr", "Expr"]
        ]
      tree =
        [ Constructor "Leaf" ["a"]
        , Constructor "Node" ["(Tree a)", "(Tree a)"]
        ]
      status =
        [ Constructor "Ok" []
        , Constructor "Err" ["String"]
        ]
  in pure $ hasRecursiveConstructor "Expr"   expr
         && hasRecursiveConstructor "Tree"   tree
         && not (hasRecursiveConstructor "Status" status)

-- | BUG-17 core: a recursive Expr must produce the 'sized'
-- template shape — 'sized go', a base 'oneof' branch, a
-- recursive 'frequency' branch, and 'go (n `div` 2)' in each
-- recursive arg position. If the template ever reverts to naive
-- 'oneof' for a recursive type, QuickCheck will OOM on the
-- first sample with default size.
testArbitraryExprSized :: IO Bool
testArbitraryExprSized =
  let ctors =
        [ Constructor "Lit" ["Int"]
        , Constructor "Var" ["String"]
        , Constructor "Neg" ["Expr"]
        , Constructor "Add" ["Expr", "Expr"]
        , Constructor "Mul" ["Expr", "Expr"]
        ]
      out = renderTemplate "Expr" [] ctors
  in pure $ T.isInfixOf "instance Arbitrary Expr where" out
         && T.isInfixOf "arbitrary = sized go"          out
         && T.isInfixOf "go 0 = oneof"                  out
         && T.isInfixOf "go n = frequency"              out
         && T.isInfixOf "Lit <$> arbitrary"             out
         && T.isInfixOf "Neg <$> go (n `div` 2)"        out
         && T.isInfixOf "Add <$> go (n `div` 2) <*> go (n `div` 2)" out

-- | Polymorphic recursive type: 'Tree a' should emit the sized
-- template AND the proper 'Arbitrary a =>' context.
testArbitraryTreeSized :: IO Bool
testArbitraryTreeSized =
  let ctors =
        [ Constructor "Leaf" ["a"]
        , Constructor "Node" ["(Tree a)", "(Tree a)"]
        ]
      out = renderTemplate "Tree" ["a"] ctors
  in pure $ T.isInfixOf "instance Arbitrary a => Arbitrary (Tree a) where" out
         && T.isInfixOf "arbitrary = sized go"                out
         && T.isInfixOf "Leaf <$> arbitrary"                  out
         && T.isInfixOf "Node <$> go (n `div` 2) <*> go (n `div` 2)" out

-- | Non-recursive types keep the classical flat template —
-- 'sized' is pure overhead without recursion.
testArbitraryFlatTemplate :: IO Bool
testArbitraryFlatTemplate =
  let ctors =
        [ Constructor "Ok"  []
        , Constructor "Err" ["String"]
        ]
      out = renderTemplate "Status" [] ctors
  in pure $ T.isInfixOf "arbitrary = oneof"       out
         && not (T.isInfixOf "sized"       out)
         && not (T.isInfixOf "frequency"   out)
         && T.isInfixOf "pure Ok"                 out
         && T.isInfixOf "Err <$> arbitrary"       out

-- | 'isRecursiveArg' must tokenise on non-identifier characters
-- so paren / bracket / comma-separated arg positions pick up the
-- type name cleanly. Pin the tokeniser shape.
testArbitraryRecursionTokens :: IO Bool
testArbitraryRecursionTokens = pure $
     isRecursiveArg "Tree" "(Tree a)"
  && isRecursiveArg "Tree" "Maybe (Tree a)"
  && isRecursiveArg "Tree" "[Tree a]"
  && not (isRecursiveArg "Tree" "TreeLike a")   -- different identifier
  && not (isRecursiveArg "Tree" "Int")
  && not (isRecursiveArg "Tree" "String")

-- | Issue #210: when loadForTarget returns (False, errs),
-- 'compileFailedErr' must produce status='failed' with
-- kind='validation' and a message mentioning the error count.
testArbitraryCompileFailShape :: IO Bool
testArbitraryCompileFailShape =
  let result = compileFailedErr 3
  in pure $ case trContent result of
       [TextContent body_] ->
         case A.decode (TLE.encodeUtf8 (TL.fromStrict body_)) of
           Just (A.Object top) ->
             AKM.lookup "status" top == Just (A.String "failed")
             && case AKM.lookup "error" top of
                  Just (A.Object err) ->
                    AKM.lookup "kind" err == Just (A.String "validation")
                    && case AKM.lookup "message" err of
                         Just (A.String msg) -> "3 compile error(s)" `T.isInfixOf` msg
                         _                   -> False
                  _ -> False
           _ -> False
       _ -> False

-- | Issue #218: when getInfo returns Nothing (GHC wired-in primitives
-- like Bool after -hide-all-packages stanza flags), 'handle' must NOT
-- say "not in scope" — it must say "cannot introspect" with the hint
-- that QuickCheck already provides an instance.
--
-- We test via the source text rather than running the full GHC session
-- to keep this a fast unit test. The patch is in the Right Nothing
-- branch of 'handle'.
testArbitraryWiredInMessage :: IO Bool
testArbitraryWiredInMessage = do
  src <- TIO.readFile "src/HaskellFlows/Tool/Arbitrary.hs"
  -- The new message must NOT say "not in scope"
  let noOldMsg = not ("not in scope (getInfo=Nothing)" `T.isInfixOf` src)
  -- The new message MUST mention "wired-in" or "introspect"
  let hasNewMsg = "cannot introspect" `T.isInfixOf` src
                || "wired-in" `T.isInfixOf` src
  -- The Right Nothing branch must use validationErr not notInScopeErr
  let usesValidation = "Right Nothing ->" `T.isInfixOf` src
                     && "validationErr" `T.isInfixOf` src
  pure (noOldMsg && hasNewMsg && usesValidation)

-- | Issue #219: 'hasUnboxedConstructor' detects constructors whose
-- name ends with '#' (I#, C#, W#, …). A constructor that does NOT
-- end with '#' must return False.
testArbitraryHasUnboxedConstructor :: IO Bool
testArbitraryHasUnboxedConstructor = pure $
  -- Typical unboxed-primop constructors
     hasUnboxedConstructor (Constructor { cName = "I#", cArgs = ["Int#"] })
  && hasUnboxedConstructor (Constructor { cName = "C#", cArgs = ["Char#"] })
  && hasUnboxedConstructor (Constructor { cName = "W#", cArgs = ["Word#"] })
  -- Regular constructors must NOT be flagged
  && not (hasUnboxedConstructor (Constructor { cName = "Just",  cArgs = ["a"] }))
  && not (hasUnboxedConstructor (Constructor { cName = "False", cArgs = [] }))
  && not (hasUnboxedConstructor (Constructor { cName = "PathNotAbsolute", cArgs = ["Text"] }))

-- | #226: 'renderTyThing' must try all names returned by 'parseName'
-- rather than blindly taking the first one.  When an external-package
-- type shares an unqualified name with a home-module type, taking only
-- the first name causes getInfo to return Nothing → "wired-in primitive"
-- message.  Verified by checking the source uses the multi-name loop.
testArbitraryFirstJustSource :: IO Bool
testArbitraryFirstJustSource = do
  src <- TIO.readFile "src/HaskellFlows/Tool/Arbitrary.hs"
  let usesMapM    = "mapM tryName" `T.isInfixOf` src
      hasFirstJust = "firstJust"   `T.isInfixOf` src
      noNilTail   = not ("n :| _ <- parseName" `T.isInfixOf` src)
  pure (usesMapM && hasFirstJust && noNilTail)

-- | #226: the pure template path correctly handles two-param types.
-- renderTemplate with params ["a","b"] produces the right instance header.
testArbitraryTwoParamTemplate :: IO Bool
testArbitraryTwoParamTemplate =
  let raw = T.unlines
        [ "data Tagged a b = Tagged a b"
        ]
      params = parseTypeParams raw
      ctors  = parseConstructors raw
      tmpl   = renderTemplate "Tagged" params ctors
  in pure $ params == ["a", "b"]
         && "(Arbitrary a, Arbitrary b) => Arbitrary (Tagged a b)" `T.isInfixOf` tmpl

-- | Issue #217: 'ghc_goto' descriptor must acknowledge that source
-- locations are only available for interpreted modules and that most
-- project modules compile to object code.
testGhcGotoDescriptorAccurate :: IO Bool
testGhcGotoDescriptorAccurate = do
  src <- TIO.readFile "src/HaskellFlows/Tool/Goto.hs"
  -- Descriptor must mention the compiled-mode limitation
  let mentionsCompiled = "compiled" `T.isInfixOf` src
  -- Descriptor must mention interpreted mode (byte-code)
  let mentionsByteCode = "interpreted" `T.isInfixOf` src
                       || "byte-code"  `T.isInfixOf` src
  pure (mentionsCompiled && mentionsByteCode)
