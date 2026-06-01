-- | Unit tests for 'Tool.Move' slice/rewrite/export helpers: binding
-- extraction, module header parsing, export-list manipulation, and
-- bare-import detection. All pure.
--
-- Extracted from the Spec.hs monolith (#271) via the function-export shape.
module Spec.MoveUnit
  ( testMoveSliceFindsBinding
  , testMoveSliceAbsorbsHaddock
  , testMoveSliceMisses
  , testMoveRemoveSlice
  , testMoveInsertSlice
  , testMoveRewriteSelective
  , testMoveRewriteBare
  , testMoveRewriteQualified
  , testMoveModulePath
  , testMoveRemoveExport
  , testMoveRemoveExportOpen
  , testMoveAddDestExport
  , testMoveAddDestExportIdempotent
  , testMoveAddDestExportOpen
  , testMoveAddDestExportTypeCons
  , testMoveRemoveExportTypeCons
  , testCollectModuleHeaderSingle
  , testCollectModuleHeaderMulti
  , testMoveRemoveExportMultiLine
  , testMoveAddDestExportMultiLine
  , testMoveSequenceMultilineHeader236
  , testHasBareImportOfDetects
  , testHasBareImportOfQualified
  , testHasBareImportOfSelectiveMiss
  , testMoveSliceStopsAtHaddock
  ) where

import qualified Data.Text as T
import Data.Maybe (isNothing)

import qualified HaskellFlows.Tool.Move as MoveTool

-- | Issue #62: 'sliceTopLevelBinding' must find a column-0
-- signature and grow the slice down to the next top-level
-- binding's start.
testMoveSliceFindsBinding :: IO Bool
testMoveSliceFindsBinding =
  let body = T.unlines
        [ "module M where"
        , ""
        , "double :: Int -> Int"
        , "double x = x + x"
        , ""
        , "next :: Int -> Int"
        , "next y = y + 1"
        ]
  in case MoveTool.sliceTopLevelBinding "double" body of
       Just s ->
         pure $ "double :: Int -> Int" `T.isInfixOf` MoveTool.srSliced s
             && "double x = x + x"     `T.isInfixOf` MoveTool.srSliced s
             && not ("next" `T.isInfixOf` MoveTool.srSliced s)
       Nothing -> pure False

testMoveSliceAbsorbsHaddock :: IO Bool
testMoveSliceAbsorbsHaddock =
  let body = T.unlines
        [ "module M where"
        , ""
        , "-- | Doubles its input."
        , "-- Continues across lines."
        , "double :: Int -> Int"
        , "double x = x + x"
        ]
  in case MoveTool.sliceTopLevelBinding "double" body of
       Just s -> pure $
         "Doubles its input"   `T.isInfixOf` MoveTool.srSliced s
            && "double x = x + x" `T.isInfixOf` MoveTool.srSliced s
       Nothing -> pure False

testMoveSliceMisses :: IO Bool
testMoveSliceMisses =
  let body = T.unlines
        [ "module M where"
        , "double :: Int -> Int"
        , "double x = x + x"
        ]
  in pure (isNothing (MoveTool.sliceTopLevelBinding "missing" body))

testMoveRemoveSlice :: IO Bool
testMoveRemoveSlice =
  let body = T.unlines
        [ "module M where"
        , ""
        , "double :: Int -> Int"
        , "double x = x + x"
        , ""
        , "next :: Int"
        , "next = 0"
        ]
  in case MoveTool.sliceTopLevelBinding "double" body of
       Just s ->
         let after = MoveTool.removeSliceFromBody s body
         in pure $ not ("double" `T.isInfixOf` after)
                && "next" `T.isInfixOf` after
       Nothing -> pure False

testMoveInsertSlice :: IO Bool
testMoveInsertSlice =
  let body = T.unlines
        [ "module M where"
        , ""
        , "double :: Int -> Int"
        , "double x = x + x"
        ]
      destBody = T.unlines
        [ "module Dest where"
        , ""
        , "existing :: Int"
        , "existing = 0"
        ]
  in case MoveTool.sliceTopLevelBinding "double" body of
       Just s ->
         let merged = MoveTool.insertSliceAtEnd s destBody
         in pure $ "existing"            `T.isInfixOf` merged
                && "double :: Int -> Int" `T.isInfixOf` merged
                -- blank-line separator between existing + slice
                && T.isInfixOf "existing = 0\n\ndouble" merged
       Nothing -> pure False

-- | Issue #62: a consumer body with @import Foo (bar, double)@ and
-- a move of 'double' must split into
-- @import Foo (bar)@ + @import Bar (double)@ — preserving leading
-- whitespace.
testMoveRewriteSelective :: IO Bool
testMoveRewriteSelective =
  let body = T.unlines
        [ "module Other where"
        , ""
        , "import Foo (bar, double)"
        ]
      rewritten = MoveTool.rewriteImports "double" "Foo" "Bar" body
  in pure $ "import Foo (bar)"     `T.isInfixOf` rewritten
        && "import Bar (double)"   `T.isInfixOf` rewritten
        && not ("Foo (bar, double)" `T.isInfixOf` rewritten)

-- | Phase 1 deferral: bare 'import Foo' is left alone — verify
-- catches anything that breaks.
testMoveRewriteBare :: IO Bool
testMoveRewriteBare =
  let body = T.unlines [ "module Other where", "import Foo" ]
      rewritten = MoveTool.rewriteImports "double" "Foo" "Bar" body
  in pure $ "import Foo" `T.isInfixOf` rewritten
        && not ("import Bar" `T.isInfixOf` rewritten)

-- | Phase 1 deferral: 'import qualified Foo as F' is left alone.
testMoveRewriteQualified :: IO Bool
testMoveRewriteQualified =
  let body = T.unlines [ "module O where", "import qualified Foo as F" ]
      rewritten = MoveTool.rewriteImports "double" "Foo" "Bar" body
  in pure $ "import qualified Foo as F" `T.isInfixOf` rewritten
        && not ("import Bar" `T.isInfixOf` rewritten)

testMoveModulePath :: IO Bool
testMoveModulePath = pure $
     MoveTool.moduleNameToPath "Foo"          == "src/Foo.hs"
  && MoveTool.moduleNameToPath "Foo.Bar"      == "src/Foo/Bar.hs"
  && MoveTool.moduleNameToPath "Expr.Simplify" == "src/Expr/Simplify.hs"

-- | Issue #62: when the source module's header carries an
-- explicit export list with the moved symbol, the rewriter
-- drops the symbol from it. Without this, post-move load
-- fails with \"Not in scope\" on the export list.
testMoveRemoveExport :: IO Bool
testMoveRemoveExport =
  let body = T.unlines
        [ "module Source (greet, double) where"
        , ""
        , "double :: Int -> Int"
        , "double x = x + x"
        ]
      stripped = MoveTool.removeFromSourceExportList "double" body
  in pure $ T.isInfixOf "module Source (greet) where" stripped
        && not ("greet, double" `T.isInfixOf` stripped)

-- | Issue #62: open export ('module Foo where' with no parens)
-- is left unchanged.
testMoveRemoveExportOpen :: IO Bool
testMoveRemoveExportOpen =
  let body = T.unlines
        [ "module M where"
        , ""
        , "double = 42"
        ]
      stripped = MoveTool.removeFromSourceExportList "double" body
  in pure (stripped == body)

-- | Issue #76: 'addToDestinationExportList' must insert the
-- moved symbol into a destination header that declares an
-- explicit export list. Without this step, 'ghc_move' lands
-- the symbol in the file but it stays private.
testMoveAddDestExport :: IO Bool
testMoveAddDestExport =
  let body = T.unlines
        [ "module Dest (a, b) where"
        , ""
        , "a = 1"
        , "b = 2"
        ]
      out = MoveTool.addToDestinationExportList "moved" body
  in pure $ T.isInfixOf "module Dest (a, b, moved) where" out
         && T.isInfixOf "a = 1"  out  -- body untouched
         && T.isInfixOf "b = 2"  out

-- | Issue #76: idempotence — if the destination already exports
-- the symbol (e.g. a re-run of the move), the helper must not
-- duplicate the entry.
testMoveAddDestExportIdempotent :: IO Bool
testMoveAddDestExportIdempotent =
  let body = T.unlines
        [ "module Dest (a, moved, b) where"
        , "a = 1"
        ]
      out = MoveTool.addToDestinationExportList "moved" body
  in pure (out == body)

-- | Issue #76: open exports ('module Foo where') already export
-- every binding by default. The helper must leave them alone —
-- introducing a list would change the API surface.
testMoveAddDestExportOpen :: IO Bool
testMoveAddDestExportOpen =
  let body = T.unlines
        [ "module Dest where"
        , "a = 1"
        ]
      out = MoveTool.addToDestinationExportList "moved" body
  in pure (out == body)

-- | Issue #207: 'addToDestinationExportList' must not stop at the ')'
-- inside a Type(..) constructor export. The old T.breakOn ")" approach
-- misparsed @module Dest (Expr(..), eval) where@ as if the ')' inside
-- @Expr(..)@ was the export-list close, so the new symbol was never
-- appended.
testMoveAddDestExportTypeCons :: IO Bool
testMoveAddDestExportTypeCons =
  let body = T.unlines
        [ "module Dest (Expr(..), eval) where"
        , ""
        , "data Expr = Lit Int"
        , "eval :: Expr -> Int"
        , "eval (Lit n) = n"
        ]
      out = MoveTool.addToDestinationExportList "moved" body
  in pure $ T.isInfixOf "module Dest (Expr(..), eval, moved) where" out
         && T.isInfixOf "data Expr" out  -- body preserved

-- | Issue #207: 'removeFromSourceExportList' must also correctly parse
-- headers containing Type(..) constructor exports and not corrupt them.
testMoveRemoveExportTypeCons :: IO Bool
testMoveRemoveExportTypeCons =
  let body = T.unlines
        [ "module Source (Expr(..), eval, moved) where"
        , ""
        , "data Expr = Lit Int"
        ]
      out = MoveTool.removeFromSourceExportList "moved" body
      header = T.takeWhile (/= '\n') out
  in pure $ T.isInfixOf "module Source (Expr(..), eval) where" out
         && not ("moved" `T.isInfixOf` header)

-- | #228: collectModuleHeader handles a standard single-line header.
testCollectModuleHeaderSingle :: IO Bool
testCollectModuleHeaderSingle =
  let lns = [ "module Source (greet, double) where"
            , ""
            , "greet = \"hi\""
            ]
  in pure $ MoveTool.collectModuleHeader lns
         == Just (1, "module Source (greet, double) where")

-- | #228: collectModuleHeader collects all lines up to and including
-- the one ending with @where@.
testCollectModuleHeaderMulti :: IO Bool
testCollectModuleHeaderMulti =
  let lns = [ "module Source"
            , "  ( greet"
            , "  , double"
            , "  ) where"
            , ""
            , "greet = \"hi\""
            ]
  in pure $ MoveTool.collectModuleHeader lns
         == Just (4, "module Source ( greet , double ) where")

-- | #228: removeFromSourceExportList works on multi-line headers.
-- The multi-line header is collapsed to a single line after the rewrite.
testMoveRemoveExportMultiLine :: IO Bool
testMoveRemoveExportMultiLine =
  let body = T.unlines
        [ "module Source"
        , "  ( greet"
        , "  , double"
        , "  ) where"
        , ""
        , "greet = \"hi\""
        , "double x = x * 2"
        ]
      out = MoveTool.removeFromSourceExportList "double" body
  in pure $ "module Source (greet) where" `T.isInfixOf` out
         && not ("double" `T.isInfixOf` T.takeWhile (/= '\n') out)

-- | #228: addToDestinationExportList works on multi-line headers.
-- The multi-line header is collapsed to a single line after the rewrite.
testMoveAddDestExportMultiLine :: IO Bool
testMoveAddDestExportMultiLine =
  let body = T.unlines
        [ "module Dest"
        , "  ( foo"
        , "  , bar"
        , "  ) where"
        , ""
        , "foo = 1"
        , "bar = 2"
        ]
      out = MoveTool.addToDestinationExportList "double" body
  in pure $ "module Dest (foo, bar, double) where" `T.isInfixOf` out

-- | #236 fix: the correct sequence uses a fresh slice from the
-- export-stripped body, ensuring line numbers are consistent.
testMoveSequenceMultilineHeader236 :: IO Bool
testMoveSequenceMultilineHeader236 = pure $
  let body = T.unlines
        [ "module Src"
        , "  ( add"
        , "  , safeDiv"
        , "  , listSum"
        , "  ) where"
        , ""
        , "add :: Int -> Int -> Int"
        , "add x y = x + y"
        , ""
        , "safeDiv :: Int -> Int -> Maybe Int"
        , "safeDiv _ 0 = Nothing"
        , "safeDiv x y = Just x"
        , ""
        , "listSum :: [Int] -> Int"
        , "listSum = foldr add 0"
        ]
      stripped = MoveTool.removeFromSourceExportList "safeDiv" body
      -- Fix: re-slice from stripped body to get correct line numbers
      mSliced  = MoveTool.sliceTopLevelBinding "safeDiv" stripped
      result   = case mSliced of
                   Nothing -> stripped  -- symbol not found (would be a bug)
                   Just s  -> MoveTool.removeSliceFromBody s stripped
  in -- safeDiv should be gone
     not ("safeDiv" `T.isInfixOf` result)
     -- listSum should survive
  && "listSum :: [Int] -> Int" `T.isInfixOf` result
  && "listSum = foldr add 0"   `T.isInfixOf` result
     -- add should also survive
  && "add :: Int -> Int -> Int" `T.isInfixOf` result

-- | Issue #206: 'hasBareImportOf' detects a plain @import Foo@ line.
testHasBareImportOfDetects :: IO Bool
testHasBareImportOfDetects =
  let body = T.unlines
        [ "module Consumer where"
        , "import Data.Text"
        , "import HaskellFlows.Tool.Source"
        , "foo = 1"
        ]
  in pure (MoveTool.hasBareImportOf "HaskellFlows.Tool.Source" body)

-- | Issue #206: 'hasBareImportOf' detects @import qualified Foo@.
testHasBareImportOfQualified :: IO Bool
testHasBareImportOfQualified =
  let body = T.unlines
        [ "import qualified HaskellFlows.Tool.Source"
        ]
  in pure (MoveTool.hasBareImportOf "HaskellFlows.Tool.Source" body)

-- | Issue #206: 'hasBareImportOf' must NOT trigger on a selective
-- import @import Foo (sym)@ — the rewriter handles those already.
testHasBareImportOfSelectiveMiss :: IO Bool
testHasBareImportOfSelectiveMiss =
  let body = T.unlines
        [ "import HaskellFlows.Tool.Source (sym)"
        ]
  in pure (not (MoveTool.hasBareImportOf "HaskellFlows.Tool.Source" body))

-- | Issue #76: the slicer's biggest leak is mistaking the next
-- binding's '-- |' Haddock for a continuation of the current
-- binding. The fix treats column-0 '-- |' / '-- ^' as a slice
-- boundary; the slice for 'first' must end at line 5, before
-- 'second's Haddock starts.
testMoveSliceStopsAtHaddock :: IO Bool
testMoveSliceStopsAtHaddock =
  let body = T.unlines
        [ "-- | First."          -- 1
        , "first :: Int"         -- 2
        , "first = 1"            -- 3
        , ""                     -- 4
        , "-- | Second."         -- 5  ← boundary
        , "second :: Int"        -- 6
        , "second = 2"           -- 7
        ]
  in case MoveTool.sliceTopLevelBinding "first" body of
       Nothing -> pure False
       Just s  ->
         let sliced = MoveTool.srSliced s
         in pure $ T.isInfixOf "first :: Int" sliced
                && T.isInfixOf "first = 1"   sliced
                && not (T.isInfixOf "Second" sliced)
                && not (T.isInfixOf "second" sliced)
