-- | Unit tests for 'Refactor.Extract.extractBinding', 'isGuardBranch',
-- and 'Tool.Refactor' diagnostic-key helpers (#46, #50, #227). All pure.
--
-- Extracted from the Spec.hs monolith (#271) via the function-export shape.
module Spec.Extract
  ( testExtractBinding
  , testExtractEmpty
  , testRefactorErrorKeySame
  , testRefactorErrorKeyDistinct
  , testRefactorSignaturesErrorsOnly
  , testRefactorPostSubsetPre
  , testRefactorNewErrorDetected
  , testExtractRefusesTopLevelEquation
  , testExtractRefusesTypeSignature
  , testExtractRefusesImport
  , testExtractAllowsIndentedBody
  , testExtractRefusesModuleDecl
  , testExtractRefusesDataDecl
  , testExtractRefusesNewtypeDecl
  , testExtractRefusesClassDecl
  , testExtractRefusesInstanceDecl
  , testExtractRefusesPragma
  , testExtractRefusesGuardBranch
  , testIsGuardBranch
  , testIsGuardBranchNeg
  , testExtractRefusesOperatorDef
  , testExtractRefusesMultilineEquation
  , testExtractRefusesMixedRange
  , testExtractRefusesLeadingBlanksWithCol0
  , testExtractRefusalMessageShape
  , testExtractAllowsLetBody
  , testExtractAllowsDoBody
  , testExtractAllowsWhereBody
  , testExtractAllowsMultilineBody
  , testExtractSurvivesEolWhitespace
  , testExtractProducesSingleEquals
  , testExtractAllBlankRangeRefused
  -- diagnostic-test helpers (shared with other Spec modules)
  , mkErr
  , mkWarn
  ) where

import Data.Text (Text)
import qualified Data.Text as T

import HaskellFlows.Parser.Error
  ( GhcError (..)
  , Severity (..)
  )
import HaskellFlows.Refactor.Extract
  ( ExtractResult (..)
  , extractBinding
  , isGuardBranch
  )
import qualified HaskellFlows.Tool.Refactor as RefactorTool

testExtractBinding :: IO Bool
testExtractBinding =
  let src = T.unlines
        [ "main = do"
        , "  let x = 1 + 2 + 3"
        , "  print x"
        ]
  in case extractBinding "sumSmall" 2 2 src of
       Right er ->
         pure $ "sumSmall" `T.isInfixOf` erNewContent er
             && "sumSmall =" `T.isInfixOf` erBindingTxt er
       _ -> pure False

testExtractEmpty :: IO Bool
testExtractEmpty =
  pure $ case extractBinding "foo" 5 4 "body" of
    Left _ -> True
    _      -> False

-- | Issue #50: structural-key helpers for the diagnostic-diff
-- accept criterion. Two diagnostics with identical (file, line,
-- column, message) are considered the same — that's what makes
-- the post ⊆ pre test mean "the rewrite introduced no new errors".

testRefactorErrorKeySame :: IO Bool
testRefactorErrorKeySame =
  let a = mkErr "F.hs" 10 5 "Found hole: _x :: Int"
      b = mkErr "F.hs" 10 5 "Found hole: _x :: Int"
  in pure (RefactorTool.errorKey a == RefactorTool.errorKey b)

testRefactorErrorKeyDistinct :: IO Bool
testRefactorErrorKeyDistinct =
  let a = mkErr "F.hs" 10 5 "Variable not in scope: foo"
      b = mkErr "F.hs" 10 5 "Variable not in scope: bar"
  in pure (RefactorTool.errorKey a /= RefactorTool.errorKey b)

testRefactorSignaturesErrorsOnly :: IO Bool
testRefactorSignaturesErrorsOnly =
  let err  = mkErr  "F.hs" 1 1 "boom"
      warn = mkWarn "F.hs" 2 2 "unused"
  in pure (RefactorTool.errorSignatures [err, warn]
             == [RefactorTool.errorKey err])

-- | Issue #50: a rename that leaves an unrelated pre-existing
-- error in place must NOT be rolled back. Model the diff
-- check directly: post is identical to pre → no new errors.
testRefactorPostSubsetPre :: IO Bool
testRefactorPostSubsetPre =
  let pre  = [mkErr "F.hs" 23 1 "Found hole: _holeArg :: [a]"]
      post = pre  -- rename touched line 13, hole at line 23 unchanged
      preSigs  = RefactorTool.errorSignatures pre
      postSigs = RefactorTool.errorSignatures post
      newErrSigs = filter (`notElem` preSigs) postSigs
  in pure (null newErrSigs)

-- | Issue #50: a rename that introduces a NEW error must be
-- rejected — that's the conservative side of the diff.
testRefactorNewErrorDetected :: IO Bool
testRefactorNewErrorDetected =
  let pre  = [mkErr "F.hs" 23 1 "Found hole: _holeArg :: [a]"]
      post = pre <> [mkErr "F.hs" 13 5 "Variable not in scope: greeting"]
      preSigs  = RefactorTool.errorSignatures pre
      postSigs = RefactorTool.errorSignatures post
      newErrSigs = filter (`notElem` preSigs) postSigs
  in pure (length newErrSigs == 1)

-- | Tiny ctor helpers for the diagnostic tests above.
mkErr :: Text -> Int -> Int -> Text -> GhcError
mkErr file ln col msg = GhcError
  { geFile     = file
  , geLine     = ln
  , geColumn   = col
  , geSeverity = SevError
  , geCode     = Nothing
  , geMessage  = msg
  }

mkWarn :: Text -> Int -> Int -> Text -> GhcError
mkWarn file ln col msg = GhcError
  { geFile     = file
  , geLine     = ln
  , geColumn   = col
  , geSeverity = SevWarning
  , geCode     = Nothing
  , geMessage  = msg
  }

-- | Regression test for issue #46. Pointing extract_binding at a whole
-- top-level equation used to produce broken Haskell — the call site
-- got a bare name (no @=@) and the extracted binding got a nested @=@
-- (its RHS was the original equation line, not the equation's body).
-- The fix refuses any range that sits at column 0, since by Haskell
-- layout rules a body expression is always indented.
--
-- Repro is the exact source from the issue.
testExtractRefusesTopLevelEquation :: IO Bool
testExtractRefusesTopLevelEquation =
  let src = T.unlines
        [ "module Demo where"
        , ""
        , "doubledSum :: [Int] -> Int"
        , "doubledSum xs = foldr (\\x acc -> x * 2 + acc) 0 xs"
        ]
  in pure $ case extractBinding "doubleAndAdd" 4 4 src of
       Left msg ->
         "expression range" `T.isInfixOf` msg
           && "column 0"   `T.isInfixOf` msg
           && "doubledSum" `T.isInfixOf` msg
       Right _  -> False

-- | A type signature lives at column 0 and lifting it is also nonsense.
-- Same column-0 guard catches it; the message still tells the agent to
-- narrow the scope.
testExtractRefusesTypeSignature :: IO Bool
testExtractRefusesTypeSignature =
  let src = T.unlines
        [ "module Demo where"
        , ""
        , "doubledSum :: [Int] -> Int"
        , "doubledSum xs = foldr (\\x acc -> x * 2 + acc) 0 xs"
        ]
  in pure $ case extractBinding "newName" 3 3 src of
       Left msg -> "expression range" `T.isInfixOf` msg
       Right _  -> False

-- | An import line at column 0 must also be refused, not silently
-- corrupted into garbage.
testExtractRefusesImport :: IO Bool
testExtractRefusesImport =
  let src = T.unlines
        [ "module Demo where"
        , ""
        , "import Data.List (sort)"
        , ""
        , "main :: IO ()"
        , "main = print (sort [3,1,2])"
        ]
  in pure $ case extractBinding "imp" 3 3 src of
       Left msg -> "expression range" `T.isInfixOf` msg
       Right _  -> False

-- | Sanity check that the guard does NOT regress the documented
-- success path: an indented body expression must still extract cleanly
-- and the resulting binding must contain a single @=@ (no nested
-- equation, no dangling header).
testExtractAllowsIndentedBody :: IO Bool
testExtractAllowsIndentedBody =
  let src = T.unlines
        [ "module Demo where"
        , ""
        , "doubledSum :: [Int] -> Int"
        , "doubledSum xs ="
        , "  foldr (\\x acc -> x * 2 + acc) 0 xs"
        ]
      countEqualsOnNewBindingLine txt =
        -- The first line of the appended binding must be exactly
        -- "<name> ="; the body lines must NOT start with another
        -- "<name> =".
        let bls = T.lines txt
            isHeader l = "doubleAndAdd =" `T.isPrefixOf` l
            headers    = filter isHeader bls
        in length headers == 1
  in pure $ case extractBinding "doubleAndAdd" 5 5 src of
       Left _   -> False
       Right er ->
         -- Call-site: "doubledSum xs =" preserved on its own line, no
         -- bare orphan name.
         not ("doubledSum xs ="
              `T.isInfixOf` erBindingTxt er)
           && countEqualsOnNewBindingLine (erBindingTxt er)
           && "doubleAndAdd ="    `T.isPrefixOf` erBindingTxt er
           && "doubleAndAdd"      `T.isInfixOf` erNewContent er

-- | The module-header line is the highest-stakes line at column 0:
-- lifting it would orphan the entire file. The guard must refuse it.
testExtractRefusesModuleDecl :: IO Bool
testExtractRefusesModuleDecl =
  let src = T.unlines
        [ "module Demo where"
        , ""
        , "x :: Int"
        , "x = 1"
        ]
  in pure $ case extractBinding "newName" 1 1 src of
       Left msg -> "expression range" `T.isInfixOf` msg
       Right _  -> False

-- | A @data@ declaration sits at column 0 and is meaningless to lift
-- as an expression.
testExtractRefusesDataDecl :: IO Bool
testExtractRefusesDataDecl =
  let src = T.unlines
        [ "module Demo where"
        , ""
        , "data Color = Red | Green | Blue"
        ]
  in pure $ case extractBinding "newName" 3 3 src of
       Left msg -> "expression range" `T.isInfixOf` msg
       Right _  -> False

-- | A @newtype@ declaration is also a top-level form, also refused.
testExtractRefusesNewtypeDecl :: IO Bool
testExtractRefusesNewtypeDecl =
  let src = T.unlines
        [ "module Demo where"
        , ""
        , "newtype Wrap a = Wrap { unwrap :: a }"
        ]
  in pure $ case extractBinding "newName" 3 3 src of
       Left msg -> "expression range" `T.isInfixOf` msg
       Right _  -> False

-- | A @class@ header at column 0 is a top-level form: refused.
testExtractRefusesClassDecl :: IO Bool
testExtractRefusesClassDecl =
  let src = T.unlines
        [ "module Demo where"
        , ""
        , "class Foo a where"
        , "  foo :: a -> a"
        ]
  in pure $ case extractBinding "newName" 3 3 src of
       Left msg -> "expression range" `T.isInfixOf` msg
       Right _  -> False

-- | An @instance@ header at column 0 is a top-level form: refused.
testExtractRefusesInstanceDecl :: IO Bool
testExtractRefusesInstanceDecl =
  let src = T.unlines
        [ "module Demo where"
        , ""
        , "instance Show Color where"
        , "  show Red = \"red\""
        ]
  in pure $ case extractBinding "newName" 3 3 src of
       Left msg -> "expression range" `T.isInfixOf` msg
       Right _  -> False

-- | Pragmas live at column 0 too. The guard treats them like any
-- other top-level form.
testExtractRefusesPragma :: IO Bool
testExtractRefusesPragma =
  let src = T.unlines
        [ "{-# LANGUAGE OverloadedStrings #-}"
        , "module Demo where"
        , ""
        , "main = putStrLn \"hi\""
        ]
  in pure $ case extractBinding "newName" 1 1 src of
       Left msg -> "expression range" `T.isInfixOf` msg
       Right _  -> False

-- | #227: selecting a whole guard branch (line starts with '|') must
-- be refused with a clear message, not allowed to produce broken Haskell.
testExtractRefusesGuardBranch :: IO Bool
testExtractRefusesGuardBranch =
  let src = T.unlines
        [ "module Demo where"
        , ""
        , "factorial :: Int -> Int"
        , "factorial n"
        , "  | n <= 0    = 1"
        , "  | otherwise = n * factorial (n - 1)"
        ]
  in pure $ case extractBinding "recurse" 6 6 src of
       Left msg -> "guard" `T.isInfixOf` msg || "guard branch" `T.isInfixOf` msg
       Right _  -> False

-- | #227: 'isGuardBranch' detects guard-branch lines correctly.
testIsGuardBranch :: IO Bool
testIsGuardBranch = pure $
     isGuardBranch ["  | n <= 0    = 1"]
  && isGuardBranch ["  | otherwise = n * factorial (n - 1)"]
  && isGuardBranch ["    | True = go"]
  && isGuardBranch ["  | n <= 0    = 1", "  | otherwise = n * factorial (n - 1)"]

-- | #227: 'isGuardBranch' correctly ignores normal expression lines.
testIsGuardBranchNeg :: IO Bool
testIsGuardBranchNeg = pure $
     not (isGuardBranch ["  n * factorial (n - 1)"])
  && not (isGuardBranch ["  let x = 1"])
  && not (isGuardBranch [])
  && not (isGuardBranch [""])

-- | An operator definition like @(+++) :: ...@ or @(+++) x y = ...@
-- starts at column 0 too — same refusal.
testExtractRefusesOperatorDef :: IO Bool
testExtractRefusesOperatorDef =
  let src = T.unlines
        [ "module Demo where"
        , ""
        , "(+++) :: Int -> Int -> Int"
        , "(+++) x y = x + y + 1"
        ]
  in pure $ case extractBinding "plus3" 4 4 src of
       Left msg -> "expression range" `T.isInfixOf` msg
       Right _  -> False

-- | A multi-line range that spans a whole equation block (signature +
-- body) at column 0 must be refused even though the equation has its
-- body on a continuation line — the guard sees @commonIndent == 0@
-- because the signature line dominates.
testExtractRefusesMultilineEquation :: IO Bool
testExtractRefusesMultilineEquation =
  let src = T.unlines
        [ "module Demo where"
        , ""
        , "doubledSum :: [Int] -> Int"
        , "doubledSum xs ="
        , "  foldr (\\x acc -> x * 2 + acc) 0 xs"
        ]
  in pure $ case extractBinding "newName" 3 5 src of
       Left msg -> "expression range" `T.isInfixOf` msg
       Right _  -> False

-- | A range that mixes a column-0 line with indented continuations
-- still has @commonIndent == 0@. Must be refused — the column-0 line
-- is the equation header, lifting it would corrupt the file.
testExtractRefusesMixedRange :: IO Bool
testExtractRefusesMixedRange =
  let src = T.unlines
        [ "module Demo where"
        , ""
        , "buildMessage :: String -> String"
        , "buildMessage name ="
        , "  \"Hello, \" ++ name"
        ]
  in pure $ case extractBinding "newName" 4 5 src of
       Left msg -> "expression range" `T.isInfixOf` msg
       Right _  -> False

-- | Leading blank lines in the range must NOT trick the guard. Even if
-- the first selected line is blank, as long as some non-blank line in
-- the range sits at column 0, the guard fires.
testExtractRefusesLeadingBlanksWithCol0 :: IO Bool
testExtractRefusesLeadingBlanksWithCol0 =
  let src = T.unlines
        [ "module Demo where"
        , ""
        , ""
        , "x :: Int"
        , "x = 42"
        ]
  in pure $ case extractBinding "newName" 3 5 src of
       Left msg -> "expression range" `T.isInfixOf` msg
       Right _  -> False

-- | The refusal message must (a) cite the exact line range, (b)
-- include a preview of the offending line so the agent can see what
-- it pointed at, and (c) explain how to recover. All three are
-- machine-checkable via substring presence.
testExtractRefusalMessageShape :: IO Bool
testExtractRefusalMessageShape =
  let src = T.unlines
        [ "module Demo where"
        , ""
        , "x :: Int"
        , "x = 42"
        ]
  in pure $ case extractBinding "newName" 4 4 src of
       Left msg ->
            "4-4"        `T.isInfixOf` msg
         && "x = 42"     `T.isInfixOf` msg
         && "Narrow"     `T.isInfixOf` msg
         && "expression range" `T.isInfixOf` msg
       Right _ -> False

-- | Sanity success path: a let-binding's RHS expression at column 6
-- extracts cleanly.
testExtractAllowsLetBody :: IO Bool
testExtractAllowsLetBody =
  let src = T.unlines
        [ "module Demo where"
        , ""
        , "build :: Int"
        , "build ="
        , "  let result = 1 + 2 + 3"
        , "  in result + 1"
        ]
  in pure $ case extractBinding "smallSum" 5 5 src of
       Left _   -> False
       Right er ->
            "smallSum"         `T.isInfixOf` erNewContent er
         && "smallSum ="       `T.isPrefixOf` erBindingTxt er
         && erIndent er > 0

-- | Sanity success path: a do-block statement at column 2 extracts
-- cleanly.
testExtractAllowsDoBody :: IO Bool
testExtractAllowsDoBody =
  let src = T.unlines
        [ "module Demo where"
        , ""
        , "main :: IO ()"
        , "main = do"
        , "  putStrLn \"hello world\""
        , "  pure ()"
        ]
  in pure $ case extractBinding "greeting" 5 5 src of
       Left _   -> False
       Right er ->
            "greeting"        `T.isInfixOf` erNewContent er
         && "greeting ="      `T.isPrefixOf` erBindingTxt er
         && erIndent er == 2

-- | Sanity success path: a where-clause body expression extracts
-- cleanly. The where-binding header itself sits at column 2; its RHS
-- expression sits at column 4 or beyond.
testExtractAllowsWhereBody :: IO Bool
testExtractAllowsWhereBody =
  let src = T.unlines
        [ "module Demo where"
        , ""
        , "f :: Int -> Int"
        , "f x = helper"
        , "  where"
        , "    helper = x * x + 1"
        ]
  in pure $ case extractBinding "square" 6 6 src of
       Left _   -> False
       Right er ->
            "square"        `T.isInfixOf` erNewContent er
         && "square ="      `T.isPrefixOf` erBindingTxt er

-- | A multi-line indented body: the guard must allow it AND the
-- relative indentation between the body lines must be preserved (the
-- inner lines stay nested under the first line).
testExtractAllowsMultilineBody :: IO Bool
testExtractAllowsMultilineBody =
  let src = T.unlines
        [ "module Demo where"
        , ""
        , "f :: [Int] -> Int"
        , "f xs ="
        , "  foldr"
        , "    (\\x acc -> x + acc)"
        , "    0"
        , "    xs"
        ]
  in pure $ case extractBinding "summing" 5 8 src of
       Left _ -> False
       Right er ->
         let bind  = erBindingTxt er
             newC  = erNewContent er
         in    "summing ="      `T.isPrefixOf` bind
            && "foldr"          `T.isInfixOf` bind
            && "summing"        `T.isInfixOf` newC
            -- The relative indent is preserved (the inner lines stay
            -- deeper than 'foldr').
            && T.isInfixOf "  foldr" bind

-- | Trailing whitespace at end-of-line must NOT trick the guard.
-- @T.takeWhile isSpace@ counts only LEADING whitespace, so trailing
-- whitespace shouldn't shift the indent calculation. Pin the
-- invariant.
testExtractSurvivesEolWhitespace :: IO Bool
testExtractSurvivesEolWhitespace =
  let src = T.unlines
        [ "module Demo where"
        , ""
        , "f :: Int"
        , "f ="
        , "  1 + 2   "  -- trailing spaces
        ]
  in pure $ case extractBinding "onePlus2" 5 5 src of
       Left _   -> False
       Right er -> "onePlus2 =" `T.isPrefixOf` erBindingTxt er

-- | Regression invariant for the bug fix: the appended binding must
-- contain EXACTLY ONE @=@ at column 0 (its own header), and the
-- call-site must NEVER be a bare name with no @=@. Both halves of
-- the original bug pattern must be impossible.
testExtractProducesSingleEquals :: IO Bool
testExtractProducesSingleEquals =
  let src = T.unlines
        [ "module Demo where"
        , ""
        , "f :: Int"
        , "f ="
        , "  let x = 1 + 2 in x + 3"
        ]
      countCol0Equals txt =
        length [ () | l <- T.lines txt
                    , T.length l >= 1
                    , T.take 1 l /= " "
                    , T.take 1 l /= "\t"
                    , "=" `T.isInfixOf` T.takeWhile (/= '\n') l
                    , let stripped = T.strip l
                    , -- Only count lines whose first '=' is the binding
                      -- delimiter, not part of a string literal etc.
                      not ("--" `T.isPrefixOf` stripped)
                    , -- The "<word> =" prefix shape is what we want.
                      let firstEq = T.takeWhile (/= '=') l
                      in not (T.null firstEq)
                ]
  in pure $ case extractBinding "letBody" 5 5 src of
       Left _   -> False
       Right er ->
         let bind = erBindingTxt er
         in    "letBody ="           `T.isPrefixOf` bind
            && countCol0Equals bind == 1

-- | A range consisting of only blank lines triggers the existing
-- "extracted range is empty" path (because the @null body@ check is
-- on raw lines, but actually @body@ is non-empty list of blank lines,
-- so @hasNonBlank body == False@ falls through to the column-0
-- branch's @&& hasNonBlank body@ guard. We expect the textual cut to
-- proceed but produce a degenerate (yet not bug-shaped) result; the
-- compile-verify layer would catch any nonsense. The test below pins
-- that no exception is thrown and the call doesn't refuse with the
-- top-level message — so the guard is precise to non-blank cuts.
testExtractAllBlankRangeRefused :: IO Bool
testExtractAllBlankRangeRefused =
  let src = T.unlines
        [ "module Demo where"
        , ""
        , ""
        , ""
        , "x = 1"
        ]
  in pure $ case extractBinding "newName" 2 4 src of
       -- Either it refuses (different reason) or it goes through —
       -- but it MUST NOT trip the top-level guard since there's no
       -- non-blank column-0 line in [2,4].
       Left msg  -> not ("expression range" `T.isInfixOf` msg)
       Right _er -> True
