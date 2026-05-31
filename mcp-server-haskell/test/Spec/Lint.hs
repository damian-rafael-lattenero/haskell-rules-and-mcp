-- | Unit tests for @ghc_lint@ path resolution — the #81 (CWE-22) traversal
-- guard in 'resolveTarget' and the #128 'stripProjectDirPrefix'
-- no-double-basename fix.
--
-- Extracted from the Spec.hs monolith (#271) via the function-export shape:
-- the driver keeps the registrations and imports these functions. These tests
-- exercise only pure 'LintTool' helpers, so they carry no shared-fixture deps.
module Spec.Lint
  ( testLintResolveRejectsTraversal
  , testLintResolveRejectsAbsoluteOutside
  , testLintResolveAcceptsInTree
  , testStripProjectDirPrefixNoOp
  , testStripProjectDirPrefixStrips
  , testStripProjectDirPrefixAbsolute
  , testLintResolveNoDuplication
  ) where

import qualified Data.List as List
import qualified Data.Text as T

import qualified HaskellFlows.Tool.Lint as LintTool
import HaskellFlows.Types (mkProjectDir)

-- | Issue #81 (CWE-22): the previous 'resolveTarget' compared the
-- joined path against the project root with a literal string-prefix
-- check, which the agent could trick by passing a relative path
-- that contained '..' segments. The fix mirrors 'mkModulePath' and
-- rejects anything whose normalised segments contain '..' before
-- it can reach hlint. Each entry below is a path the old gate
-- accepted but the new one must refuse.
testLintResolveRejectsTraversal :: IO Bool
testLintResolveRejectsTraversal =
  case mkProjectDir "/tmp/project" of
    Left _   -> pure False
    Right pd ->
      let escapes = [ "../.."
                    , "../../something"
                    , "./../foo"
                    , "a/../../escape"
                    ]
          rejectedAsPath p = case LintTool.resolveTarget pd
                                  (LintTool.LintArgs (Just (T.pack p)) Nothing "warning") of
            Left err -> "escapes project directory" `T.isInfixOf` err
            Right _  -> False
          rejectedAsModulePath p = case LintTool.resolveTarget pd
                                  (LintTool.LintArgs Nothing (Just (T.pack p)) "warning") of
            Left err -> "escapes project directory" `T.isInfixOf` err
            Right _  -> False
      in pure $ all rejectedAsPath escapes && all rejectedAsModulePath escapes

-- | Companion: absolute paths outside the project root must also be
-- rejected. (The old gate already caught these — this anchors the
-- regression so a future refactor doesn't trade one bypass for
-- another.)
testLintResolveRejectsAbsoluteOutside :: IO Bool
testLintResolveRejectsAbsoluteOutside =
  case mkProjectDir "/tmp/project" of
    Left _   -> pure False
    Right pd ->
      let outside = LintTool.resolveTarget pd
                       (LintTool.LintArgs (Just "/tmp") Nothing "warning")
      in pure $ case outside of
           Left err -> "escapes project directory" `T.isInfixOf` err
           Right _  -> False

-- | Companion: legitimate relative paths inside the project must
-- still resolve. Both 'path' (directory) and 'module_path' (file)
-- forms are exercised; the empty-args default (root itself) is the
-- third anchor.
testLintResolveAcceptsInTree :: IO Bool
testLintResolveAcceptsInTree =
  case mkProjectDir "/tmp/project" of
    Left _   -> pure False
    Right pd ->
      let asPath       = LintTool.resolveTarget pd
                           (LintTool.LintArgs (Just "src/") Nothing "warning")
          asModulePath = LintTool.resolveTarget pd
                           (LintTool.LintArgs Nothing (Just "src/Foo.hs") "warning")
          asEmpty      = LintTool.resolveTarget pd
                           (LintTool.LintArgs Nothing Nothing "warning")
          isInTree (Right p) = "/tmp/project" `List.isPrefixOf` p
          isInTree _         = False
      in pure $ isInTree asPath && isInTree asModulePath && isInTree asEmpty

-- #128: stripProjectDirPrefix must not change a path that doesn't start
-- with the project basename.
testStripProjectDirPrefixNoOp :: IO Bool
testStripProjectDirPrefixNoOp =
  let root   = "/home/user/my-project"
      result = LintTool.stripProjectDirPrefix root "src/"
  in pure (result == "src/")

-- #128: when the raw path starts with the project's own basename,
-- strip that segment so path-joining doesn't double it.
testStripProjectDirPrefixStrips :: IO Bool
testStripProjectDirPrefixStrips =
  let root   = "/home/user/my-project"
      -- "my-project/" is the basename of root — stripping leaves ""
      result = LintTool.stripProjectDirPrefix root "my-project/"
      -- also test "my-project/src" → "src"
      result2 = LintTool.stripProjectDirPrefix root "my-project/src"
  in pure (null result && result2 == "src")

-- #128: absolute paths must be returned unchanged regardless of basename.
testStripProjectDirPrefixAbsolute :: IO Bool
testStripProjectDirPrefixAbsolute =
  let root   = "/home/user/my-project"
      absP   = "/home/user/my-project/src"
      result = LintTool.stripProjectDirPrefix root absP
  in pure (result == absP)

-- #128: resolveTarget with path="<basename>/" must resolve to projectDir,
-- not projectDir/<basename>. This is the exact dogfood repro case.
testLintResolveNoDuplication :: IO Bool
testLintResolveNoDuplication =
  case mkProjectDir "/tmp/my-project" of
    Left _   -> pure False
    Right pd ->
      -- Pass the project's own basename as the lint path — the old code
      -- would produce /tmp/my-project/my-project (doubled, doesn't exist).
      let args   = LintTool.LintArgs (Just "my-project") Nothing "warning"
          result = LintTool.resolveTarget pd args
      in pure $ case result of
           Right resolved -> resolved == "/tmp/my-project"
           Left _         -> False
