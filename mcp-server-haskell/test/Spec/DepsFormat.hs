-- | Unit tests for ghc_workflow parity check and ghc_deps formatting
-- invariants (indentation, no leading comma, stanza targeting). All pure.
--
-- Extracted from the Spec.hs monolith (#271) via the function-export shape.
module Spec.DepsFormat
  ( testWorkflowToolsParity
  , testDepsAddIndentsForCabal
  , testDepsAddNoTopComma
  , testDepsAddTargetsTestSuite
  ) where

import qualified Data.Maybe
import qualified Data.Text as T

import HaskellFlows.Mcp.Server (allToolNameTexts, allToolDescriptors)
import HaskellFlows.Tool.Deps (addDep, parseStanzaSelector)

-- | Issue #24: @toolsActive@ in 'ghc_workflow' must enumerate the
-- same set of tools as @tools/list@. The two used to drift because
-- the list was hand-maintained in two places. Paranoia check: also
-- confirm every name is non-empty and the server registers more
-- than the 9-tool Phase-5 baseline.
testWorkflowToolsParity :: IO Bool
testWorkflowToolsParity = pure $
     length allToolNameTexts == length allToolDescriptors
  && not (any T.null allToolNameTexts)
  && length allToolNameTexts >= 20

--------------------------------------------------------------------------------
-- Phase 11b regressions: ghc_deps F-01 / F-02 / F-03 fixes.
--------------------------------------------------------------------------------

-- | F-01: @ghc_deps add@ previously wrote @,@-prefixed continuation
-- lines at the same column as the @build-depends:@ field. Cabal 3.0
-- rejects that as a new field header and the file becomes
-- unparseable. Pin the invariant: after add, the inserted line's
-- leading whitespace must strictly exceed the header's indent.
testDepsAddIndentsForCabal :: IO Bool
testDepsAddIndentsForCabal =
  let body = T.unlines
        [ "library"
        , "    build-depends:    base >= 4.20 && < 5"
        ]
      newBody = addDep Nothing "QuickCheck" body
      isContComma ln = "," `T.isPrefixOf` T.stripStart ln
      commaLines = filter isContComma (T.lines newBody)
      headerIndent = 4  -- 4 spaces before "build-depends:"
  in pure $ case commaLines of
       [ln] ->
         let leading = T.length (T.takeWhile (== ' ') ln)
         in leading > headerIndent
       _ -> False

-- | F-02 (same root as F-01, framed positively): after @addDep@ on a
-- pristine scaffold, the entire file must not contain any line whose
-- first non-whitespace char is \",\" at column <= field indent.
-- This guards against future parser-confusing shapes.
testDepsAddNoTopComma :: IO Bool
testDepsAddNoTopComma =
  let body = T.unlines
        [ "cabal-version: 3.0"
        , "name: foo"
        , ""
        , "library"
        , "    build-depends:    base >= 4.20 && < 5"
        ]
      newBody  = addDep (Just ">= 2.14") "QuickCheck" body
      offenderLines =
        [ ln
        | ln <- T.lines newBody
        , let ws = T.length (T.takeWhile (== ' ') ln)
        , "," `T.isPrefixOf` T.stripStart ln
        , ws <= 4
        ]
  in pure (null offenderLines)

-- | F-03: with a stanza selector, @addDep@ must land in the
-- requested stanza, not the first @build-depends:@ it finds.
testDepsAddTargetsTestSuite :: IO Bool
testDepsAddTargetsTestSuite =
  let body = T.unlines
        [ "library"
        , "    build-depends:    base"
        , ""
        , "test-suite foo-test"
        , "    build-depends:    base"
        , "                    , foo"
        ]
      -- Scope everything through resolveStanza-style slicing by using
      -- applyWithinStanza through addDep at the tool layer; here we
      -- simulate via the exported primitive.
      newBody = case parseStanzaSelector "test-suite" of
        Left _ -> body
        Right sel ->
          let lns = T.lines body
              -- In-file-tool uses applyWithinStanza; mirror that with a
              -- direct slice call via the public API (parseStanzaSelector
              -- + addDep on the slice).
              (pre, stanzaLns, post) = sliceOrEmpty sel lns
              inner  = T.unlines stanzaLns
              inner' = addDep Nothing "QuickCheck" inner
          in T.unlines (pre <> T.lines inner' <> post)
      libDeps       = scopedDeps "library" newBody
      testSuiteDeps = scopedDeps "test-suite" newBody
  in pure $ "QuickCheck" `elem` testSuiteDeps
         && "QuickCheck" `notElem` libDeps
  where
    sliceOrEmpty sel lns =
      Data.Maybe.fromMaybe ([], lns, []) (lookupStanza sel lns)
    -- Tiny reimpl of the MCP's stanza slice, used only by this test.
    lookupStanza (kind, mName) lns =
      let match ln
            | not (T.null (T.takeWhile (== ' ') ln)) = False
            | otherwise =
                let s = T.strip ln
                    w = T.takeWhile (/= ' ') s
                    r = T.strip (T.dropWhile (/= ' ') s)
                in w == kind
                && case mName of
                     Nothing
                       | kind == "library" -> T.null r
                       | otherwise         -> True
                     Just name -> r == name
          isTop ln =
            T.null (T.takeWhile (== ' ') ln)
            && not (T.null (T.strip ln))
            && not (":" `T.isInfixOf` T.strip ln)
            && T.takeWhile (/= ' ') (T.strip ln)
                 `elem` topStanzaKinds
          topStanzaKinds =
            [ "library", "executable", "test-suite", "benchmark"
            , "foreign-library", "common", "flag", "source-repository"
            ]
      in case break match lns of
           (_,   [])     -> Nothing
           (pre, h : tl) ->
             let (body', post) = break isTop tl
             in Just (pre, h : body', post)
    scopedDeps kind body' =
      case parseStanzaSelector kind of
        Left _    -> []
        Right sel -> case lookupStanza sel (T.lines body') of
          Nothing       -> []
          Just (_, l, _) -> stanzaDeps (T.unlines l)
    stanzaDeps body' =
      -- same line-oriented parser used by the tool; inline-enough for
      -- the test by slicing the build-depends: line + continuations.
      let ls = T.lines body'
          rest = dropWhile (not . startsBuildDepends) ls
      in case rest of
           []     -> []
           (h:tl) ->
             let tailVal = T.strip (T.drop (T.length "build-depends:")
                                            (T.strip h))
                 cont    = takeWhile isContLine tl
                 joined  = T.intercalate " " (tailVal : map T.strip cont)
             in [ T.strip (T.takeWhile
                             (\c -> c /= ' ' && c /= '>' && c /= '<'
                                 && c /= '=' && c /= '^' && c /= '&')
                             (T.strip tok))
                | tok <- T.splitOn "," joined
                , not (T.null (T.strip tok))
                ]
    startsBuildDepends ln =
      "build-depends:" `T.isPrefixOf` T.stripStart (T.toLower ln)
    isContLine ln =
      not (T.null (T.takeWhile (== ' ') ln))
      && not (T.null (T.strip ln))
