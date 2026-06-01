-- | Unit tests for CheckProject exposed-module parsing, Info tool AId/
-- TyCon/AConLike branches, TypeSignature stripForall/stripLineComments,
-- and Suggest forall-reverse rule. Mix of pure and GhcSession tests.
--
-- Extracted from the Spec.hs monolith (#271) via the function-export shape.
module Spec.InfoAdvanced
  ( testParseExposedModulesStripsComments
  , testParseExposedModulesRejectsPunct
  , testInfoAnIdDefinition
  , testInfoPreferTyConInSource
  , testInfoQueryUsesPreferTyCon
  , testInfoAConLikeBranchExists
  , testInfoAConLikeUsesDisplayType
  , testStripForallInferred
  , testStripForallExplicit
  , testStripForallNoop
  , testParseSigForallList
  , testRulesFireForForallReverse
  , testStripLineCommentsHaddock
  , testStripLineCommentsMid
  , testStripLineCommentsClean
  , testParseSigHaddockAnnotated
  ) where

import qualified Data.Text as T
import qualified Data.Text.IO as TIO

import HaskellFlows.Ghc.ApiSession (startGhcSession, killGhcSession)
import HaskellFlows.Parser.TypeSignature
  ( ParsedSig (..)
  , SigType (..)
  , parseSignature
  , isSameTypeThroughout
  , stripForall
  , stripLineComments
  )
import HaskellFlows.Suggest.Rules (applyRules, Suggestion (..))
import HaskellFlows.Tool.CheckProject (parseExposedModules)
import qualified HaskellFlows.Tool.Info as InfoTool
import HaskellFlows.Types (mkProjectDir)

import Spec.Helpers (withTempProject)

--------------------------------------------------------------------------------
-- Issue #109 — .cabal comment stripping in check_project
--------------------------------------------------------------------------------

-- | #109: 'parseExposedModules' must not extract comment words like
-- "Bench", "Phase", "A)" that follow a @--@ marker in the payload.
testParseExposedModulesStripsComments :: IO Bool
testParseExposedModulesStripsComments =
  let body = T.unlines
        [ "library"
        , "  exposed-modules:"
        , "    Core.Logic"
        , "    -- Bench modules (#96 Phase A)"
        , "    Core.Parser"
        ]
      mods = parseExposedModules body
  in pure $ "Core.Logic"   `elem` mods
         && "Core.Parser"  `elem` mods
         && "Bench"    `notElem` mods
         && "Phase"    `notElem` mods
         && "A)"       `notElem` mods

-- | #109: tokens with non-module characters (e.g. trailing ')') must
-- be rejected by the strengthened 'isModuleName' predicate.
testParseExposedModulesRejectsPunct :: IO Bool
testParseExposedModulesRejectsPunct =
  let body = T.unlines
        [ "library"
        , "  exposed-modules: Good.Module, Bad)"
        ]
      mods = parseExposedModules body
  in pure $ "Good.Module" `elem`    mods
         && "Bad)"        `notElem` mods
         && "Bad"         `notElem` mods

--------------------------------------------------------------------------------
-- Issue #107 — ghc_info renderDefinition for functions
--------------------------------------------------------------------------------

-- | #107: the 'AnId' branch in 'queryInfo' must use 'idType' to produce
-- "name :: <type>" instead of "Identifier 'name'" (pprShortTyThing).
-- Checked structurally by scanning Info.hs source.
testInfoAnIdDefinition :: IO Bool
testInfoAnIdDefinition = do
  src <- TIO.readFile "src/HaskellFlows/Tool/Info.hs"
  let code = T.unlines (filter (not . isDocLine) (T.lines src))
  pure $ T.isInfixOf "AnId i ->" code
      && T.isInfixOf "idType i" code
      && T.isInfixOf "nm <> \" :: \"" code
  where
    isDocLine ln = "--" `T.isPrefixOf` T.stripStart ln

--------------------------------------------------------------------------------
-- Issue #130 — ghc_info eponymous record TyCon selection
--------------------------------------------------------------------------------

-- | #130: the Info.hs source must define 'preferTyCon' with the
-- ATyCon-preference logic.
testInfoPreferTyConInSource :: IO Bool
testInfoPreferTyConInSource = do
  src <- TIO.readFile "src/HaskellFlows/Tool/Info.hs"
  pure $ T.isInfixOf "preferTyCon" src
      && T.isInfixOf "ATyCon {}" src
      && T.isInfixOf "toList names" src

-- | #130: 'queryInfo' must use 'preferTyCon' and NOT take only the
-- first name via the old @n :| _@ pattern.
testInfoQueryUsesPreferTyCon :: IO Bool
testInfoQueryUsesPreferTyCon = do
  src <- TIO.readFile "src/HaskellFlows/Tool/Info.hs"
  let code = T.unlines (filter (not . isDocLine) (T.lines src))
  pure $ T.isInfixOf "preferTyCon infos" code
      -- The old first-name shortcut must be gone
      && not (T.isInfixOf "n :| _" code)
  where
    isDocLine ln = "--" `T.isPrefixOf` T.stripStart ln

--------------------------------------------------------------------------------
-- Issue #184 — AConLike data constructors render as "Name :: Type"
--------------------------------------------------------------------------------

-- | #184: Info.hs must have an explicit 'AConLike (RealDataCon dc)' branch
-- so that data constructors like 'Just', 'Nothing', 'True' are rendered as
-- "Name :: Type" and NOT routed to the catch-all renderDefinition which
-- produced the garbled "data Just Data constructor 'Just'" output.
testInfoAConLikeBranchExists :: IO Bool
testInfoAConLikeBranchExists = do
  src <- TIO.readFile "src/HaskellFlows/Tool/Info.hs"
  let code = T.unlines (filter (not . isDocLine) (T.lines src))
  pure $ T.isInfixOf "AConLike (RealDataCon dc)" code
      && T.isInfixOf "AConLike (PatSynCon ps)" code
  where
    isDocLine ln = "--" `T.isPrefixOf` T.stripStart ln

-- | #184: the RealDataCon branch must use 'dataConDisplayType' to build
-- the "Name :: Type" string, not 'renderDefinition' (which prepends "data ").
testInfoAConLikeUsesDisplayType :: IO Bool
testInfoAConLikeUsesDisplayType = do
  src <- TIO.readFile "src/HaskellFlows/Tool/Info.hs"
  let code = T.unlines (filter (not . isDocLine) (T.lines src))
  pure $ T.isInfixOf "dataConDisplayType" code
      -- The old garble path must not be taken for constructors:
      && not (T.isInfixOf "renderDefinition kind nm renderedThing" code
               && T.isInfixOf "AConLike" (T.unlines
                    [ ln | ln <- T.lines code
                    , T.isInfixOf "renderDefinition kind nm renderedThing" ln ]))
  where
    isDocLine ln = "--" `T.isPrefixOf` T.stripStart ln

--------------------------------------------------------------------------------
-- Issue #111 — forall stripping in parseSignature
--------------------------------------------------------------------------------

-- | #111: 'stripForall' handles the inferred-variable form @forall {a}.@
-- produced by GHC 9.2+ for non-specified variables.
testStripForallInferred :: IO Bool
testStripForallInferred =
  pure $ stripForall "forall {a}. [a] -> [a]" == "[a] -> [a]"

-- | #111: 'stripForall' handles the explicit @forall a.@ form.
testStripForallExplicit :: IO Bool
testStripForallExplicit =
  pure $ stripForall "forall a. [a] -> [a]" == "[a] -> [a]"

-- | #111: 'stripForall' is a no-op when there is no leading @forall@.
testStripForallNoop :: IO Bool
testStripForallNoop =
  pure $ stripForall "[a] -> [a]" == "[a] -> [a]"
      && stripForall "a -> a"     == "a -> a"

-- | #111: 'parseSignature' handles a full @forall {a}.@ prefix and
-- produces a valid 'ParsedSig' where arg == return (required for
-- the involutive + list-roundtrip rules to fire on @reverse@).
testParseSigForallList :: IO Bool
testParseSigForallList =
  case parseSignature "forall {a}. [a] -> [a]" of
    Nothing  -> pure False
    Just sig -> pure $ isSameTypeThroughout sig && length (psArgs sig) == 1

-- | #111: the involutive rule must fire for a @reverse@-shaped signature
-- even when the string carries a leading @forall {a}.@ (the form GHC
-- 9.2+ emits from 'exprType TM_Inst').
testRulesFireForForallReverse :: IO Bool
testRulesFireForForallReverse =
  case parseSignature "forall {a}. [a] -> [a]" of
    Nothing  -> pure False
    Just sig ->
      let suggs = applyRules "reverse" sig
      in pure $ any ((== "Involutive")           . sLaw) suggs
             && any ((== "Self-inverse on lists") . sLaw) suggs

--------------------------------------------------------------------------------
-- Issue #137 — Haddock comment stripping in parseSignature
--------------------------------------------------------------------------------

-- | #137: 'stripLineComments' strips a '-- ^' Haddock annotation to
-- end of line, preserving the type token before it.
testStripLineCommentsHaddock :: IO Bool
testStripLineCommentsHaddock =
  let raw    = "Set.Set String   -- ^ existing context module names"
      result = stripLineComments raw
  in pure $ T.isInfixOf "Set.Set String" result
         && not ("--" `T.isInfixOf` result)

-- | #137: mid-line '--' comment is stripped.
testStripLineCommentsMid :: IO Bool
testStripLineCommentsMid =
  let raw    = "Int -- some note"
      result = stripLineComments raw
  in pure (T.strip result == "Int")

-- | #137: comment-free lines pass through unchanged (modulo trailing
-- whitespace normalisation by 'T.stripEnd').
testStripLineCommentsClean :: IO Bool
testStripLineCommentsClean =
  let raw = "Set.Set String -> [String] -> [String]"
  in pure (T.strip (stripLineComments raw) == raw)

-- | #137: 'parseSignature' correctly parses a multi-line Haddock-annotated
-- signature into its component types, recovering the exact same result as
-- the comment-free version.
testParseSigHaddockAnnotated :: IO Bool
testParseSigHaddockAnnotated =
  let annotated = "Set.Set String   -- ^ existing names\n  -> [String]   -- ^ candidates\n  -> [String]"
      clean     = "Set.Set String -> [String] -> [String]"
  in pure (parseSignature annotated == parseSignature clean)
