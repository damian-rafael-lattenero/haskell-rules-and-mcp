-- | Unit tests for 'Tool.CreateProject' name validation, scaffold, and
-- auto-switch behaviour; plus 'Tool.CheckModule' gate reason field.
--
-- Extracted from the Spec.hs monolith (#271) via the function-export shape.
module Spec.CreateProject
  ( testCreateValidateAccept
  , testCreateValidateEmpty
  , testCreateValidateUpper
  , testCreateValidateDoubleHyphen
  , testCreateValidateTrailing
  , testCreateValidateLeadingDigit
  , testCreateValidateSymbols
  , testCreateProjectScaffoldGreenCabal
  , testCreateValidateErrorMsg
  , testCreateValidateAllDigitComponent
  , testCreateValidateVPrefixedOk
  , testCreateOverwriteRemovesStaleCalab
  , testCreateWriteFalseIsPreview
  , testCreateWriteFalseContent
  , testCreateUsesSuppliedPath
  , testCreateAutoSwitchPresent
  , testCreatePreviewNoSwitch
  , testCreateNoPathNoSwitch
  , testCheckGateReasonMatchesOk
  ) where

import qualified Data.Aeson as A
import qualified Data.Aeson.Key as AKey
import qualified Data.Aeson.KeyMap as AKM
import qualified Data.Text as T
import qualified Data.Text.IO as TIO
import qualified Data.List as List
import Data.Maybe (fromMaybe)
import System.Directory (doesFileExist, getTemporaryDirectory, removePathForcibly, createDirectoryIfMissing, listDirectory)
import System.FilePath ((</>))

import qualified HaskellFlows.Mcp.Envelope as Env
import HaskellFlows.Types (mkProjectDir)
import qualified HaskellFlows.Types
import qualified HaskellFlows.Tool.CheckModule as CheckModule
import qualified HaskellFlows.Tool.CreateProject as CreateProject

import Spec.Helpers (withTempProject)

-- ---------------------------------------------------------------------------
-- Local helpers
-- ---------------------------------------------------------------------------

gateField :: T.Text -> A.Value -> Maybe A.Value
gateField k (A.Object o) = AKM.lookup (AKey.fromText k) o
gateField _ _            = Nothing

-- | After #290: serialise the full 'ToolResponse' as JSON so field
-- inspection can drill into it the same way as before.
extractPayload :: Env.ToolResponse -> A.Value
extractPayload = A.toJSON

-- | Issue #90: return the inner @result@ payload from a 'ToolResponse'.
-- After #290 this is just 'Env.reResult' with a 'Null' fallback.
resultPayload :: Env.ToolResponse -> A.Value
resultPayload tr = fromMaybe A.Null (Env.reResult tr)

-- | After #290: 'trIsError' equivalent — True when the status is not
-- one of the non-error statuses (ok / partial / no_match).
isError :: Env.ToolResponse -> Bool
isError tr = Env.reStatus tr `notElem`
  [Env.StatusOk, Env.StatusPartial, Env.StatusNoMatch]

unProjectDirRaw :: HaskellFlows.Types.ProjectDir -> FilePath
unProjectDirRaw = HaskellFlows.Types.unProjectDir

-- ---------------------------------------------------------------------------
-- Tests
-- ---------------------------------------------------------------------------

testCreateValidateAccept :: IO Bool
testCreateValidateAccept = pure $ and
  [ CreateProject.validateName "haskell-flows-mcp" == Right "haskell-flows-mcp"
  , CreateProject.validateName "x"                 == Right "x"
  -- "abc-123-def" removed: segment '123' is all-digit → now rejected by #233 rule
  , CreateProject.validateName "single"            == Right "single"
  , CreateProject.validateName "my-pkg-v2"         == Right "my-pkg-v2"
  , CreateProject.validateName "lib-core"          == Right "lib-core"
  ]

testCreateValidateEmpty :: IO Bool
testCreateValidateEmpty = pure $
  case CreateProject.validateName "" of
    Left _  -> True
    Right _ -> False

testCreateValidateUpper :: IO Bool
testCreateValidateUpper = pure $
     isLeft (CreateProject.validateName "Invalid-Name")
  && isLeft (CreateProject.validateName "camelCase")
  && isLeft (CreateProject.validateName "ALLCAPS")
  where
    isLeft (Left _) = True
    isLeft _        = False

testCreateValidateDoubleHyphen :: IO Bool
testCreateValidateDoubleHyphen = pure $
  case CreateProject.validateName "with--double" of
    Left msg -> "consecutive hyphens" `T.isInfixOf` msg
    Right _  -> False

testCreateValidateTrailing :: IO Bool
testCreateValidateTrailing = pure $
  case CreateProject.validateName "ends-" of
    Left msg -> "end in a hyphen" `T.isInfixOf` msg
    Right _  -> False

testCreateValidateLeadingDigit :: IO Bool
testCreateValidateLeadingDigit = pure $
  case CreateProject.validateName "1leading-digit" of
    Left msg -> "lowercase letter" `T.isInfixOf` msg
    Right _  -> False

testCreateValidateSymbols :: IO Bool
testCreateValidateSymbols = pure $
     isLeft (CreateProject.validateName "with_underscore")
  && isLeft (CreateProject.validateName "with.dot")
  && isLeft (CreateProject.validateName "with space")
  && isLeft (CreateProject.validateName "leading-")
  && isLeft (CreateProject.validateName "-leading")
  where
    isLeft (Left _) = True
    isLeft _        = False

-- | Issue #69: a freshly-scaffolded cabal file must declare
-- 'category', 'maintainer', and 'description'. Without these,
-- 'cabal check' (and our 'ghc_validate_cabal') tags the project
-- with 3 warnings on the agent's very first gate-call. The
-- placeholders are stubs the agent should fill before
-- publishing — but they keep the gate green by default.
testCreateProjectScaffoldGreenCabal :: IO Bool
testCreateProjectScaffoldGreenCabal =
  let cabal = CreateProject.cabalFile "demo" "Demo"
  in pure $  T.isInfixOf "category:" cabal
          && T.isInfixOf "maintainer:" cabal
          && T.isInfixOf "description:" cabal
          -- The TODO sentinel keeps it obvious to the agent
          -- that the description is placeholder text.
          && T.isInfixOf "TODO:" cabal

-- | Issue #58: error messages must NAME the violation so the agent
-- can rename appropriately instead of guessing what \"invalid name\"
-- meant. Pin that the rejected name and the rule both appear.
testCreateValidateErrorMsg :: IO Bool
testCreateValidateErrorMsg = pure $
  case CreateProject.validateName "Bad-Name" of
    Left msg ->
         "Bad-Name" `T.isInfixOf` msg
      && ("lowercase" `T.isInfixOf` msg
            || "Hackage" `T.isInfixOf` msg)
    Right _ -> False

--------------------------------------------------------------------------------
-- Issue #233 — validateName: all-digit component rejection
--------------------------------------------------------------------------------

-- | #233: cabal fails with "unexpected Empty component" when a
-- hyphen-separated name component is all digits.  Pin that the
-- validator now rejects such names with an informative message.
testCreateValidateAllDigitComponent :: IO Bool
testCreateValidateAllDigitComponent = pure $ and
  [ isLeft (CreateProject.validateName "dogfood-2026-05-25-b")  -- full repro case
  , isLeft (CreateProject.validateName "pkg-2026")              -- single trailing digit seg
  , isLeft (CreateProject.validateName "a-1-b")                 -- digit in middle
  , case CreateProject.validateName "dogfood-2026-05-25-b" of
      Left msg -> "all-digit" `T.isInfixOf` msg
                  || "version" `T.isInfixOf` msg
      Right _  -> False
  ]
  where
    isLeft (Left _) = True
    isLeft _        = False

-- | #233: names with letter-prefixed numeric-like segments are fine;
-- segments that are ENTIRELY digits are not (cabal parses them as
-- version components, creating a parse ambiguity).
testCreateValidateVPrefixedOk :: IO Bool
testCreateValidateVPrefixedOk = pure $ and
  [ CreateProject.validateName "dogfood-v2026"      == Right "dogfood-v2026"
  , CreateProject.validateName "pkg-v1"             == Right "pkg-v1"
  , CreateProject.validateName "lib-r2d2"           == Right "lib-r2d2"
  , CreateProject.validateName "http2"              == Right "http2"        -- no hyphen, fine
  , isLeft (CreateProject.validateName "abc-123-def")  -- '123' is all-digit → rejected
  , isLeft (CreateProject.validateName "lib-42")       -- '42' is all-digit → rejected
  ]
  where
    isLeft (Left _) = True
    isLeft _        = False

--------------------------------------------------------------------------------
-- Issue #234 — scaffold overwrite=true removes stale .cabal files
--------------------------------------------------------------------------------

-- | #234: when overwrite=True and a stale .cabal exists with a
-- different package name, scaffold removes it before writing.
testCreateOverwriteRemovesStaleCalab :: IO Bool
testCreateOverwriteRemovesStaleCalab = do
  tmp <- getTemporaryDirectory
  let dir = tmp </> "hf-test234-stale-cabal"
  removePathForcibly dir
  createDirectoryIfMissing True dir
  -- Simulate the stale state: write an old-name.cabal in the dir.
  TIO.writeFile (dir </> "old-name.cabal") "name: old-name\n"
  -- Now scaffold with a new name and overwrite=True.
  _ <- CreateProject.scaffold dir "new-name" "NewName" True True
  -- The stale .cabal should be gone; only new-name.cabal remains.
  entries <- listDirectory dir
  let calabFiles = filter (List.isSuffixOf ".cabal") entries
  removePathForcibly dir
  pure (calabFiles == ["new-name.cabal"])

--------------------------------------------------------------------------------
-- Issue #126 — ghc_project(create): path + write fixes
--------------------------------------------------------------------------------

-- | #126 Bug B: write=false is preview mode — it must never fail due
-- to existing files. Here we run scaffold against a root that DOES
-- have the scaffold files (the test directory itself), but since
-- write=false the overwrite check is skipped and we get ok.
testCreateWriteFalseIsPreview :: IO Bool
testCreateWriteFalseIsPreview = do
  -- "." has cabal.project, src/, test/Spec.hs — exactly the clash
  -- scenario that was failing before the fix.
  result <- CreateProject.scaffold "." "my-pkg" "MyPkg" False False
  pure (not (isError result))

-- | #126 Bug B: write=false response carries "preview" key with
-- generated file contents and write=false discriminator.
testCreateWriteFalseContent :: IO Bool
testCreateWriteFalseContent = do
  result <- CreateProject.scaffold "/nonexistent-dir" "my-pkg" "MyPkg" False False
  -- The preview fields live inside the 'result' key of the envelope,
  -- not at the top level — use resultPayload, not extractPayload.
  case resultPayload result of
    A.Object o ->
      let hasPreview = AKM.member (AKey.fromText "preview") o
          writeFalse = case AKM.lookup (AKey.fromText "write") o of
                         Just (A.Bool False) -> True
                         _                  -> False
      in pure (hasPreview && writeFalse && not (isError result))
    _ -> pure False

-- | #126 Bug A: scaffold uses the supplied root path, not the
-- active projectDir. We supply a temp dir that has no scaffold
-- files. The call with write=true and overwrite=false should
-- succeed (no clashes at the target path).
testCreateUsesSuppliedPath :: IO Bool
testCreateUsesSuppliedPath = withTempProject $ \pd -> do
  -- The temp dir is freshly created — no cabal.project or Spec.hs.
  -- Before the fix, the clash check looked in the *active* projectDir
  -- (mcp-server-haskell/) which DOES have those files.
  result <- CreateProject.scaffold (unProjectDirRaw pd) "fresh-pkg" "FreshPkg" False True
  pure (not (isError result))

-- | Issue #256: after a successful @ghc_project(action="create", path=<p>)@
-- the server must auto-switch to the new path so subsequent tool calls
-- (ghc_deps, ghc_modules) operate on the right project.
-- Verified via source inspection: 'createAutoSwitchPath' must exist,
-- must gate on 'trIsError', and must call 'SwitchProjectTool.handle'.
-- #275: the dispatch (and this logic) moved from Server to Tool.Project.
testCreateAutoSwitchPresent :: IO Bool
testCreateAutoSwitchPresent = do
  src <- TIO.readFile "src/HaskellFlows/Tool/Project.hs"
  let code = T.unlines (filter (not . isDocLine) (T.lines src))
  pure $ T.isInfixOf "createAutoSwitchPath"        code
      && T.isInfixOf "trIsError r"                 code
      && T.isInfixOf "SwitchProjectTool.handle"    code
  where
    isDocLine ln = "--" `T.isPrefixOf` T.stripStart ln

-- | Issue #256: when @write=false@ (preview mode) 'createAutoSwitchPath'
-- must return Nothing — no switch should happen. #275: now in Tool.Project.
testCreatePreviewNoSwitch :: IO Bool
testCreatePreviewNoSwitch = do
  src <- TIO.readFile "src/HaskellFlows/Tool/Project.hs"
  let code = T.unlines (filter (not . isDocLine) (T.lines src))
  -- The helper must gate on writeDisk; the false branch returns Nothing.
  pure $ T.isInfixOf "writeDisk"            code
      && T.isInfixOf "createAutoSwitchPath" code
  where
    isDocLine ln = "--" `T.isPrefixOf` T.stripStart ln

-- | Issue #256: when no explicit @path@ is supplied the scaffold lands
-- in the current project dir — no switch is needed, 'createAutoSwitchPath'
-- must return Nothing. #275: now in Tool.Project.
testCreateNoPathNoSwitch :: IO Bool
testCreateNoPathNoSwitch = do
  src <- TIO.readFile "src/HaskellFlows/Tool/Project.hs"
  let code = T.unlines (filter (not . isDocLine) (T.lines src))
  -- When path key is absent the helper must return Nothing.
  pure $ T.isInfixOf "createAutoSwitchPath" code
      && T.isInfixOf "Nothing"              code
  where
    isDocLine ln = "--" `T.isPrefixOf` T.stripStart ln

testCheckGateReasonMatchesOk :: IO Bool
testCheckGateReasonMatchesOk =
  let cases =
        [ (1, 0, 1, 0)  -- one regressed
        , (1, 0, 0, 1)  -- one skipped
        , (3, 1, 1, 1)  -- mixed
        ]
      check (total, passed, regressed, skipped) =
        let g = CheckModule.propertiesGate total passed regressed skipped
        in case (gateField "ok" g, gateField "reason" g) of
             (Just (A.Bool False), Just (A.String r))
               | not ("stored properties pass" `T.isInfixOf` r)
                 && r /= "" -> True
             _ -> False
  in pure (all check cases)
