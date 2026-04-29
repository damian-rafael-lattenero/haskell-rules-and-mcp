-- | Flow: 'ghc_create_project' rejects non-Hackage-conformant
-- package names BEFORE any filesystem write (#58).
--
-- Pre-fix behaviour
-- -----------------
-- The validator only checked \"alnum + hyphen\". Mixed-case,
-- consecutive hyphens, trailing hyphens, and leading digits all
-- slipped through and surfaced downstream as confusing
-- \"Target files already exist\" overwrite errors that blamed
-- the wrong rule.
--
-- New contract
-- ------------
-- 'validateName' enforces every Hackage rule and the response
-- carries the rejected name PLUS the specific violation so the
-- agent can rename without guessing.
module Scenarios.FlowCreateProjectNameValidation
  ( runFlow
  ) where

import Data.Aeson (Value (..), object, (.=))
import qualified Data.Aeson.Key as Key
import qualified Data.Aeson.KeyMap as KeyMap
import Data.Text (Text)
import qualified Data.Text as T
import System.Directory (doesFileExist, listDirectory)

import E2E.Assert
  ( Check (..)
  , checkPure
  , liveCheck
  , stepFooter
  , stepHeader
  )
import qualified E2E.Client as Client
import E2E.Envelope (statusOk, lookupField)
import HaskellFlows.Mcp.ToolName (ToolName (..))

runFlow :: Client.McpClient -> FilePath -> IO [Check]
runFlow c projectDir = do
  -- Step 1 — uppercase name must be rejected; .cabal must NOT be
  -- written.
  t0 <- stepHeader 1 "ghc_create_project rejects uppercase name (#58)"
  rUpper <- Client.callTool c GhcCreateProject
              (object [ "name" .= ("Bad-Name" :: Text) ])
  let okShape = statusOk rUpper == Just False
      errMsg  = lookupString "error" rUpper
      msgOK   = case errMsg of
        Just m -> T.isInfixOf "Bad-Name" m
                && (T.isInfixOf "lowercase" m
                      || T.isInfixOf "Hackage" m)
        Nothing -> False
  -- And no .cabal landed on disk.
  entries <- listDirectory projectDir
  let noCabalLeak = not (any (T.isSuffixOf ".cabal" . T.pack) entries)
  cUpper <- liveCheck $ checkPure
    "uppercase name → success=false, error names rule, no FS write"
    (okShape && msgOK && noCabalLeak)
    ( "Expected: success=false, error mentions 'Bad-Name' + lowercase/Hackage, \
      \no .cabal in projectDir. Got: success=" <> T.pack (show okShape)
      <> ", err=" <> T.pack (show errMsg)
      <> ", entries=" <> T.pack (show entries) )
  stepFooter 1 t0

  -- Step 2 — consecutive hyphens.
  t1 <- stepHeader 2 "ghc_create_project rejects double hyphen (#58)"
  rDouble <- Client.callTool c GhcCreateProject
               (object [ "name" .= ("foo--bar" :: Text) ])
  let dblOK = statusOk rDouble == Just False
            && case lookupString "error" rDouble of
                 Just m -> T.isInfixOf "consecutive hyphens" m
                 Nothing -> False
  cDouble <- liveCheck $ checkPure
    "double-hyphen rejected with named violation"
    dblOK
    ("Expected named-rule rejection. Got: " <> truncRender rDouble)
  stepFooter 2 t1

  -- Step 3 — leading digit.
  t2 <- stepHeader 3 "ghc_create_project rejects leading digit (#58)"
  rDigit <- Client.callTool c GhcCreateProject
              (object [ "name" .= ("9pkg" :: Text) ])
  let digitOK = statusOk rDigit == Just False
              && case lookupString "error" rDigit of
                   Just m -> T.isInfixOf "lowercase letter" m
                   Nothing -> False
  cDigit <- liveCheck $ checkPure
    "leading-digit rejected with start-rule message"
    digitOK
    ("Expected start-rule rejection. Got: " <> truncRender rDigit)
  stepFooter 3 t2

  -- Step 4 — happy path: a canonical name still works.
  t3 <- stepHeader 4 "ghc_create_project accepts lowercase-hyphen name (#58)"
  rOk <- Client.callTool c GhcCreateProject
           (object [ "name" .= ("good-pkg" :: Text) ])
  let okFlag = statusOk rOk == Just True
  cabalLanded <- doesFileExist (projectDir <> "/good-pkg.cabal")
  cOk <- liveCheck $ checkPure
    "good-pkg accepted; good-pkg.cabal landed on disk"
    (okFlag && cabalLanded)
    ( "Expected success=true and good-pkg.cabal to exist. \
      \Got: success=" <> T.pack (show okFlag)
      <> ", cabalLanded=" <> T.pack (show cabalLanded)
      <> ", raw=" <> truncRender rOk )
  stepFooter 4 t3

  pure [cUpper, cDouble, cDigit, cOk]

--------------------------------------------------------------------------------
-- helpers
--------------------------------------------------------------------------------

lookupString :: Text -> Value -> Maybe Text
lookupString k v = case lookupField k v of
  Just (String s) -> Just s
  _               -> Nothing

truncRender :: Value -> Text
truncRender v =
  let raw = T.pack (show v)
      cap = 600
  in if T.length raw > cap then T.take cap raw <> "…(truncated)" else raw
