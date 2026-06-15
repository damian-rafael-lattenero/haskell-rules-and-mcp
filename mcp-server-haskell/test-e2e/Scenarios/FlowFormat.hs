-- | Flow: 'ghc_format' — fourmolu/ormolu formatting.
--
-- Closes a coverage gap: ghc_format had no dedicated E2E scenario.
-- Oracles hold whether or not a formatter is installed on the runner:
--
--   1. Path traversal in module_path → refused (deterministic).
--   2. Non-existent module → failed with a clean validation error, not a
--      raw formatter backtrace (deterministic; #246).
--   3. write=false on a deliberately mis-formatted module → when a
--      formatter is present, status=ok / check_only=true / wrote=false
--      and the returned text is NORMALISED ("foo :: Int", "foo = 42")
--      where the input had collapsed spacing — a real normalisation
--      oracle, not "has a formatted field". No formatter → unavailable.
--   4. write=true → wrote=true / check_only=false AND the file on disk
--      actually changed to the normalised form. No formatter → unavailable.
module Scenarios.FlowFormat
  ( runFlow
  ) where

import Data.Aeson (Value (..), object, (.=))
import Data.Maybe (isJust)
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.IO as TIO
import System.Directory (createDirectoryIfMissing, findExecutable)
import System.FilePath ((</>))
import System.IO (readFile')

import E2E.Assert
  ( Check (..)
  , checkPure
  , liveCheck
  , stepFooter
  , stepHeader
  )
import qualified E2E.Client as Client
import E2E.Envelope (errorKind, fieldBool, fieldText, statusIs)
import HaskellFlows.Mcp.ToolName (ToolName (..))

-- | A deliberately mis-formatted but syntactically valid module:
-- collapsed/over-spaced operators a formatter must normalise.
messySrc :: Text
messySrc = T.unlines
  [ "module Messy where"
  , ""
  , "foo    ::    Int"
  , "foo=    42"
  ]

runFlow :: Client.McpClient -> FilePath -> IO [Check]
runFlow c projectDir = do
  ----------------------------------------------------------------
  -- Step 1 — path traversal is refused (deterministic).
  ----------------------------------------------------------------
  t0 <- stepHeader 1 "ghc_format path traversal → refused"
  rTrav <- Client.callTool c GhcFormat
             (object [ "module_path" .= ("../../../../etc/passwd" :: Text) ])
  c1 <- liveCheck $ checkPure
          "traversal module_path → status=refused, kind=path_traversal"
          (statusIs "refused" rTrav && errorKind rTrav == Just "path_traversal")
          ("expected refused/path_traversal; got: " <> truncRender rTrav)
  stepFooter 1 t0

  ----------------------------------------------------------------
  -- Step 2 — non-existent module → clean failed, not a backtrace.
  ----------------------------------------------------------------
  t1 <- stepHeader 2 "ghc_format non-existent module → failed (clean)"
  rMissing <- Client.callTool c GhcFormat
                (object [ "module_path" .= ("src/DoesNotExist.hs" :: Text) ])
  c2 <- liveCheck $ checkPure
          "missing module → status=failed, kind=module_path_does_not_exist"
          ( statusIs "failed" rMissing
              && errorKind rMissing == Just "module_path_does_not_exist" )
          ("expected failed/module_path_does_not_exist; got: " <> truncRender rMissing)
  stepFooter 2 t1

  ----------------------------------------------------------------
  -- write a messy module for the formatting steps.
  ----------------------------------------------------------------
  createDirectoryIfMissing True (projectDir </> "src")
  TIO.writeFile (projectDir </> "src" </> "Messy.hs") messySrc
  mFormatter <- findExecutable "fourmolu"
  mFormatter2 <- maybe (findExecutable "ormolu") (pure . Just) mFormatter
  let haveFormatter = isJust mFormatter2

  ----------------------------------------------------------------
  -- Step 3 — write=false normalises and does NOT touch disk.
  ----------------------------------------------------------------
  t2 <- stepHeader 3 "ghc_format write=false previews normalised text"
  rPreview <- Client.callTool c GhcFormat
                (object [ "module_path" .= ("src/Messy.hs" :: Text)
                        , "write"       .= False
                        ])
  c3 <- liveCheck $ checkPure
          (if haveFormatter
             then "formatter present → ok, check_only=true, text normalised"
             else "no formatter → status=unavailable")
          (if haveFormatter then previewNormalised rPreview
                            else statusIs "unavailable" rPreview)
          ("haveFormatter=" <> T.pack (show haveFormatter)
            <> "; got: " <> truncRender rPreview)
  -- write=false must not have rewritten the file.
  diskUnchanged <- (== T.unpack messySrc) <$> readFile' (projectDir </> "src" </> "Messy.hs")
  c3b <- liveCheck $ checkPure
          "write=false left the file on disk unchanged"
          (not haveFormatter || diskUnchanged)
          "write=false must not rewrite the module"
  stepFooter 3 t2

  ----------------------------------------------------------------
  -- Step 4 — write=true rewrites the file to the normalised form.
  ----------------------------------------------------------------
  t3 <- stepHeader 4 "ghc_format write=true rewrites in place"
  rWrite <- Client.callTool c GhcFormat
              (object [ "module_path" .= ("src/Messy.hs" :: Text)
                      , "write"       .= True
                      ])
  onDisk <- readFile' (projectDir </> "src" </> "Messy.hs")
  c4 <- liveCheck $ checkPure
          (if haveFormatter
             then "formatter present → wrote=true, check_only=false, file normalised on disk"
             else "no formatter → status=unavailable")
          (if haveFormatter
             then statusIs "ok" rWrite
                    && fieldBool "wrote" rWrite == Just True
                    && fieldBool "check_only" rWrite == Just False
                    && ("foo :: Int" `T.isInfixOf` T.pack onDisk)
             else statusIs "unavailable" rWrite)
          ("haveFormatter=" <> T.pack (show haveFormatter)
            <> "; resp: " <> truncRender rWrite)
  stepFooter 4 t3

  pure [c1, c2, c3, c3b, c4]

--------------------------------------------------------------------------------
-- oracles
--------------------------------------------------------------------------------

-- | write=false success: ok envelope, check_only flag set, file not
-- rewritten, and the previewed text is the NORMALISED form (canonical
-- spacing the messy input lacked).
previewNormalised :: Value -> Bool
previewNormalised r =
  statusIs "ok" r
    && fieldBool "check_only" r == Just True
    && fieldBool "wrote" r == Just False
    && case fieldText "formatted" r of
         Just t  -> "foo :: Int" `T.isInfixOf` t && "foo = 42" `T.isInfixOf` t
         Nothing -> False

truncRender :: Value -> Text
truncRender v =
  let raw = T.pack (show v)
      cap = 600
   in if T.length raw > cap then T.take cap raw <> "…(truncated)" else raw
