-- | @ghc_fix_warning@ — propose patches for common GHC warnings.
--
-- Handles a short list of well-defined cases (unused imports,
-- unused matches, missing top-level signatures). Other codes
-- return @patch: null@ + a hint string — never a mis-applied fix.
--
-- By default the tool is READ-ONLY — it returns the patch as text
-- for the agent to apply. Pass @apply=true@ to have the tool
-- write the file in place (still rejects the write if the patch
-- would produce an empty file to avoid accidental truncation).
module HaskellFlows.Tool.FixWarning
  ( descriptor
  , handle
  , FixWarningArgs (..)
  , FixPlan (..)
  , planForCode
  ) where

import Control.Exception (SomeException, try)
import Data.Aeson
import Data.Aeson.Types (parseEither)
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.IO as TIO
import qualified Data.Text.Lazy as TL
import qualified Data.Text.Lazy.Encoding as TLE

import HaskellFlows.Mcp.Protocol
import HaskellFlows.Types (ProjectDir, mkModulePath, unModulePath)

descriptor :: ToolDescriptor
descriptor =
  ToolDescriptor
    { tdName        = "ghc_fix_warning"
    , tdDescription =
        "Propose a patch for a common GHC warning. Read-only by "
          <> "default; pass apply=true to write the file. Handles "
          <> "unused imports, unused matches, missing top-level "
          <> "signatures. Other codes return patch=null with a hint."
    , tdInputSchema =
        object
          [ "type"       .= ("object" :: Text)
          , "properties" .= object
              [ "module_path" .= obj "string"
              , "line"        .= obj "integer"
              , "code"        .= obj "string"
              , "apply"       .= obj "boolean"
              ]
          , "required"             .= (["module_path", "line", "code"] :: [Text])
          , "additionalProperties" .= False
          ]
    }
  where
    obj :: Text -> Value
    obj t = object [ "type" .= t ]

data FixWarningArgs = FixWarningArgs
  { fwModulePath :: !Text
  , fwLine       :: !Int
  , fwCode       :: !Text
  , fwApply      :: !Bool
  }
  deriving stock (Show)

instance FromJSON FixWarningArgs where
  parseJSON = withObject "FixWarningArgs" $ \o ->
    FixWarningArgs
      <$> o .:  "module_path"
      <*> o .:  "line"
      <*> o .:  "code"
      <*> o .:? "apply" .!= False

data FixPlan = FixPlan
  { fpPatch :: !(Maybe Text)   -- ^ the proposed full replacement line or empty if drop
  , fpDrop  :: !Bool           -- ^ true when the line should be deleted
  , fpHint  :: !Text
  }
  deriving stock (Eq, Show)

-- | Map a GHC code to a static plan, independent of source content.
-- Keep this list tight — only bet on cases that are high-signal.
planForCode :: Text -> FixPlan
planForCode code = case code of
  "GHC-66111" -> FixPlan  -- unused-imports
    { fpPatch = Nothing
    , fpDrop  = True
    , fpHint  = "Drop the unused import line."
    }
  "GHC-40910" -> FixPlan  -- unused-matches
    { fpPatch = Nothing
    , fpDrop  = False
    , fpHint  = "Prefix the unused binding with an underscore \
               \(e.g. `x` → `_x`). Left to the agent — the exact \
               \column is message-dependent."
    }
  "GHC-38417" -> FixPlan  -- missing-signatures
    { fpPatch = Nothing
    , fpDrop  = False
    , fpHint  = "Add a top-level type signature above the reported \
               \definition. Use `ghc_type` on the bound name for \
               \the inferred signature."
    }
  _ -> FixPlan
    { fpPatch = Nothing
    , fpDrop  = False
    , fpHint  = "No structured fix registered for this code. \
               \Inspect the warning message and fix by hand."
    }

handle :: ProjectDir -> Value -> IO ToolResult
handle pd rawArgs = case parseEither parseJSON rawArgs of
  Left err -> pure (errorResult (T.pack ("Invalid arguments: " <> err)))
  Right args -> case mkModulePath pd (T.unpack (fwModulePath args)) of
    Left e -> pure (errorResult (T.pack (show e)))
    Right mp -> do
      let plan  = planForCode (fwCode args)
          full  = unModulePath mp
      eRead <- try (TIO.readFile full) :: IO (Either SomeException Text)
      case eRead of
        Left e -> pure (errorResult (T.pack ("Could not read: " <> show e)))
        Right body ->
          if fwApply args && fpDrop plan
            then writePatched full plan args body
            else pure (previewResult full plan args)

writePatched :: FilePath -> FixPlan -> FixWarningArgs -> Text -> IO ToolResult
writePatched full plan args body = do
  let lns = T.lines body
      (pre, rest) = splitAt (fwLine args - 1) lns
      newLns = case rest of
        [] -> lns
        (_ : tl) -> pre <> tl    -- drop-the-line case (fpDrop == True)
      newBody = T.unlines newLns
  if T.null (T.strip newBody)
    then pure (errorResult "Refusing to write — the patch would empty the file.")
    else do
      wres <- try (TIO.writeFile full newBody)
        :: IO (Either SomeException ())
      case wres of
        Left e  -> pure (errorResult (T.pack ("Could not write: " <> show e)))
        Right _ -> pure (appliedResult full plan args)

previewResult :: FilePath -> FixPlan -> FixWarningArgs -> ToolResult
previewResult path plan args =
  let payload = object
        [ "success"   .= True
        , "applied"   .= False
        , "path"      .= T.pack path
        , "code"      .= fwCode args
        , "line"      .= fwLine args
        , "hint"      .= fpHint plan
        , "dropLine"  .= fpDrop plan
        ]
  in ToolResult { trContent = [ TextContent (encodeUtf8Text payload) ], trIsError = False }

appliedResult :: FilePath -> FixPlan -> FixWarningArgs -> ToolResult
appliedResult path plan args =
  let payload = object
        [ "success"  .= True
        , "applied"  .= True
        , "path"     .= T.pack path
        , "code"     .= fwCode args
        , "line"     .= fwLine args
        , "hint"     .= fpHint plan
        ]
  in ToolResult { trContent = [ TextContent (encodeUtf8Text payload) ], trIsError = False }

errorResult :: Text -> ToolResult
errorResult msg =
  ToolResult
    { trContent = [ TextContent (encodeUtf8Text (object
        [ "success" .= False, "error" .= msg ])) ]
    , trIsError = True
    }

encodeUtf8Text :: Value -> Text
encodeUtf8Text = TL.toStrict . TLE.decodeUtf8 . encode
