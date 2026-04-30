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
    -- * Issue #55 — concrete-patch helpers
  , planForCodeWithName
  , underscorePrefix
  ) where

import Control.Exception (SomeException, try)
import Data.Char (isAsciiLower, isAsciiUpper, isDigit)
import Data.Aeson
import Data.Aeson.Types (parseEither)
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.IO as TIO

import qualified HaskellFlows.Mcp.Envelope as Env
import HaskellFlows.Mcp.PermissiveJSON
  ( IntField (unIntField)
  , BoolField (unBoolField)
  )
import HaskellFlows.Mcp.Protocol
import HaskellFlows.Mcp.ToolName (ToolName (..), toolNameText)
import HaskellFlows.Types (ProjectDir, mkModulePath, unModulePath)

descriptor :: ToolDescriptor
descriptor =
  ToolDescriptor
    { tdName        = toolNameText GhcFixWarning
    , tdDescription =
        "Propose a patch for a common GHC warning. Read-only by "
          <> "default; pass apply=true to write the file. Handles "
          <> "unused imports (GHC-66111), unused bindings (GHC-40910 "
          <> "when 'name' is supplied), missing top-level signatures. "
          <> "Response carries 'fixable' so the agent knows whether "
          <> "to expect a concrete patch or just a hint."
    , tdInputSchema =
        object
          [ "type"       .= ("object" :: Text)
          , "properties" .= object
              [ "module_path" .= obj "string"
              , "line"        .= obj "integer"
              , "code"        .= obj "string"
              , "apply"       .= obj "boolean"
              , "name"        .= object
                  [ "type"        .= ("string" :: Text)
                  , "description" .=
                      ("Issue #55: identifier the warning names. \
                       \Required for GHC-40910 (unused-binding) to \
                       \produce a concrete patch — the tool prefixes \
                       \this name with an underscore on the given line. \
                       \Optional for codes whose patch doesn't depend \
                       \on a binding name." :: Text)
                  ]
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
  , fwName       :: !(Maybe Text)
  }
  deriving stock (Show)

-- | Issue #88: 'line' and 'apply' accept stringified forms
-- ("3" / "true") so MCP host clients that serialise primitives
-- as strings can still drive 'ghc_fix_warning'. The JSON Schema
-- still advertises @integer@ / @boolean@ — the parser is just
-- more lenient about what it accepts.
instance FromJSON FixWarningArgs where
  parseJSON = withObject "FixWarningArgs" $ \o ->
    FixWarningArgs
      <$> o .:  "module_path"
      <*> (unIntField <$> o .:  "line")
      <*> o .:  "code"
      <*> (maybe False unBoolField <$> o .:? "apply")
      <*> o .:? "name"

data FixPlan = FixPlan
  { fpPatch   :: !(Maybe Text)   -- ^ replacement line (Just = replace, Nothing = no patch)
  , fpDrop    :: !Bool           -- ^ true when the line should be deleted
  , fpHint    :: !Text
  , fpFixable :: !Bool           -- ^ #55: machine-readable \"can this tool patch?\" signal
  }
  deriving stock (Eq, Show)

-- | Map a GHC code to a static plan, independent of source content.
-- Keep this list tight — only bet on cases that are high-signal.
--
-- Issue #55: 'fpFixable' is the machine-readable signal the agent
-- branches on. @True@ means \"the tool can produce a concrete
-- patch with the inputs you've already passed\"; @False@ means
-- \"only advice — fix by hand\".
planForCode :: Text -> FixPlan
planForCode code = case code of
  "GHC-66111" -> FixPlan  -- unused-imports
    { fpPatch   = Nothing
    , fpDrop    = True
    , fpHint    = "Drop the unused import line."
    , fpFixable = True
    }
  "GHC-40910" -> FixPlan  -- unused-matches / unused-binding
    { fpPatch   = Nothing
    , fpDrop    = False
    , fpHint    = "Prefix the unused binding with an underscore \
                  \(e.g. `x` → `_x`). Pass 'name' so the tool can \
                  \produce a concrete patch."
    , fpFixable = False
    }
  "GHC-38417" -> FixPlan  -- missing-signatures
    { fpPatch   = Nothing
    , fpDrop    = False
    , fpHint    = "Add a top-level type signature above the reported \
                  \definition. Use `ghc_type` on the bound name for \
                  \the inferred signature."
    , fpFixable = False
    }
  _ -> FixPlan
    { fpPatch   = Nothing
    , fpDrop    = False
    , fpHint    = "No structured fix registered for this code. \
                  \Inspect the warning message and fix by hand."
    , fpFixable = False
    }

-- | Issue #55: refine 'planForCode' with a binding name when the
-- caller has it. For @GHC-40910@ this turns the advice-only plan
-- into a concrete-patch plan that prefixes the binding with an
-- underscore on the source line. The patch is line-level: the
-- caller already gave us 'fwLine', and 'underscorePrefix' rewrites
-- a free occurrence of the name on that one line.
planForCodeWithName :: Text -> Maybe Text -> Text -> FixPlan
planForCodeWithName code mName srcLine =
  let base = planForCode code
  in case (code, mName) of
       ("GHC-40910", Just nm) ->
         case underscorePrefix nm srcLine of
           Just patched ->
             base
               { fpPatch   = Just patched
               , fpDrop    = False
               , fpFixable = True
               , fpHint    = "Prefix '" <> nm <> "' with an underscore \
                             \on the warning's line."
               }
           Nothing -> base   -- name not found on the line; degrade gracefully
       _ -> base

-- | Issue #55: replace the FIRST free word-boundary occurrence of
-- @name@ on @srcLine@ with @\"_\" <> name@. Returns 'Nothing' when
-- the name doesn't appear as a token on the line (string literals
-- / comments / substring matches don't count). Conservative on
-- purpose: if there's any ambiguity we'd rather emit no patch
-- than a wrong one.
underscorePrefix :: Text -> Text -> Maybe Text
underscorePrefix name srcLine =
  let target = "_" <> name
  in if T.isInfixOf target srcLine
       then Nothing  -- already underscore-prefixed; leave alone
       else go (T.length name) (T.unpack srcLine)
  where
    nameStr   = T.unpack name
    nameLen   = length nameStr
    -- Walk the line keeping track of whether the previous char was
    -- an identifier char. Replace the FIRST occurrence whose
    -- surroundings are NOT identifier chars (word-boundary).
    go _ s = case findToken s False of
      Nothing       -> Nothing
      Just (pre, post) ->
        Just (T.pack pre <> "_" <> name <> T.pack post)

    findToken :: String -> Bool -> Maybe (String, String)
    findToken []          _    = Nothing
    findToken str@(c:cs)  prev =
      case matchHere str prev of
        Just rest -> Just ("", rest)
        Nothing   ->
          case findToken cs (isIdentChar c) of
            Just (pre, post) -> Just (c : pre, post)
            Nothing          -> Nothing

    matchHere s prev
      | not prev
      , take nameLen s == nameStr
      , not (any isIdentChar (take 1 (drop nameLen s)))
      = Just (drop nameLen s)
      | otherwise = Nothing

    isIdentChar c = isAsciiLower c
                 || isAsciiUpper c
                 || isDigit c
                 || c == '_' || c == '\''

handle :: ProjectDir -> Value -> IO ToolResult
handle pd rawArgs = case parseEither parseJSON rawArgs of
  Left err -> pure (errorResult (T.pack ("Invalid arguments: " <> err)))
  Right args -> case mkModulePath pd (T.unpack (fwModulePath args)) of
    Left e -> pure (pathTraversalResult (T.pack (show e)))
    Right mp -> do
      let full  = unModulePath mp
      eRead <- try (TIO.readFile full) :: IO (Either SomeException Text)
      case eRead of
        Left e -> pure (errorResult (T.pack ("Could not read: " <> show e)))
        Right body -> do
          -- Issue #55: refine the static plan with the source line
          -- + binding name so we can promote the GHC-40910 case
          -- from advice-only to concrete-patch.
          let lns      = T.lines body
              ix       = fwLine args - 1
              srcLine  = if ix >= 0 && ix < length lns then lns !! ix else ""
              plan     = planForCodeWithName (fwCode args)
                           (fwName args) srcLine
          if fwApply args && fpFixable plan
            then writePatched full plan args body
            else pure (previewResult full plan args)

writePatched :: FilePath -> FixPlan -> FixWarningArgs -> Text -> IO ToolResult
writePatched full plan args body = do
  let lns = T.lines body
      ix  = fwLine args - 1
      (pre, rest) = splitAt ix lns
      newLns
        | fpDrop plan = case rest of
            []       -> lns
            (_ : tl) -> pre <> tl
        | Just patched <- fpPatch plan = case rest of
            []       -> lns
            (_ : tl) -> pre <> [patched] <> tl
        | otherwise = lns  -- defensive: shouldn't reach when fpFixable=True
      newBody = T.unlines newLns
  if T.null (T.strip newBody)
    then pure (errorResult "Refusing to write — the patch would empty the file.")
    else do
      wres <- try (TIO.writeFile full newBody)
        :: IO (Either SomeException ())
      case wres of
        Left e  -> pure (errorResult (T.pack ("Could not write: " <> show e)))
        Right _ -> pure (appliedResult full plan args)

-- | Issue #90 Phase C: read-only preview → status='ok' with the
-- plan ('fixable', 'patch', 'hint', 'dropLine') under 'result'.
-- 'applied=False' is the explicit signal callers branch on.
previewResult :: FilePath -> FixPlan -> FixWarningArgs -> ToolResult
previewResult path plan args =
  Env.toolResponseToResult (Env.mkOk (object
    [ "applied"   .= False
    , "fixable"   .= fpFixable plan
    , "path"      .= T.pack path
    , "code"      .= fwCode args
    , "line"      .= fwLine args
    , "hint"      .= fpHint plan
    , "dropLine"  .= fpDrop plan
    , "patch"     .= fpPatch plan
    ]))

-- | Issue #90 Phase C: in-place patch → status='ok' with
-- 'applied=True'. Same shape as preview minus 'dropLine' (the
-- caller doesn't need it once the patch is on disk).
appliedResult :: FilePath -> FixPlan -> FixWarningArgs -> ToolResult
appliedResult path plan args =
  Env.toolResponseToResult (Env.mkOk (object
    [ "applied"  .= True
    , "fixable"  .= fpFixable plan
    , "path"     .= T.pack path
    , "code"     .= fwCode args
    , "line"     .= fwLine args
    , "hint"     .= fpHint plan
    , "patch"    .= fpPatch plan
    ]))

-- | Issue #90 Phase C: bad input / IO failure / 'patch would
-- empty file' refusal → status='failed', kind='validation'.
errorResult :: Text -> ToolResult
errorResult msg =
  Env.toolResponseToResult
    (Env.mkFailed (Env.mkErrorEnvelope Env.Validation msg))

-- | Issue #100 Phase C: 'mkModulePath' rejected the path (escapes
-- project root) → status='refused', kind='path_traversal'.
pathTraversalResult :: Text -> ToolResult
pathTraversalResult msg =
  Env.toolResponseToResult
    (Env.mkRefused (Env.mkErrorEnvelope Env.PathTraversal msg))
