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
    -- * Issue #202 — binding-tail patch
  , patchTailBindings
    -- * Issue #235 — preceding type-sig patch
  , isTypeSigLine
  , patchPrecedingTypeSig
    -- * Test-only
  , previewResult
  ) where

import Control.Exception (SomeException, try)
import Data.Char (isAsciiLower, isAsciiUpper, isDigit)
import Data.Maybe (catMaybes, fromMaybe)
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
import HaskellFlows.Util.Safe (safeAt)

descriptor :: ToolDescriptor
descriptor =
  ToolDescriptor
    { tdName        = toolNameText GhcFixWarning
    , tdDescription =
        "PURPOSE: Propose (or apply) a patch for a common GHC warning. "
          <> "WHEN: a ghc_load / ghc_check_module surfaced a fixable code "
          <> "(GHC-66111 unused import, GHC-40910 unused binding when "
          <> "'name' is supplied, missing top-level signature). "
          <> "WHEN NOT: the diagnostic is a type error — that is "
          <> "ghc_explain_error, not a warning. "
          <> "PREREQUISITES: a previous compile pass produced the "
          <> "diagnostic at the given (module_path, line, code). "
          <> "OUTPUT: {fixable, patch?, hint?, applied?}; read-only by "
          <> "default — pass apply=true to write the file. "
          <> "SEE ALSO: ghc_explain_error, ghc_lint."
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
          let lns = T.lines body
              ix  = fwLine args - 1
          -- Issue #247: validate line number before proceeding.
          -- An out-of-bounds line previously silently yielded
          -- srcLine="" which produced fixable:false with no
          -- indication the input was wrong.
          case safeAt ix lns of
            Nothing -> pure (errorResult
                   ("Line " <> T.pack (show (fwLine args))
                    <> " is out of bounds — the file has "
                    <> T.pack (show (length lns)) <> " line"
                    <> (if length lns == 1 then "" else "s") <> "."))
            Just srcLine -> do
              let plan    = planForCodeWithName (fwCode args)
                              (fwName args) srcLine
              if fwApply args && fpFixable plan
                then writePatched full plan args body
                else pure (previewResult full plan args)

-- | Issue #202: after renaming a GHC-40910 type-sig line, scan
-- subsequent lines and rename every binding equation whose first
-- token is exactly @nm@ (at column 0, word-boundary). Stops as
-- soon as a line does not start with @nm@.
--
-- Example: given @nm = "unusedBinding"@ and tail
--   @["unusedBinding = 42", ""]@
-- returns @["_unusedBinding = 42", ""]@.
patchTailBindings :: Text -> [Text] -> [Text]
patchTailBindings nm = go
  where
    go []     = []
    go (l:ls) =
      if isBindingFor l
        then case underscorePrefix nm l of
               Just patched -> patched : go ls
               Nothing      -> l : ls   -- already prefixed or ambiguous
        else l : ls  -- first non-binding line → stop

    -- True when line starts with @nm@ at column 0 with a word
    -- boundary after it (i.e. it's a binding equation, not a type
    -- signature or unrelated identifier).
    isBindingFor l =
      case T.stripPrefix nm l of
        Nothing   -> False
        Just rest ->
          -- Don't match the sig itself (":: …") — that's already patched.
          not (T.isPrefixOf "::" (T.stripStart rest))
            && case T.uncons rest of
                 Nothing     -> True
                 Just (c, _) -> not (isIdentChar c)

    isIdentChar c = isAsciiLower c || isAsciiUpper c
                 || isDigit c || c == '_' || c == '\''

-- | #235: True when @line@ is a type-signature line for @nm@, i.e.
-- it starts with @nm@ at the beginning of the (stripped) line and is
-- immediately followed by @::@ (with optional surrounding whitespace).
-- Used to distinguish \"patching a type sig\" from \"patching a binding\"
-- so 'writePatched' knows whether to also fix the preceding type sig.
isTypeSigLine :: Text -> Text -> Bool
isTypeSigLine nm l =
  let stripped = T.stripStart l
  in nm `T.isPrefixOf` stripped
       && "::" `T.isPrefixOf` T.stripStart (T.drop (T.length nm) stripped)

-- | #235: Scan backward through @ls@ (the lines BEFORE the warning
-- line) for the type-signature belonging to @nm@.  Skips blank lines
-- and Haddock / inline comment lines (@-- …@).  Stops — and returns
-- the original list unchanged — when it encounters a non-blank,
-- non-comment line that is NOT the expected type sig.
--
-- When the type sig is found, renames @nm :: …@ to @_nm :: …@
-- (preserving indentation and everything after @::@).
--
-- Example:
--
-- > patchPrecedingTypeSig "listSum"
-- >   ["-- | Sum of a list.", "listSum :: [Int] -> Int"]
-- > == ["-- | Sum of a list.", "_listSum :: [Int] -> Int"]
patchPrecedingTypeSig :: Text -> [Text] -> [Text]
patchPrecedingTypeSig nm ls = fromMaybe ls (go (length ls - 1))
  where
    go i
      | i < 0     = Nothing
      | otherwise = case safeAt i ls of
          Nothing   -> Nothing  -- index out of bounds; give up
          Just line
            | isTypeSigLine nm line ->
                let before  = take i ls
                    renamed = T.replace (nm <> " ::") ("_" <> nm <> " ::") line
                    after   = drop (i + 1) ls
                in Just (before <> [renamed] <> after)
            | isBlankOrComment line -> go (i - 1)
            | otherwise -> Nothing  -- hit a non-blank non-comment non-sig; give up

    isBlankOrComment l =
      T.null (T.strip l) || "--" `T.isPrefixOf` T.stripStart l

writePatched :: FilePath -> FixPlan -> FixWarningArgs -> Text -> IO ToolResult
writePatched full plan args body = do
  let lns        = T.lines body
      ix         = fwLine args - 1
      totalLines = length lns
  -- Issue #221: guard before any patch logic so out-of-bounds
  -- lines return a clear error rather than a silent no-op.
  if ix < 0 || ix >= totalLines
    then pure (Env.toolResponseToResult (Env.mkFailed
          (Env.mkErrorEnvelope Env.Validation
            (T.pack ( "line " <> show (fwLine args)
                   <> " out of bounds — file has "
                   <> show totalLines <> " line(s)" )))))
    else do
      let (pre, rest) = splitAt ix lns
          newLns
            | fpDrop plan = case rest of
                []       -> lns
                (_ : tl) -> pre <> tl
            | Just patched <- fpPatch plan = case rest of
                []       -> lns
                -- Issue #202: for GHC-40910 also rename any binding
                -- equations that immediately follow the patched line.
                (origLine : tl) ->
                  let fixedTail = case (fwCode args, fwName args) of
                        ("GHC-40910", Just nm) -> patchTailBindings nm tl
                        _                      -> tl
                      -- #235: when patching a BINDING (not a type sig),
                      -- also rename the type signature that precedes it.
                      -- Without this, the type sig retains the old name
                      -- while the binding gains the underscore prefix,
                      -- causing GHC-44432 "type sig lacks binding".
                      fixedPre = case (fwCode args, fwName args) of
                        ("GHC-40910", Just nm)
                          | not (isTypeSigLine nm origLine) ->
                              patchPrecedingTypeSig nm pre
                        _ -> pre
                  in fixedPre <> [patched] <> fixedTail
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
  Env.toolResponseToResult (Env.mkOk (object (catMaybes
    [ Just ("applied"   .= False)
    , Just ("fixable"   .= fpFixable plan)
    , Just ("path"      .= T.pack path)
    , Just ("code"      .= fwCode args)
    , Just ("line"      .= fwLine args)
    , Just ("hint"      .= fpHint plan)
    , Just ("dropLine"  .= fpDrop plan)
    , ("patch" .=) <$> fpPatch plan
    ])))

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
