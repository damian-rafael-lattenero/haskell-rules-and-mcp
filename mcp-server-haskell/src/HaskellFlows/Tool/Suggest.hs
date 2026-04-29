-- | @ghc_suggest@ — emit candidate QuickCheck laws for a function
-- based on its type signature.
--
-- Flow:
--
-- 1. Query GHCi for the function's type via @:t@.
-- 2. Parse the type structurally via 'HaskellFlows.Parser.TypeSignature'.
-- 3. Run every rule in 'HaskellFlows.Suggest.Rules.allRules' against
--    the parsed signature.
-- 4. Return the matching suggestions with @law@, @property@
--    (ready-to-run), @rationale@, @confidence@, @category@.
--
-- Innovation vs TS port:
--
-- * Rule engine is composable — rules are values in a list, not
--   hard-coded @if-else@ branches. Extending is a one-line append
--   in 'Suggest.Rules.allRules'.
-- * Each suggestion carries a @rationale@ and @confidence@ — the
--   agent can weight which to try first.
-- * Optional @category@ filter lets the caller scope to only
--   algebraic / list / monoid laws when that's what they need.
module HaskellFlows.Tool.Suggest
  ( descriptor
  , handle
  , SuggestArgs (..)
  , outOfScopeResult
    -- * Sibling-aware helpers (BUG-03)
  , gatherSiblings
  , parseShowModules
  , parseBrowseBindings
  ) where

import Data.Aeson
import Data.Aeson.Types (parseEither)
import Data.Char (isAsciiLower)
import Data.Maybe (mapMaybe)
import Data.Text (Text)
import qualified Data.Text as T

import Control.Exception (SomeException, try)

import GHC
  ( Ghc
  , ModSummary
  , TcRnExprMode (TM_Inst)
  , TyThing (AnId)
  , exprType
  , getModuleGraph
  , getModuleInfo
  , mgModSummaries
  , modInfoExports
  , modInfoLookupName
  , ms_mod
  )
import GHC.Types.Id (idType)
import GHC.Types.Name (nameOccName)
import GHC.Types.Name.Occurrence (occNameString)
import GHC.Utils.Outputable (showPprUnsafe)

import HaskellFlows.Ghc.ApiSession
  ( GhcSession
  , firstLibraryOrTestSuite
  , LoadFlavour (..)
  , loadForTarget
  , withGhcSession
  )
import HaskellFlows.Ghc.Sanitize
  ( sanitizeExpression
  )
import qualified HaskellFlows.Mcp.Envelope as Env
import HaskellFlows.Mcp.Protocol
import HaskellFlows.Mcp.ToolName (ToolName (..), toolNameText)
import HaskellFlows.Parser.Error (GhcError)
import HaskellFlows.Parser.Type (isOutOfScope)
import HaskellFlows.Parser.TypeSignature (ParsedSig, parseSignature)
import HaskellFlows.Suggest.Rules
  ( Confidence (..)
  , RuleContext (..)
  , Suggestion (..)
  , applyRulesCtx
  )

descriptor :: ToolDescriptor
descriptor =
  ToolDescriptor
    { tdName        = toolNameText GhcSuggest
    , tdDescription =
        "Given a function name, propose QuickCheck properties that "
          <> "the function's type signature suggests. Each suggestion "
          <> "includes a ready-to-run property expression, a rationale, "
          <> "and a confidence score. Feed the property straight into "
          <> "ghc_quickcheck."
    , tdInputSchema =
        object
          [ "type"       .= ("object" :: Text)
          , "properties" .= object
              [ "function_name" .= object
                  [ "type"        .= ("string" :: Text)
                  , "description" .=
                      ("Name of the function to analyse. Example: "
                       <> "\"reverse\", \"Prelude.map\"." :: Text)
                  ]
              , "category" .= object
                  [ "type"        .= ("string" :: Text)
                  , "description" .=
                      ("Optional category filter: \"algebraic\", \"list\", "
                       <> "\"monoid\", \"predicate\". Omit to return all "
                       <> "matching laws." :: Text)
                  ]
              ]
          , "required"             .= ["function_name" :: Text]
          , "additionalProperties" .= False
          ]
    }

data SuggestArgs = SuggestArgs
  { saFunctionName :: !Text
  , saCategory     :: !(Maybe Text)
  }
  deriving stock (Show)

instance FromJSON SuggestArgs where
  parseJSON = withObject "SuggestArgs" $ \o -> do
    fn <- o .:  "function_name"
    c  <- o .:? "category"
    pure SuggestArgs { saFunctionName = fn, saCategory = c }

handle :: GhcSession -> Value -> IO ToolResult
handle ghcSess rawArgs = case parseEither parseJSON rawArgs of
  Left parseError ->
    pure (parseErrorResult parseError)
  Right args -> case sanitizeExpression (saFunctionName args) of
    Left e -> pure (Env.toolResponseToResult
                      (Env.mkRefused (Env.sanitizeRejection "function_name" e)))
    Right safe -> do
      tgt <- firstLibraryOrTestSuite ghcSess
      eLoad <- try (loadForTarget ghcSess tgt Strict)
      case eLoad :: Either SomeException (Bool, [GhcError]) of
        Left ex ->
          pure (subprocessResult
                  ("loadForTarget failed: " <> T.pack (show ex)))
        Right _ -> do
          eType <- try (withGhcSession ghcSess (queryType safe))
          case eType :: Either SomeException Text of
            Left ex ->
              pure (outOfScopeResult safe (T.pack (show ex)))
            Right typeText
              | isOutOfScope typeText ->
                  pure (outOfScopeResult safe typeText)
              | otherwise ->
                  case parseSignature typeText of
                    Nothing ->
                      pure (validationErr
                        ("Could not parse signature: " <> typeText))
                    Just sig -> do
                      siblings <- gatherSiblings ghcSess safe
                      let ctx = RuleContext
                            { rcName     = safe
                            , rcSig      = sig
                            , rcSiblings = siblings
                            }
                          matches  = applyRulesCtx ctx
                          filtered = case saCategory args of
                            Nothing -> matches
                            Just c  -> filter ((c ==) . sCategory) matches
                      pure (successResult safe typeText filtered)

-- | Issue #90 Phase C: caller-side parse failure.
parseErrorResult :: String -> ToolResult
parseErrorResult err =
  let kind | "key" `isInfixOfStr` err = Env.MissingArg
           | otherwise                = Env.TypeMismatch
      envErr = (Env.mkErrorEnvelope kind
                  (T.pack ("Invalid arguments: " <> err)))
                    { Env.eeCause = Just (T.pack err) }
  in Env.toolResponseToResult (Env.mkFailed envErr)
  where
    isInfixOfStr needle haystack =
      let n = length needle
      in any (\i -> take n (drop i haystack) == needle)
             [0 .. length haystack - n]

-- | Issue #90 Phase C: GHC API exception (load threw).
subprocessResult :: Text -> ToolResult
subprocessResult msg =
  Env.toolResponseToResult
    (Env.mkFailed (Env.mkErrorEnvelope Env.SubprocessError msg))

-- | Issue #90 Phase C: type signature parsed by GHC but our
-- in-house parser couldn't read it.
validationErr :: Text -> ToolResult
validationErr msg =
  Env.toolResponseToResult
    (Env.mkFailed (Env.mkErrorEnvelope Env.Validation msg))

-- | Ask GHC for the type of @safe@. Exceptions (unresolved name,
-- ambiguous type, …) are caught at the IO layer by the caller.
queryType :: Text -> Ghc Text
queryType safe = do
  t <- exprType TM_Inst (T.unpack safe)
  pure (T.pack (showPprUnsafe t))

--------------------------------------------------------------------------------
-- response shaping
--------------------------------------------------------------------------------

-- | Issue #90 Phase C: matched suggestions → status='ok'. The
-- payload shape ('function', 'signature', 'count', 'suggestions',
-- 'hint') is preserved verbatim under 'result'.
successResult :: Text -> Text -> [Suggestion] -> ToolResult
successResult fn sig suggestions =
  Env.toolResponseToResult (Env.mkOk (object
    [ "function"    .= fn
    , "signature"   .= sig
    , "count"       .= length suggestions
    , "suggestions" .= map renderSuggestion suggestions
    , "hint"        .= hintFor suggestions
    ]))

renderSuggestion :: Suggestion -> Value
renderSuggestion s =
  object
    [ "law"        .= sLaw s
    , "property"   .= sProperty s
    , "rationale"  .= sRationale s
    , "confidence" .= renderConfidence (sConfidence s)
    , "category"   .= sCategory s
    ]

renderConfidence :: Confidence -> Text
renderConfidence = \case
  High   -> "high"
  Medium -> "medium"
  Low    -> "low"

hintFor :: [Suggestion] -> Text
hintFor [] =
  "No laws matched the signature. Common reasons: function is effectful \
  \(IO / monadic), arity > 2, or return type is polymorphic in a way rules \
  \don't pattern-match on yet."
hintFor xs =
  "Try the highest-confidence suggestion first via ghc_quickcheck. "
  <> "Total: " <> T.pack (show (length xs))
  <> " candidate law(s). High-confidence: "
  <> T.pack (show (length (filter ((High ==) . sConfidence) xs)))
  <> "."

-- | BUG-15: a structured response for the "function not in scope"
-- case. The raw GHC error (@<interactive>:1:1: error: [GHC-88464]
-- Variable not in scope: foo@) is passed back as @ghcError@ for
-- drill-in, but the top-level keys speak the agent's language:
-- 'reason' names the class of failure and 'hint' tells the agent
-- exactly which tool to call next. The Server.runTool nextStep
-- push still runs — steering the agent straight at @ghc_load@
-- instead of letting it guess.
--
-- Issue #90 Phase C: status='no_match' kind='not_in_scope'. The
-- pre-envelope pre-existing fields ('reason', 'function', 'hint',
-- 'ghcError') stay under 'result' — consumers branch on those
-- exactly as before.
outOfScopeResult :: Text -> Text -> ToolResult
outOfScopeResult fn ghcOutput =
  let payload  = object
        [ "reason"   .= ("function_not_in_scope" :: Text)
        , "function" .= fn
        , "hint"     .=
            ( "`" <> fn <> "` is not in scope in the current GHCi \
              \session. Load the module that defines it first via \
              \ghc_load(module_path=\"<path>\"), or pass a fully \
              \qualified name (e.g. \"Data.List.sort\") if the \
              \definition lives in an already-loaded module under \
              \a qualified import."
              :: Text )
        , "ghcError" .= ghcOutput
        ]
      envErr   = Env.mkErrorEnvelope Env.NotInScope
                   ("'" <> fn <> "' is not in scope")
      response = (Env.mkNoMatch payload) { Env.reError = Just envErr }
  in Env.toolResponseToResult response

--------------------------------------------------------------------------------
-- BUG-03: sibling discovery
--------------------------------------------------------------------------------

-- | Discover the top-level bindings that live next to the focal
-- function and return them as the @rcSiblings@ input to
-- 'applyRulesCtx'.
--
-- Two sources are walked and merged:
--
--   (a) The focal function's HOME module — where the name is
--       defined. Standard sibling: @simplify@ co-defined with
--       @simpNeg@ / @simpAdd@ in @Expr.Simplify@.
--   (b) The CURRENTLY-FOCUSED module — flagged with @*@ in
--       @:show modules@. When a project has a harness module
--       that re-exports several source modules (e.g. a
--       test/Gen module re-exporting @simplify@ + @eval@ +
--       @pretty@ for QuickCheck), loading it makes those
--       re-exports visible as siblings of the focal.
--
-- Flow:
--
-- 1. @:info \<focalName\>@ → "Defined at <path>" → focal file.
-- 2. @:show modules@ → [(ModuleName, FilePath)] + the
--    @*@-focused module.
-- 3. Browse both the focal's home module and the focused one
--    (if distinct). De-duplicate names.
-- 4. @:browse \<module\>@ → @name :: type@ per binding.
-- 5. Parse each line; keep lower-case value bindings whose
--    type parses; drop the focal itself.
--
-- Any step failing produces @[]@ so the non-sibling rules
-- still fire — degradation, never crash.
gatherSiblings :: GhcSession -> Text -> IO [(Text, ParsedSig)]
gatherSiblings ghcSess focalName = do
  eRes <- try (withGhcSession ghcSess (collectSiblings focalName))
  pure $ case eRes :: Either SomeException [(Text, ParsedSig)] of
    Left _   -> []
    Right xs -> xs

-- | Walk the loaded module graph. For each module, iterate its
-- exported 'Name's, resolve each to a 'TyThing', keep only value
-- bindings ('AnId'), render the name + type as Text, parse the
-- signature, and return the (name, sig) pair. Drops the focal
-- function itself. Dedup by name (first wins).
collectSiblings :: Text -> Ghc [(Text, ParsedSig)]
collectSiblings focalName = do
  mg <- getModuleGraph
  allPairs <- concat <$> mapM collectFromModule (mgModSummaries mg)
  pure (nubByName allPairs)
  where
    collectFromModule :: ModSummary -> Ghc [(Text, ParsedSig)]
    collectFromModule ms = do
      mInfo <- getModuleInfo (ms_mod ms)
      case mInfo of
        Nothing   -> pure []
        Just info -> do
          let names = modInfoExports info
          allPairs <- mapM (tryId info) names
          pure [ (nm, sig)
               | Just (nm, ty) <- allPairs
               , nm /= focalName
               , Just sig <- [parseSignature ty]
               ]

    -- Look up a Name's 'TyThing'; keep only value bindings (AnId).
    -- Returns Just (name, typeText) or Nothing when not a value.
    tryId info nm = do
      mThing <- modInfoLookupName info nm
      pure $ case mThing of
        Just (AnId i) ->
          let occ = T.pack (occNameString (nameOccName nm))
              ty  = T.pack (showPprUnsafe (idType i))
          in Just (occ, ty)
        _ -> Nothing

-- | Dedup a list of @(Text, a)@ by the first projection,
-- keeping the first occurrence.
nubByName :: [(Text, a)] -> [(Text, a)]
nubByName = go []
  where
    go _    []                 = []
    go seen ((n, s) : rest)
      | n `elem` seen          = go seen rest
      | otherwise              = (n, s) : go (n : seen) rest

-- Wave-5 removed `nubText`, `catMaybesT`, `focusedModule`,
-- `moduleForFile`, `_siblingsFromBrowse` — all only made sense in
-- the text-based @:browse@ + @:show modules@ parsing era. The
-- in-process collectSiblings walks the module graph directly.

-- | Parse GHCi's @:show modules@ output into @(ModuleName, FilePath)@
-- tuples. Typical line (stripped):
--
-- > Expr.Simplify    ( src/Expr/Simplify.hs, interpreted )
--
-- A leading asterisk marks the @*@-focused module — this variant
-- drops it; use 'parseShowModulesLines' if you need to know
-- which module is focused.
parseShowModules :: Text -> [(Text, FilePath)]
parseShowModules =
  map (\(_, n, p) -> (n, p)) . parseShowModulesLines

-- | Parse GHCi's @:show modules@ output while preserving the
-- @*@-focus flag per row. Used by 'gatherSiblings' to also
-- browse the currently-focused module when it differs from the
-- focal function's home (e.g. a test harness module that
-- re-exports several source modules).
parseShowModulesLines :: Text -> [(Bool, Text, FilePath)]
parseShowModulesLines = mapMaybe parseShowLine . T.lines
  where
    parseShowLine raw =
      let s0           = T.strip raw
          (starred, s) =
            if "* " `T.isPrefixOf` s0 then (True, T.drop 2 s0)
                                      else (False, s0)
          (name, rest) = T.breakOn "(" s
          name'        = T.strip name
          inside       = T.drop 1 rest
          (file, _)    = T.breakOn "," inside
          file'        = T.strip file
      in if T.null name' || T.null file' || T.null rest
           then Nothing
           else Just (starred, name', T.unpack file')

-- | Extract top-level value bindings from @:browse@ output as
-- @(name, type)@ tuples. Skips type/class declarations (names
-- that don't start with a lowercase letter or underscore) and
-- ignores lines without a top-level @::@.
parseBrowseBindings :: Text -> [(Text, Text)]
parseBrowseBindings = mapMaybe parseOne . T.lines
  where
    parseOne raw =
      let s = T.strip raw
      in if T.null s || indented raw
           then Nothing       -- indented continuation lines belong to a prior entry
           else do
             let (before, after) = T.breakOn "::" s
                 name = T.strip before
                 ty   = T.strip (T.drop 2 after)
             if T.null before || T.null after || T.null name || T.null ty
               then Nothing
               else case T.uncons name of
                      Just (c, _) | isAsciiLower c || c == '_' -> Just (name, ty)
                      _                                        -> Nothing

    indented raw = case T.uncons raw of
      Just (c, _) -> c == ' ' || c == '\t'
      Nothing     -> False

-- | Combine 'parseBrowseBindings' + 'parseSignature', drop the
-- focal function itself, and yield rule siblings. Not used by
-- the Wave-5 handler (which walks the module graph directly),
-- but kept because 'parseBrowseBindings' still has a unit-test
-- and co-locating the helper makes the parser layer self-contained.
_siblingsFromBrowse :: Text -> Text -> [(Text, ParsedSig)]
_siblingsFromBrowse focalName browseOut =
  [ (nm, sig)
  | (nm, ty) <- parseBrowseBindings browseOut
  , nm /= focalName
  , Just sig <- [parseSignature ty]
  ]
