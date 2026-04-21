-- | @ghci_suggest@ — emit candidate QuickCheck laws for a function
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
import qualified Data.Text.Lazy as TL
import qualified Data.Text.Lazy.Encoding as TLE

import HaskellFlows.Ghci.Session
import HaskellFlows.Mcp.Protocol
import HaskellFlows.Parser.Type (parseTypeOutput, ParsedType (..), isOutOfScope)
import HaskellFlows.Parser.TypeSignature (ParsedSig, parseSignature)
import HaskellFlows.Suggest.Rules
  ( Confidence (..)
  , RuleContext (..)
  , Suggestion (..)
  , applyRulesCtx
  )
import HaskellFlows.Tool.Goto (Location (..), parseDefinedAt)

descriptor :: ToolDescriptor
descriptor =
  ToolDescriptor
    { tdName        = "ghci_suggest"
    , tdDescription =
        "Given a function name, propose QuickCheck properties that "
          <> "the function's type signature suggests. Each suggestion "
          <> "includes a ready-to-run property expression, a rationale, "
          <> "and a confidence score. Feed the property straight into "
          <> "ghci_quickcheck."
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

handle :: Session -> Value -> IO ToolResult
handle sess rawArgs = case parseEither parseJSON rawArgs of
  Left parseError ->
    pure (errorResult (T.pack ("Invalid arguments: " <> parseError)))
  Right args -> case sanitizeExpression (saFunctionName args) of
    Left e -> pure (errorResult (formatCommandError e))
    Right safe -> do
      typeRes <- typeOf sess safe
      case typeRes of
        Left cmdErr ->
          pure (errorResult (formatCommandError cmdErr))
        Right gr
          -- BUG-15: surface GHC's "not in scope" diagnostic as a
          -- structured, actionable hint instead of the raw GHC
          -- error message. The most common cause is that the
          -- caller asked for laws on a function whose module
          -- hasn't been loaded in this GHCi session — the fix is
          -- to @ghci_load@ the module first, not to rephrase the
          -- function name.
          | isOutOfScope (grOutput gr) ->
              pure (outOfScopeResult safe (grOutput gr))
          | not (grSuccess gr) ->
              pure (errorResult (grOutput gr))
          | otherwise -> do
              let mParsedType = parseTypeOutput (grOutput gr)
              case mParsedType of
                Nothing -> pure (errorResult
                  ( "Could not parse ':t " <> safe <> "' output. "
                 <> "Raw:\n" <> grOutput gr ))
                Just pt -> case parseSignature (ptType pt) of
                  Nothing -> pure (errorResult
                    ( "Could not parse signature: " <> ptType pt ))
                  Just sig -> do
                    -- BUG-03: discover the focal function's home
                    -- module and collect its OTHER top-level
                    -- bindings as siblings. The sibling list is
                    -- what enables 'ruleEvaluatorPreservation' and
                    -- 'ruleConstantFoldingSoundness' to fire — the
                    -- engines match on "this function is @a -> a@
                    -- and there is a sibling whose last argument is
                    -- @a@ and whose return type is different". Pre
                    -- BUG-03 the siblings list was hard-wired to
                    -- @[]@ via 'applyRules' (back-compat single-sig
                    -- entrypoint) and the engines never fired.
                    siblings <- gatherSiblings sess safe
                    let ctx = RuleContext
                          { rcName     = safe
                          , rcSig      = sig
                          , rcSiblings = siblings
                          }
                        matches  = applyRulesCtx ctx
                        filtered = case saCategory args of
                          Nothing -> matches
                          Just c  -> filter ((c ==) . sCategory) matches
                    pure (successResult safe (ptType pt) filtered)

--------------------------------------------------------------------------------
-- response shaping
--------------------------------------------------------------------------------

successResult :: Text -> Text -> [Suggestion] -> ToolResult
successResult fn sig suggestions =
  let payload =
        object
          [ "success"     .= True
          , "function"    .= fn
          , "signature"   .= sig
          , "count"       .= length suggestions
          , "suggestions" .= map renderSuggestion suggestions
          , "hint"        .= hintFor suggestions
          ]
  in ToolResult
       { trContent = [ TextContent (encodeUtf8Text payload) ]
       , trIsError = False
       }

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
  "Try the highest-confidence suggestion first via ghci_quickcheck. "
  <> "Total: " <> T.pack (show (length xs))
  <> " candidate law(s). High-confidence: "
  <> T.pack (show (length (filter ((High ==) . sConfidence) xs)))
  <> "."

errorResult :: Text -> ToolResult
errorResult msg =
  ToolResult
    { trContent = [ TextContent (encodeUtf8Text (object
        [ "success" .= False
        , "error"   .= msg
        ]))
      ]
    , trIsError = True
    }

-- | BUG-15: a structured response for the "function not in scope"
-- case. The raw GHC error (@<interactive>:1:1: error: [GHC-88464]
-- Variable not in scope: foo@) is passed back as @ghcError@ for
-- drill-in, but the top-level keys speak the agent's language:
-- @reason@ names the class of failure and @hint@ tells the agent
-- exactly which tool to call next. 'success: false' is preserved
-- so the MCP treats this as an error for metrics, but the
-- @nextStep@ push in @Server.runTool@ still runs — steering the
-- agent straight at @ghci_load@ instead of letting it guess.
outOfScopeResult :: Text -> Text -> ToolResult
outOfScopeResult fn ghcOutput =
  ToolResult
    { trContent = [ TextContent (encodeUtf8Text (object
        [ "success"  .= False
        , "reason"   .= ("function_not_in_scope" :: Text)
        , "function" .= fn
        , "hint"     .=
            ( "`" <> fn <> "` is not in scope in the current GHCi \
              \session. Load the module that defines it first via \
              \ghci_load(module_path=\"<path>\"), or pass a fully \
              \qualified name (e.g. \"Data.List.sort\") if the \
              \definition lives in an already-loaded module under \
              \a qualified import."
              :: Text )
        , "ghcError" .= ghcOutput
        ]))
      ]
    , trIsError = True
    }

formatCommandError :: CommandError -> Text
formatCommandError = \case
  ContainsNewline  -> "function_name must be a single line (no newline characters)"
  ContainsSentinel -> "function_name contains the internal framing sentinel and was rejected"
  EmptyInput       -> "function_name is empty"
  InputTooLarge sz cap ->
    "function_name is too large (" <> T.pack (show sz) <> " chars, cap is "
      <> T.pack (show cap) <> ")"

encodeUtf8Text :: Value -> Text
encodeUtf8Text = TL.toStrict . TLE.decodeUtf8 . encode

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
gatherSiblings :: Session -> Text -> IO [(Text, ParsedSig)]
gatherSiblings sess focalName = do
  showRes <- execute sess ":show modules"
  let showLines = parseShowModulesLines (grOutput showRes)
      -- Browse EVERY loaded module. Previous attempts that
      -- narrowed to just the focal's home module miss the common
      -- pattern where the project splits one concept across
      -- several modules (Expr.Simplify + Expr.Eval + …) and a
      -- harness module re-exports them. The rule engine's
      -- 'interpreterSiblings' filters by type anyway, so an
      -- irrelevant sibling just doesn't match. ~5–10 modules
      -- per project; each @:browse@ is sub-100ms, so total
      -- cost stays under half a second.
      targets   = map (\(_, n, _) -> n) showLines
  browsed <- mapM (\m -> execute sess (":browse " <> m)) targets
  let siblings =
        nubByName
          [ s
          | gr <- browsed
          , s  <- siblingsFromBrowse focalName (grOutput gr)
          ]
  pure siblings

-- | Dedup a list of @(Text, a)@ by the first projection,
-- keeping the first occurrence.
nubByName :: [(Text, a)] -> [(Text, a)]
nubByName = go []
  where
    go _    []                 = []
    go seen ((n, s) : rest)
      | n `elem` seen          = go seen rest
      | otherwise              = (n, s) : go (n : seen) rest

nubText :: [Text] -> [Text]
nubText = go []
  where
    go _    []         = []
    go seen (x : xs)
      | x `elem` seen  = go seen xs
      | otherwise      = x : go (x : seen) xs

catMaybesT :: [Maybe Text] -> [Text]
catMaybesT = foldr (\m acc -> maybe acc (: acc) m) []

-- | Identify the currently-focused module (marked with leading
-- @*@ in GHCi's @:show modules@). GHCi marks exactly one module
-- as focused when the caller used @:load@ with an explicit
-- target; omit otherwise.
focusedModule :: [(Bool, Text, FilePath)] -> Maybe Text
focusedModule = firstJust (\(starred, name, _) -> if starred then Just name else Nothing)
  where
    firstJust _ []     = Nothing
    firstJust f (x:xs) = case f x of
      Just y  -> Just y
      Nothing -> firstJust f xs

-- | Discard the star-flag from a parsed @:show modules@ output
-- so the existing 'moduleForFile' helper can do an ordinary
-- reverse lookup.
stripStarMap :: [(Bool, Text, FilePath)] -> [(Text, FilePath)]
stripStarMap = map (\(_, n, p) -> (n, p))

-- | Extract the source file path from a @:info@ response.
focalFileFromInfo :: Text -> Maybe FilePath
focalFileFromInfo txt = case parseDefinedAt txt of
  Just (InFile f _ _) -> Just (T.unpack f)
  _                   -> Nothing

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

-- | Reverse-lookup: given a focal source file and the output of
-- @:show modules@, return the module whose file matches. An exact
-- string compare on the GHCi-reported path is all we need — GHCi
-- always reports the project-relative form.
moduleForFile :: FilePath -> [(Text, FilePath)] -> Maybe Text
moduleForFile focal = lookup focal . map swap
  where
    swap (a, b) = (b, a)

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
-- focal function itself, and yield rule siblings.
siblingsFromBrowse :: Text -> Text -> [(Text, ParsedSig)]
siblingsFromBrowse focalName browseOut =
  [ (nm, sig)
  | (nm, ty) <- parseBrowseBindings browseOut
  , nm /= focalName
  , Just sig <- [parseSignature ty]
  ]
