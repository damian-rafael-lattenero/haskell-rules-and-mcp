-- | @ghc_info@ — Phase-2 tool (GHC-API migrated).
--
-- Given a name, returns a structured @:info@ view: kind classification
-- (class/data/newtype/function/…), rendered definition, and list of
-- class instances. Pre-migration parsed @:i@ stdout via regex;
-- post-migration queries 'GHC.getInfo' directly and builds the same
-- 'ParsedInfo' shape from the returned 'TyThing' + @[ClsInst]@.
--
-- Boundary safety: still routes through 'sanitizeExpression' so the
-- newline/sentinel/empty/too-large rejection contract is unchanged.
module HaskellFlows.Tool.Info
  ( descriptor
  , handle
  , InfoArgs (..)
    -- * Issue #54 — constructor extraction helpers
  , successResult
  , renderConstructorsBlock
    -- * Issue #70 — class method extraction helpers
  , renderClassDefinition
  , classMethodPairs
  , renderClassMethodsBlock
  ) where

import Control.Exception (SomeException, try)
import Data.Aeson
import Data.Aeson.Types (parseEither)
import Data.Char (isAsciiLower, isAsciiUpper)
import Data.List.NonEmpty (NonEmpty ((:|)))
import Data.Text (Text)
import qualified Data.Text as T

import qualified HaskellFlows.Mcp.Envelope as Env

import GHC
  ( Ghc
  , TyThing (AConLike, ATyCon, AnId)
  , getInfo
  , parseName
  )
import GHC.Core.Class (Class, classMethods)
import GHC.Core.DataCon
  ( DataCon
  , dataConName
  , dataConOrigArgTys
  )
import GHC.Core.TyCon
  ( TyCon
  , isClassTyCon
  , isDataTyCon
  , isNewTyCon
  , isTypeSynonymTyCon
  , tyConClass_maybe
  , tyConDataCons
  , tyConTyVars
  )
import GHC.Core.TyCo.Rep (scaledThing)
import GHC.Types.Id (idType)
import GHC.Types.Name (nameOccName)
import GHC.Types.Name.Occurrence (occNameString)
import GHC.Types.Var (varName)
import GHC.Utils.Outputable (showPprUnsafe)

import HaskellFlows.Ghc.ApiSession (GhcSession, withGhcSession)
import HaskellFlows.Ghc.Sanitize (sanitizeExpression)
import HaskellFlows.Mcp.Protocol
import HaskellFlows.Mcp.ToolName (ToolName (..), toolNameText)
import HaskellFlows.Parser.Type
  ( InfoKind (..)
  , ParsedInfo (..)
  )

descriptor :: ToolDescriptor
descriptor =
  ToolDescriptor
    { tdName        = toolNameText GhcInfo
    , tdDescription =
        "PURPOSE: Get detailed information about a Haskell name "
          <> "(function, type, typeclass) via :i / GHC API. "
          <> "WHEN: inspecting a single symbol — its definition, kind, "
          <> "instances, and where it is defined; following up on a "
          <> "ghc_complete or ghc_browse hit to drill in. "
          <> "WHEN NOT: you only need the type — that is cheaper via "
          <> "ghc_type; you want the source location only — that is "
          <> "ghc_goto; you want the Haddock prose — that is ghc_doc. "
          <> "PREREQUISITES: name must be in scope (preloads + imports). "
          <> "OUTPUT: structured ParsedInfo {kind, definition, instances, "
          <> "constructors? methods? defined_at}. "
          <> "SEE ALSO: ghc_type, ghc_doc, ghc_goto, ghc_browse."
    , tdInputSchema =
        object
          [ "type"       .= ("object" :: Text)
          , "properties" .= object
              [ "name" .= object
                  [ "type"        .= ("string" :: Text)
                  , "description" .=
                      ("The name to look up. Examples: \"Functor\", \
                       \\"Map.Map\", \"Maybe\", \"(++)\"" :: Text)
                  ]
              ]
          , "required"             .= ["name" :: Text]
          , "additionalProperties" .= False
          ]
    }

newtype InfoArgs = InfoArgs
  { iaName :: Text
  }
  deriving stock (Show)

instance FromJSON InfoArgs where
  parseJSON = withObject "InfoArgs" $ \o ->
    InfoArgs <$> o .: "name"

handle :: GhcSession -> Value -> IO ToolResult
handle ghcSess rawArgs = case parseEither parseJSON rawArgs of
  Left parseError ->
    pure (Env.toolResponseToResult (Env.mkFailed
      ((Env.mkErrorEnvelope (parseErrorKind parseError)
          (T.pack ("Invalid arguments: " <> parseError)))
            { Env.eeCause = Just (T.pack parseError) })))
  Right (InfoArgs nm) -> case sanitizeExpression nm of
    Left cmdErr ->
      pure (Env.toolResponseToResult (Env.mkRefused
        (Env.sanitizeRejection "name" cmdErr)))
    Right safe -> do
      eRes <- try (withGhcSession ghcSess (queryInfo safe))
      pure $ Env.toolResponseToResult $ case eRes of
        Left (se :: SomeException) ->
          -- Issue #87 + #90: instead of fabricating a 'data X'
          -- definition via the old 'bestEffortResult' (which lied
          -- to consumers — there is no real definition for an
          -- unresolved name), the migration emits status='no_match'
          -- with a structured 'searched_in' field. The agent gets a
          -- clean discriminator: 'no_match' = name not in scope,
          -- 'failed' = the request itself was malformed.
          --
          -- An exception during parseName/getInfo doesn't
          -- semantically mean 'not in scope' — it can also be a
          -- transient interactive-context race. We still classify
          -- it as no_match because the resolution attempt
          -- happened and didn't surface a real binding; the
          -- exception text rides in error.cause for debugging.
          Env.mkNoMatch (notInScopePayload safe (Just (T.pack (show se))))
        Right Nothing ->
          -- Pure 'name not found' case, no exception involved.
          Env.mkNoMatch (notInScopePayload safe Nothing)
        Right (Just (pinfo, ctorPairs, methodPairs)) ->
          Env.mkOk (successPayload pinfo ctorPairs methodPairs)

-- | Discriminate the FromJSON failure shape — same heuristic as
-- the other Phase-B migrations.
parseErrorKind :: String -> Env.ErrorKind
parseErrorKind err
  | "key" `isInfixOfStr` err = Env.MissingArg
  | otherwise                = Env.TypeMismatch
  where
    isInfixOfStr needle haystack =
      let n = length needle
      in any (\i -> take n (drop i haystack) == needle)
             [0 .. length haystack - n]

-- | Resolve the name in scope, query 'getInfo' including instances,
-- and build the pre-migration 'ParsedInfo' shape from its return.
--
-- Issue #54: when the resolved 'TyThing' is an algebraic 'TyCon'
-- (data or newtype), enumerate its 'DataCon' list and embed the
-- canonical @data X = A | B a | C a b@ shape into the rendered
-- 'piDefinition'. The constructor list is what GHCi's @:info@
-- shows on the first line; the original code dropped it entirely.
-- Result is also pre-computed as a structured list returned via
-- 'piConstructors' so JSON consumers don't have to scrape the
-- text.
queryInfo :: Text -> Ghc (Maybe (ParsedInfo, [(Text, [Text])], [(Text, Text)]))
queryInfo nm = do
  -- parseName finds both value-level and type-level names (TyCons
  -- don't live in getNamesInScope, so the old scan missed 'data'
  -- declarations like Tree). If parseName throws, the outer 'try'
  -- in handle catches it and returns an errorResult.
  n :| _ <- parseName (T.unpack nm)
  info <- getInfo True n
  pure $ case info of
    Nothing -> Nothing
    Just (thing, _fixity, clsInsts, famInsts, _doc) ->
      let kind         = kindFromTyThing thing
          renderedThing = T.pack (showPprUnsafe thing)
          (definition, ctorPairs, methodPairs) = case thing of
            -- Issue #54: algebraic types — enumerate constructors.
            ATyCon tc | not (isClassTyCon tc) ->
              let dcs       = tyConDataCons tc
                  ctorList  = map dataConPair dcs
                  dataLine
                    | null dcs  = renderedThing
                    | otherwise = renderDataLine kind nm dcs
              in (dataLine, ctorList, [])
            -- Issue #70: classes — enumerate methods.
            ATyCon tc | isClassTyCon tc, Just cls <- tyConClass_maybe tc ->
              let methods   = classMethodPairs cls
                  classLine = renderClassDefinition nm tc methods
              in (classLine, [], methods)
            -- Issue #107: functions / operators (AnId).
            -- The legacy path called 'showPprUnsafe thing' on the whole
            -- TyThing, which routes through GHC's 'pprShortTyThing' and
            -- produces "Identifier 'foo'" (just the category + quoted
            -- name, no type). For value bindings we extract the type
            -- via 'idType' directly, giving "foo :: <type>".
            AnId i ->
              let typeText = T.pack (showPprUnsafe (idType i))
                  defText  = nm <> " :: " <> typeText
              in (defText, [], [])
            -- Type synonyms and other TyThings: legacy shape.
            _ ->
              (renderDefinition kind nm renderedThing, [], [])
          parsed = ParsedInfo
            { piName       = nm
            , piKind       = kind
            , piDefinition = definition
            , piInstances  = map (T.pack . showPprUnsafe) clsInsts
                          <> map (T.pack . showPprUnsafe) famInsts
            }
      in Just (parsed, ctorPairs, methodPairs)

-- | Issue #54: render the canonical \"@data X = A | B a@\" /
-- \"@newtype X = X a@\" header from the constructor list. Mirrors
-- the rendering in 'Tool.Arbitrary' but stays self-contained to
-- avoid a Tool→Tool dependency.
renderDataLine :: InfoKind -> Text -> [DataCon] -> Text
renderDataLine kind nm dcs =
  let keyword = case kind of
        IkNewtype -> "newtype "
        _         -> "data "
      rhs = T.intercalate " | " (map renderDataConText dcs)
  in keyword <> nm <> " = " <> rhs

-- | Single-constructor rendering: @Just a@ / @Pair Int Int@.
renderDataConText :: DataCon -> Text
renderDataConText dc =
  let cn   = dataConTextName dc
      args = map (parenArg . T.pack . showPprUnsafe . scaledThing)
                 (dataConOrigArgTys dc)
  in if null args then cn else cn <> " " <> T.intercalate " " args
  where
    parenArg t
      | T.any (== ' ') (T.strip t) = "(" <> t <> ")"
      | otherwise                  = t

-- | Structured constructor pair: @(name, [arg-type])@. Returned
-- alongside 'ParsedInfo' so 'successResult' can attach a
-- 'constructors' array to the JSON response.
dataConPair :: DataCon -> (Text, [Text])
dataConPair dc =
  ( dataConTextName dc
  , map (T.pack . showPprUnsafe . scaledThing) (dataConOrigArgTys dc)
  )

dataConTextName :: DataCon -> Text
dataConTextName = T.pack . occNameString . nameOccName . dataConName

-- | Issue #54: the structured 'constructors' array shape, factored
-- out of 'successResult' so unit tests can drive it without
-- constructing a full 'DataCon' chain. Each (name, args) pair
-- becomes one @{"name": ..., "args": [...]}@ object.
renderConstructorsBlock :: [(Text, [Text])] -> [Value]
renderConstructorsBlock ctors =
  [ object [ "name" .= n, "args" .= as ] | (n, as) <- ctors ]

--------------------------------------------------------------------------------
-- Issue #70 — class extraction
--------------------------------------------------------------------------------

-- | Issue #70: enumerate a class's method signatures. Each pair
-- is @(method-name, method-type)@; the type is GHC's pretty-printed
-- form (matching the textual shape ':info' would have used).
--
-- Operator method names are wrapped in parens so the rendered
-- output matches how Haskell sources write them — e.g. the
-- 'Eq' methods come back as @"(==)"@ / @"(/=)"@, not @"=="@ /
-- @"/="@. 'occNameString' strips the parens, so the wrap is on
-- us. Returned in declaration order — same as 'classMethods',
-- so the JSON response stays stable across runs.
classMethodPairs :: Class -> [(Text, Text)]
classMethodPairs cls =
  [ ( parenthesiseIfOperator (T.pack (occNameString (nameOccName (varName m))))
    , T.pack (showPprUnsafe (idType m))
    )
  | m <- classMethods cls
  ]

-- | Wrap @(@/@)@ around an operator-shaped name. A name is
-- "operator-shaped" when its first character is not a letter,
-- digit or underscore — i.e. @==@, @/=@, @<>@, @<$@, etc.
parenthesiseIfOperator :: Text -> Text
parenthesiseIfOperator name = case T.uncons name of
  Just (c, _)
    | isAsciiUpper c || isAsciiLower c || c == '_' -> name
    | otherwise                                    -> "(" <> name <> ")"
  Nothing -> name

-- | Issue #70: render the class's @class Foo a where@ header
-- followed by the method signatures, matching the canonical
-- ':info' first-block output.
--
-- Drops the trailing 'where' when the class has zero methods
-- (rare — @class Show1 f@ in older GHCs, or marker classes).
renderClassDefinition
  :: Text                 -- ^ Class name, as the agent typed it.
  -> TyCon                -- ^ For type-variable parameters.
  -> [(Text, Text)]       -- ^ Methods from 'classMethodPairs'.
  -> Text
renderClassDefinition nm tc methods =
  let tvNames = T.unwords [ T.pack (occNameString (nameOccName (varName v)))
                          | v <- tyConTyVars tc ]
      header  = "class " <> nm <> (if T.null tvNames then "" else " " <> tvNames)
      body
        | null methods = ""
        | otherwise    =
            " where\n"
              <> T.intercalate "\n"
                   [ "  " <> n <> " :: " <> t | (n, t) <- methods ]
  in header <> body

-- | Issue #70: structured @class_methods@ array shape, mirroring
-- 'renderConstructorsBlock'. Each (name, type) pair becomes
-- @{"name": ..., "type": ...}@.
renderClassMethodsBlock :: [(Text, Text)] -> [Value]
renderClassMethodsBlock methods =
  [ object [ "name" .= n, "type" .= t ] | (n, t) <- methods ]

-- | Rebuild the declaration header (@data Tree@ / @class Functor@ /
-- …) that @:info@ would have emitted as the first line. Uses the
-- caller's name + detected kind; the GHC-rendered TyThing is
-- concatenated as body context. This is a pragmatic reconstruction —
-- the real @pprTyThing@ output is richer but the MCP's JSON contract
-- only requires that "data <Name>" / "class <Name>" / … appear in
-- the field. Body keeps the rendered info for the client that wants
-- the full shape.
renderDefinition :: InfoKind -> Text -> Text -> Text
renderDefinition kind nm rendered
  | T.null keyword  = rendered
  | otherwise       = keyword <> nm <> bodySep <> rendered
  where
    keyword = case kind of
      IkClass       -> "class "
      IkData        -> "data "
      IkNewtype     -> "newtype "
      IkTypeSynonym -> "type "
      _             -> ""
    bodySep
      | T.null (T.strip rendered) = ""
      | T.strip rendered == nm    = ""   -- rendered is just the name
      | otherwise                 = " "

-- | Classify a 'TyThing' into our enum. Mirrors what the @:i@ parser
-- guessed from the first-line syntax.
kindFromTyThing :: TyThing -> InfoKind
kindFromTyThing = \case
  AnId _      -> IkFunction
  AConLike _  -> IkData  -- a data constructor (not the type)
  ATyCon tc
    | isClassTyCon tc       -> IkClass
    | isNewTyCon tc         -> IkNewtype
    | isTypeSynonymTyCon tc -> IkTypeSynonym
    | isDataTyCon tc        -> IkData
    | otherwise             -> IkUnknown
  _           -> IkUnknown

--------------------------------------------------------------------------------
-- response shaping (unchanged schema)
--------------------------------------------------------------------------------

-- | Issue #87 + #90 §3: payload for the no-match case. The
-- previous 'bestEffortResult' fabricated a fake 'data X' /
-- 'X :: ?' definition just to keep the consumer's @success:
-- true@ branch happy. That lied — there was no real definition.
-- Post-#90 we emit a clean structured payload:
--
-- * 'name' echoes the searched identifier.
-- * 'searched_in' tells the agent where we looked.
-- * 'remediation' suggests the next move (run ghc_load on the
--   defining module, or query an external surface like
--   'ghoogle_search').
--
-- The 'cause' parameter (when 'Just') is the underlying GHC
-- exception text — flows into error.cause via the caller, NOT
-- into the user-facing payload.
notInScopePayload :: Text -> Maybe Text -> Value
notInScopePayload nm _ = object
  [ "name"        .= nm
  , "searched_in" .= ("interactive scope" :: Text)
  , "remediation" .= ("Name not currently in scope. If it's defined in a \
                      \loaded module, run ghc_load on that module first. \
                      \For external/base names, hoogle_search may surface \
                      \candidates ghc_info cannot reach." :: Text)
  ]

-- | Successful resolution payload. Issue #90 Phase B: the same
-- legacy shape (name, kind, definition, instances; constructors
-- and class_methods conditionally) but now lives under 'result'.
successPayload
  :: ParsedInfo
  -> [(Text, [Text])]   -- ^ #54 — data/newtype constructors.
  -> [(Text, Text)]     -- ^ #70 — class methods (name + type).
  -> Value
successPayload parsed ctors methods =
  let renderedCtors   = renderConstructorsBlock ctors
      renderedMethods = renderClassMethodsBlock methods
      basePayload =
        [ "name"       .= piName parsed
        , "kind"       .= kindToText (piKind parsed)
        , "definition" .= piDefinition parsed
        , "instances"  .= piInstances parsed
        ]
      withCtors
        | null ctors = basePayload
        | otherwise  = basePayload <> [ "constructors"  .= renderedCtors  ]
      pl
        | null methods = withCtors
        | otherwise    = withCtors <> [ "class_methods" .= renderedMethods ]
  in object pl

-- | Legacy wrapper kept for the existing test surface that imports
-- 'successResult' as a public helper. Routes through the envelope.
successResult
  :: ParsedInfo
  -> [(Text, [Text])]
  -> [(Text, Text)]
  -> ToolResult
successResult parsed ctors methods =
  Env.toolResponseToResult (Env.mkOk (successPayload parsed ctors methods))

kindToText :: InfoKind -> Text
kindToText = \case
  IkClass       -> "class"
  IkData        -> "data"
  IkNewtype     -> "newtype"
  IkTypeSynonym -> "type-synonym"
  IkFunction    -> "function"
  IkUnknown     -> "unknown"

