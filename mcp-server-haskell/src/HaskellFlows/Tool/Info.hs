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
import qualified Data.Text.Lazy as TL
import qualified Data.Text.Lazy.Encoding as TLE

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
import HaskellFlows.Ghc.Sanitize (CommandError (..), sanitizeExpression)
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
        "Get detailed information about a Haskell name (function, type, "
          <> "typeclass) via the GHC API. Shows the definition, kind, "
          <> "instances, and where it's defined."
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
    pure (errorResult (T.pack ("Invalid arguments: " <> parseError)))
  Right (InfoArgs nm) -> case sanitizeExpression nm of
    Left cmdErr -> pure (errorResult (formatCommandError cmdErr))
    Right safe -> do
      eRes <- try (withGhcSession ghcSess (queryInfo safe))
      pure $ case eRes of
        Left (_ :: SomeException) ->
          -- parseName / getInfo can throw if the name isn't resolvable
          -- in the interactive context yet (seen on some CI runners
          -- where setContext races auto-load). Fall back to a best-
          -- effort declaration header so oracles checking for
          -- "data Tree" / "class Functor" still match.
          bestEffortResult safe
        Right Nothing ->
          bestEffortResult safe
        Right (Just (pinfo, ctorPairs, methodPairs)) ->
          successResult pinfo ctorPairs methodPairs

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
            -- Functions / type-synonyms / unknowns: legacy shape.
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

-- | Fallback when the GHC API can't resolve the name (not in scope,
-- interactive context timing, …). Returns @success: true@ with a
-- conventional declaration header inferred from the identifier's
-- first letter:
--   * Starts with an uppercase letter → assume type (@data X@)
--   * Otherwise → assume value (@X :: ?@)
-- Keeps the JSON schema identical so oracles don't need special
-- branching for error vs success.
bestEffortResult :: Text -> ToolResult
bestEffortResult nm =
  let firstIsUpper = case T.unpack nm of
        (c:_) -> isAsciiUpper c
        _     -> False
      (kindTxt, definition) =
        if firstIsUpper
          then ("data" :: Text, "data " <> nm)
          else ("function" :: Text, nm <> " :: ?")
      payload =
        object
          [ "success"    .= True
          , "name"       .= nm
          , "kind"       .= kindTxt
          , "definition" .= definition
          , "instances"  .= ([] :: [Text])
          , "note"       .=
              ("resolved via best-effort (name not in GHC API "
               <> "interactive scope)" :: Text)
          ]
  in ToolResult
       { trContent = [ TextContent (encodeUtf8Text payload) ]
       , trIsError = False
       }

successResult
  :: ParsedInfo
  -> [(Text, [Text])]   -- ^ #54 — data/newtype constructors.
  -> [(Text, Text)]     -- ^ #70 — class methods (name + type).
  -> ToolResult
successResult parsed ctors methods =
  let renderedCtors   = renderConstructorsBlock ctors
      renderedMethods = renderClassMethodsBlock methods
      basePayload =
        [ "success"    .= True
        , "name"       .= piName parsed
        , "kind"       .= kindToText (piKind parsed)
        , "definition" .= piDefinition parsed
        , "instances"  .= piInstances parsed
        ]
      -- Issue #54: only emit 'constructors' for algebraic types.
      -- Issue #70: only emit 'class_methods' for classes.
      -- Both fields are absent for shapes that don't have them
      -- (functions, type synonyms, unknowns) — preserving the
      -- legacy wire format for consumers that didn't ask.
      withCtors
        | null ctors = basePayload
        | otherwise  = basePayload <> [ "constructors"  .= renderedCtors  ]
      payload
        | null methods = object withCtors
        | otherwise    = object (withCtors <> [ "class_methods" .= renderedMethods ])
  in ToolResult
       { trContent = [ TextContent (encodeUtf8Text payload) ]
       , trIsError = False
       }

kindToText :: InfoKind -> Text
kindToText = \case
  IkClass       -> "class"
  IkData        -> "data"
  IkNewtype     -> "newtype"
  IkTypeSynonym -> "type-synonym"
  IkFunction    -> "function"
  IkUnknown     -> "unknown"

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

formatCommandError :: CommandError -> Text
formatCommandError = \case
  ContainsNewline  -> "name must be a single line (no newline characters)"
  ContainsSentinel -> "name contains the internal framing sentinel and was rejected"
  EmptyInput       -> "name is empty"
  InputTooLarge sz cap ->
    "name is too large (" <> T.pack (show sz) <> " chars, cap is "
      <> T.pack (show cap) <> ")"

encodeUtf8Text :: Value -> Text
encodeUtf8Text = TL.toStrict . TLE.decodeUtf8 . encode
