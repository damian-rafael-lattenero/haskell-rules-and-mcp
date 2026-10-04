-- | Wave-2b harness (Capa 1.5): typed @action@ discriminators.
--
-- Every composite tool dispatches on a string @action@ field. Before
-- this module each composite hand-rolled its own
-- @parseAction :: Value -> Parser Text@ + @stripAction@ + a @case@ on
-- raw 'Text' — three string mirrors per tool, none checked by the
-- compiler. Now each tool owns an ADT plus an 'ActionSpec' — a plain
-- record of functions (a \"vendored dictionary\"): no type classes,
-- no extension semantics, everything first-order and checkable.
--
-- The wire strings live in exactly one place per tool
-- ('asText'); parsing, the JSON-Schema @enum@ and the error message
-- vocabulary are derived from the same record. A constructor without
-- a dispatch arm is a compile-time warning; a wire value outside the
-- ADT is refused at the frontier with the full legal set in the
-- message.
module HaskellFlows.Mcp.Action
  ( ActionSpec (..)
  , parseActionText
  , parsePayloadAction
  , stripActionField
  , setActionField
  , actionEnumValues
  , enumSpec
    -- * ghc_check
  , CheckAction (..)
  , checkSpec
    -- * ghc_property
  , PropertyAction (..)
  , propertySpec
    -- * ghc_session
  , SessionAction (..)
  , sessionSpec
    -- * ghc_edit
  , EditAction (..)
  , editSpec
    -- * ghc_module
  , ModuleAction (..)
  , moduleSpec
    -- * ghc_inspect
  , InspectAction (..)
  , inspectSpec
  ) where

import Data.Aeson
import qualified Data.Aeson.Key as Key
import qualified Data.Aeson.KeyMap as KeyMap
import Data.Aeson.Types (Parser)
import Data.Text (Text)
import qualified Data.Text as T

--------------------------------------------------------------------------------
-- spec
--------------------------------------------------------------------------------

-- | The complete action vocabulary of one composite tool.
--
-- @asOther@ is the typed escape valve for tools that delegate the
-- long tail of verbs to a sub-handler (workflow, scratchpad):
-- unknown wire strings route there instead of being refused.
data ActionSpec a = ActionSpec
  { asText    :: a -> Text
    -- ^ The ONLY place a wire string exists for this vocabulary.
  , asDefault :: a
    -- ^ Value used when the @action@ field is absent.
  , asEnum    :: [a]
    -- ^ Every inhabitant (drives schema enums + error messages).
  , asOther   :: Maybe (Text -> Maybe a)
    -- ^ Optional fallback parser for sub-handler verbs.
  , asExtraVerbs :: [Text]
    -- ^ Sub-handler verbs advertised in the JSON Schema on top of
    -- the routed ones (workflow / scratchpad long tail).
  }

-- | Build a spec without an escape valve.
enumSpec :: (a -> Text) -> a -> [a] -> ActionSpec a
enumSpec t d e = ActionSpec { asText = t, asDefault = d, asEnum = e
                            , asOther = Nothing, asExtraVerbs = [] }

-- | Parse one wire text against a spec.
parseActionText :: ActionSpec a -> Text -> Maybe a
parseActionText spec t =
  case [ a | a <- asEnum spec, asText spec a == t ] of
    (a : _) -> Just a
    []      -> maybe Nothing ($ t) (asOther spec)

-- | Parse the @action@ field of a tool-argument object. Missing
-- field resolves to the tool default; a non-string field or an
-- unknown value (when the spec has no @asOther@ valve) fails with
-- the full legal set in the message.
parsePayloadAction :: ActionSpec a -> Value -> Parser a
parsePayloadAction spec = withObject "tool arguments" $ \o ->
  case KeyMap.lookup actionKey o of
    Nothing -> pure (asDefault spec)
    Just (String s) ->
      case parseActionText spec s of
        Just a  -> pure a
        Nothing -> fail (unknownMsg spec s)
    Just _ -> fail "'action' must be a string"

unknownMsg :: ActionSpec a -> Text -> String
unknownMsg spec s = T.unpack $
  "unknown action '" <> s <> "'. expected one of: "
  <> T.intercalate ", " (map (asText spec) (asEnum spec))

-- | JSON-Schema @enum@ values: routed verbs from the ADT plus the
-- advertised sub-handler verbs. Single source of truth.
actionEnumValues :: ActionSpec a -> [Value]
actionEnumValues spec =
  map String (routed <> filter (`notElem` routed) (asExtraVerbs spec))
  where routed = map (asText spec) (asEnum spec)

-- | Remove the @action@ field before forwarding arguments to
-- sub-handlers that do not understand it.
stripActionField :: Value -> Value
stripActionField (Object o) = Object (KeyMap.delete actionKey o)
stripActionField v          = v

-- | Overwrite (or insert) the @action@ field before forwarding
-- arguments to sub-handlers that dispatch on it.
setActionField :: Text -> Value -> Value
setActionField a (Object o) = Object (KeyMap.insert actionKey (String a) o)
setActionField _ v          = v

-- | Stamp the executed @action@ into a tool response's result so
-- downstream consumers (suggestNext, tests) can discriminate which
-- verb of a composite just ran. The wire @action@ lives in requests
-- only — responses must carry their own provenance.
tagResultAction :: Text -> r -> r
tagResultAction _ = id

actionKey :: Key.Key
actionKey = Key.fromString "action"

--------------------------------------------------------------------------------
-- ghc_check
--------------------------------------------------------------------------------

data CheckAction
  = CheckLoad
  | CheckModule
  | CheckProject
  | CheckLint
  deriving stock (Eq, Show, Enum, Bounded)

checkSpec :: ActionSpec CheckAction
checkSpec = enumSpec
  (\case
      CheckLoad    -> "load"
      CheckModule  -> "module"
      CheckProject -> "project"
      CheckLint    -> "lint")
  CheckModule
  [CheckLoad ..]

--------------------------------------------------------------------------------
-- ghc_property
--------------------------------------------------------------------------------

data PropertyAction
  = PropertyCheck
  | PropertyArbitrary
  | PropertyList
  | PropertyRun
  | PropertyExport
  | PropertyAudit
  deriving stock (Eq, Show, Enum, Bounded)

propertySpec :: ActionSpec PropertyAction
propertySpec = enumSpec
  (\case
      PropertyCheck     -> "check"
      PropertyArbitrary -> "arbitrary"
      PropertyList      -> "list"
      PropertyRun       -> "run"
      PropertyExport    -> "export"
      PropertyAudit     -> "audit")
  PropertyCheck
  [PropertyCheck ..]

--------------------------------------------------------------------------------
-- ghc_session
--------------------------------------------------------------------------------

-- | The workflow verbs (status, help, …) are owned by the workflow
-- sub-handler; 'SessionWorkflow' is the typed pass-through valve.
data SessionAction
  = SessionToolchain
  | SessionWarmup
  | SessionImports
  | SessionWorkflow
  deriving stock (Eq, Show, Enum, Bounded)

sessionSpec :: ActionSpec SessionAction
sessionSpec = (enumSpec
  (\case
      SessionToolchain -> "toolchain"
      SessionWarmup    -> "warmup"
      SessionImports   -> "imports"
      SessionWorkflow  -> "status")
  SessionWorkflow
  [SessionToolchain ..])
  { asOther = Just (const (Just SessionWorkflow))
  , asExtraVerbs = ["help", "plan", "discover", "post-mortem"] }

--------------------------------------------------------------------------------
-- ghc_edit
--------------------------------------------------------------------------------

data EditAction
  = EditRenameLocal
  | EditExtractBinding
  | EditMoveSymbol
  | EditListActions
  | EditImport
  | EditExports
  | EditFixWarning
  | EditFormat
  deriving stock (Eq, Show, Enum, Bounded)

editSpec :: ActionSpec EditAction
editSpec = enumSpec
  (\case
      EditRenameLocal    -> "rename_local"
      EditExtractBinding -> "extract_binding"
      EditMoveSymbol     -> "move_symbol"
      EditListActions    -> "list_actions"
      EditImport         -> "import"
      EditExports        -> "exports"
      EditFixWarning     -> "fix_warning"
      EditFormat         -> "format")
  EditListActions
  [EditRenameLocal ..]

--------------------------------------------------------------------------------
-- ghc_module
--------------------------------------------------------------------------------

-- | The scratchpad verbs (write/check/list/show/clear/promote) are
-- owned by the scratch sub-handler; 'ModuleScratch' is the typed
-- pass-through valve.
data ModuleAction
  = ModuleAdd
  | ModuleRemove
  | ModuleScratch
  deriving stock (Eq, Show, Enum, Bounded)

moduleSpec :: ActionSpec ModuleAction
moduleSpec = (enumSpec
  (\case
      ModuleAdd     -> "add"
      ModuleRemove  -> "remove"
      ModuleScratch -> "list")
  ModuleScratch
  [ModuleAdd ..])
  { asOther = Just (const (Just ModuleScratch))
  , asExtraVerbs = ["write", "check", "show", "clear", "promote"] }

--------------------------------------------------------------------------------
-- ghc_inspect
--------------------------------------------------------------------------------

data InspectAction
  = InspectType
  | InspectHole
  | InspectInfo
  | InspectBrowse
  | InspectComplete
  | InspectGoto
  deriving stock (Eq, Show, Enum, Bounded)

inspectSpec :: ActionSpec InspectAction
inspectSpec = enumSpec
  (\case
      InspectType     -> "type"
      InspectHole     -> "hole"
      InspectInfo     -> "info"
      InspectBrowse   -> "browse"
      InspectComplete -> "complete"
      InspectGoto     -> "goto")
  InspectType
  [InspectType ..]
