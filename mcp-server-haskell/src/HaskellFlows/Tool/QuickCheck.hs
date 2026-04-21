-- | @ghci_quickcheck@ — run a QuickCheck property in the persistent GHCi.
--
-- Phase-4 port of @mcp-server/src/tools/quickcheck.ts@. Scope deliberately
-- narrower than the TS version for this first pass:
--
-- * No law-suggestion engine ('suggestFunctionProperties').
-- * No ambiguous-type-variable auto-hint.
-- * No persistence to the property store (regression will come when the
--   store itself is ported).
-- * No auto @load_all@ of the project — the agent is expected to call
--   'ghci_load' first.
--
-- What it does cover is the high-value path: take a property expression,
-- ensure @Test.QuickCheck@ is in scope, invoke @quickCheck@, and translate
-- the four observable QuickCheck states into a structured JSON payload.
--
-- Boundary safety is inherited from 'runProperty', which routes the
-- expression through 'sanitizeExpression' before any bytes reach GHCi.
module HaskellFlows.Tool.QuickCheck
  ( descriptor
  , handle
  , QuickCheckArgs (..)
    -- * Pure helpers exposed for unit tests
  , chooseStoreModule
  , isSimpleIdent
  ) where

import Data.Aeson
import Data.Aeson.Types (parseEither)
import Data.Char (isAlpha, isAlphaNum)
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.Lazy as TL
import qualified Data.Text.Lazy.Encoding as TLE

import HaskellFlows.Data.PropertyStore (Store, save)
import HaskellFlows.Ghci.Session
import HaskellFlows.Mcp.Protocol
import HaskellFlows.Parser.QuickCheck
import HaskellFlows.Tool.Goto (Location (..), parseDefinedAt)

descriptor :: ToolDescriptor
descriptor =
  ToolDescriptor
    { tdName        = "ghci_quickcheck"
    , tdDescription =
        "Run a QuickCheck property against the current GHCi session. "
          <> "The property is passed directly to quickCheck, so it must be a "
          <> "value of type Testable (e.g. `\\x -> reverse (reverse x) == x`). "
          <> "Returns structured pass/fail/gave-up/exception output."
    , tdInputSchema =
        object
          [ "type"       .= ("object" :: Text)
          , "properties" .= object
              [ "property" .= object
                  [ "type"        .= ("string" :: Text)
                  , "description" .=
                      ("QuickCheck-testable property expression. Examples: \
                       \\"\\\\(xs :: [Int]) -> reverse (reverse xs) == xs\", \
                       \\"prop_idempotent\"" :: Text)
                  ]
              , "module" .= object
                  [ "type"        .= ("string" :: Text)
                  , "description" .=
                      ("Optional: module path to associate with the property \
                       \in the regression store. Lets ghci_regression reload \
                       \the right scope before re-running. Example: \
                       \\"src/Foo.hs\"." :: Text)
                  ]
              ]
          , "required"             .= ["property" :: Text]
          , "additionalProperties" .= False
          ]
    }

data QuickCheckArgs = QuickCheckArgs
  { qaProperty :: !Text
  , qaModule   :: !(Maybe Text)
  }
  deriving stock (Show)

instance FromJSON QuickCheckArgs where
  parseJSON = withObject "QuickCheckArgs" $ \o -> do
    prop <- o .: "property"
    md   <- o .:? "module"
    pure QuickCheckArgs { qaProperty = prop, qaModule = md }

-- | Run a property. On success the property text + module are written
-- to the 'Store' so 'ghci_regression' can replay it later. Failures are
-- not persisted — only properties that have passed at least once count
-- as trusted baseline.
--
-- Note on the persisted @module@ field: when the property expression
-- is a bare identifier (e.g. @prop_idempotent@), we ignore the
-- caller's @qaModule@ hint and instead query GHCi's @:info@ to find
-- where the identifier is actually defined. That way a caller who
-- passes the module of the /function under test/ (the natural choice)
-- doesn't accidentally make the replay fail when the property lives
-- in a different file (typically the test-suite's @Main@). The
-- caller hint is used verbatim only for anonymous-lambda properties,
-- where @:info@ cannot help.
handle :: Store -> Session -> Value -> IO ToolResult
handle store sess rawArgs = case parseEither parseJSON rawArgs of
  Left parseError ->
    pure (errorResult (T.pack ("Invalid arguments: " <> parseError)))
  Right (QuickCheckArgs prop md) -> do
    res <- runProperty sess prop
    case res of
      Left cmdErr -> pure (errorResult (formatCommandError cmdErr))
      Right gr    -> do
        let qr = parseQuickCheckOutput prop (grOutput gr)
        case qr of
          QcPassed _ _ -> do
            resolved <- resolvePropertyModule sess prop md
            save store prop resolved
          _            -> pure ()
        pure (renderResult qr)

--------------------------------------------------------------------------------
-- store-module resolution
--------------------------------------------------------------------------------

-- | Resolve the module path to persist alongside a passing property.
-- For simple identifiers we consult @:info@; for anonymous
-- expressions we fall back to whatever the caller provided.
resolvePropertyModule :: Session -> Text -> Maybe Text -> IO (Maybe Text)
resolvePropertyModule sess prop callerHint
  | isSimpleIdent prop = do
      r <- infoOf sess prop
      let mRaw = case r of
            Right gr | grSuccess gr -> Just (grOutput gr)
            _                        -> Nothing
      pure (chooseStoreModule prop callerHint mRaw)
  | otherwise =
      pure (chooseStoreModule prop callerHint Nothing)

-- | Pure selector: given the property text, the caller's hint, and
-- (optionally) the @:info@ output, pick which path to persist.
--
-- * Identifier + valid @:info@ with a file location → that file path.
-- * Anything else → the caller hint verbatim (may itself be
--   'Nothing').
--
-- Exposed for unit tests so the resolution rules can be pinned
-- without spawning a live GHCi.
chooseStoreModule :: Text -> Maybe Text -> Maybe Text -> Maybe Text
chooseStoreModule prop callerHint mInfo
  | isSimpleIdent prop
  , Just raw <- mInfo
  , Just (InFile path _ _) <- parseDefinedAt raw
  = Just path
  | otherwise
  = callerHint

-- | True iff @t@ parses as a single Haskell identifier (possibly
-- qualified with dots, e.g. @Spec.prop_x@). Used to decide whether
-- @:info t@ is a meaningful query — lambda expressions, operator
-- sections, and other compound forms are bounced so we never ask
-- GHCi for the "definition site" of @(\\x -> x + 1)@.
isSimpleIdent :: Text -> Bool
isSimpleIdent t = case T.uncons t of
  Nothing      -> False
  Just (c, cs) ->
    (isAlpha c || c == '_')
      && T.all validRest cs
  where
    validRest c = isAlphaNum c || c == '_' || c == '\'' || c == '.'

--------------------------------------------------------------------------------
-- response shaping
--------------------------------------------------------------------------------

renderResult :: QuickCheckResult -> ToolResult
renderResult qr =
  let payload = case qr of
        QcPassed p n ->
          object
            [ "success"  .= True
            , "state"    .= ("passed" :: Text)
            , "property" .= p
            , "passed"   .= n
            ]
        QcFailed p n shr cex ->
          object
            [ "success"        .= False
            , "state"          .= ("failed" :: Text)
            , "property"       .= p
            , "passed"         .= n
            , "shrinks"        .= shr
            , "counterexample" .= cex
            ]
        QcException p err ->
          object
            [ "success"  .= False
            , "state"    .= ("exception" :: Text)
            , "property" .= p
            , "error"    .= err
            ]
        QcGaveUp p n disc ->
          object
            [ "success"   .= False
            , "state"     .= ("gave_up" :: Text)
            , "property"  .= p
            , "passed"    .= n
            , "discarded" .= disc
            , "hint"      .= ( "Too many inputs rejected by precondition (==>). \
                              \Consider relaxing the precondition or writing a \
                              \custom generator." :: Text)
            ]
        QcUnparsed p raw ->
          object
            [ "success"  .= False
            , "state"    .= ("unparsed" :: Text)
            , "property" .= p
            , "raw"      .= raw
            ]
      isErr = case qr of
        QcPassed _ _ -> False
        _            -> True
  in ToolResult
       { trContent = [ TextContent (encodeUtf8Text payload) ]
       , trIsError = isErr
       }

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
  ContainsNewline  -> "property must be a single line (no newline characters)"
  ContainsSentinel -> "property contains the internal framing sentinel and was rejected"
  EmptyInput       -> "property is empty"
  InputTooLarge sz cap ->
    "property is too large (" <> T.pack (show sz) <> " chars, cap is "
      <> T.pack (show cap) <> ")"

encodeUtf8Text :: Value -> Text
encodeUtf8Text = TL.toStrict . TLE.decodeUtf8 . encode
