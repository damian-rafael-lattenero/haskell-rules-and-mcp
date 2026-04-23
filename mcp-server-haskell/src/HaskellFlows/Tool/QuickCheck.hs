-- | @ghci_quickcheck@ — Wave-3 full in-process.
--
-- Runs a QuickCheck property against the project using the GHC API's
-- @compileExpr@ + @unsafeCoerce@ path ('evalIOString'). No more
-- subprocess ghci, no more chatty-stdout capture — the property is
-- compiled in-process under the relevant stanza's flags and its
-- @Result.output@ string is parsed by the existing
-- 'parseQuickCheckOutput' (the formatting matches GHCi's exactly
-- because we ask QuickCheck for the same output).
--
-- On success the property expression + module are persisted to the
-- property store so @ghci_regression@ can replay it later.
module HaskellFlows.Tool.QuickCheck
  ( descriptor
  , handle
  , QuickCheckArgs (..)
    -- * Pure helpers exposed for unit tests
  , chooseStoreModule
  , isSimpleIdent
  ) where

import Control.Exception (SomeException, try)
import Data.Aeson
import Data.Aeson.Types (parseEither)
import Data.Char (isAlpha, isAlphaNum)
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.Lazy as TL
import qualified Data.Text.Lazy.Encoding as TLE
import System.Timeout (timeout)

import GHC
  ( Ghc
  , InteractiveImport (IIDecl)
  , getContext
  , mkModuleName
  , setContext
  , simpleImportDecl
  )

import HaskellFlows.Data.PropertyStore (Store, save)
import HaskellFlows.Ghc.ApiSession
  ( GhcSession
  , evalIOString
  , firstTestSuiteOrLibrary
  , loadForTarget
  , LoadFlavour (..)
  , withGhcSession
  )
import HaskellFlows.Ghc.Sanitize
  ( CommandError (..)
  , sanitizeExpression
  )
import HaskellFlows.Mcp.Protocol
import HaskellFlows.Parser.Error (GhcError)
import HaskellFlows.Parser.QuickCheck

descriptor :: ToolDescriptor
descriptor =
  ToolDescriptor
    { tdName        = "ghci_quickcheck"
    , tdDescription =
        "Run a QuickCheck property against the current session. "
          <> "The property is passed directly to quickCheckWithResult, "
          <> "so it must be a value of type Testable (e.g. "
          <> "`\\x -> reverse (reverse x) == x`). Returns structured "
          <> "pass/fail/gave-up/exception output."
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

-- | Runtime ceiling for a single quickCheck invocation. Mirrors the
-- 30 s budget the legacy subprocess path used. Properties that loop
-- forever or expand exponentially hit this and surface as a
-- QcException with an explicit timeout message.
quickCheckTimeoutMicros :: Int
quickCheckTimeoutMicros = 30_000_000

handle :: Store -> GhcSession -> Value -> IO ToolResult
handle store ghcSess rawArgs = case parseEither parseJSON rawArgs of
  Left parseError ->
    pure (errorResult (T.pack ("Invalid arguments: " <> parseError)))
  Right (QuickCheckArgs prop md) -> case sanitizeExpression prop of
    Left cmdErr -> pure (errorResult (formatCommandError cmdErr))
    Right safe -> do
      tgt <- firstTestSuiteOrLibrary ghcSess
      -- Prime the session under the test-suite stanza so Test.QuickCheck
      -- (and the project's own library modules) are resolvable when we
      -- compileExpr the property.
      eLoad <- try (loadForTarget ghcSess tgt Strict)
      case eLoad :: Either SomeException (Bool, [GhcError]) of
        Left ex -> pure (errorResult ("loadForTarget failed: " <> T.pack (show ex)))
        Right _ -> do
          let stmt = buildQuickCheckStatement (T.unpack safe)
          -- No withStanzaFlags wrap: loadForTarget above already
          -- left hsc_dflags with the correct stanza flags cached
          -- in the HscEnv. Re-applying setSessionDynFlags would
          -- reset the interactive context and nothing would be
          -- in scope for compileExpr.
          mRes <- timeout quickCheckTimeoutMicros $
            try $ withGhcSession ghcSess $ do
              -- loadForTarget sets the context to the loaded
              -- module graph, but 'Test.QuickCheck' is an external
              -- package (via -package-id QckChck-…) — not in the
              -- graph, so it isn't imported. Add it explicitly so
              -- compileExpr can resolve 'Test.QuickCheck.output' /
              -- 'Test.QuickCheck.quickCheckWithResult' /
              -- 'Test.QuickCheck.stdArgs' in the statement we build.
              ensureTestQuickCheckImported
              evalIOString stmt
          case mRes of
            Nothing -> pure (renderResult (QcException prop "timeout: property exceeded 30s budget"))
            Just (Left (ex :: SomeException)) ->
              pure (renderResult (QcException prop (T.pack (show ex))))
            Just (Right out) -> do
              let qr = parseQuickCheckOutput prop (T.pack out)
              case qr of
                QcPassed _ _ ->
                  save store prop md
                _            -> pure ()
              pure (renderResult qr)

-- | Add @import qualified Test.QuickCheck@ (or a plain @import
-- Test.QuickCheck@) to the session's interactive context, on top
-- of whatever 'setContext' the prior 'loadForTarget' installed.
-- Idempotent — calling it twice is harmless.
ensureTestQuickCheckImported :: Ghc ()
ensureTestQuickCheckImported = do
  ctx <- getContext
  let qcImport = IIDecl (simpleImportDecl (mkModuleName "Test.QuickCheck"))
  setContext (ctx <> [qcImport])

-- | The exact expression we feed to 'evalIOString'. Wraps the user
-- property with 'quickCheckWithResult' so we get back a 'Result'
-- whose 'output' string is parsable by 'parseQuickCheckOutput'.
--
-- We disable 'chatty' so QuickCheck does not also print to stdout —
-- that would interleave with the MCP JSON framing. 'output' still
-- contains the full chatty-style text because QuickCheck fills it
-- from its own formatter, independent of the stdout switch.
buildQuickCheckStatement :: String -> String
buildQuickCheckStatement safe =
  "fmap Test.QuickCheck.output "
    <> "(Test.QuickCheck.quickCheckWithResult "
    <> "(Test.QuickCheck.stdArgs { Test.QuickCheck.chatty = False }) "
    <> "(" <> safe <> "))"

--------------------------------------------------------------------------------
-- store-module resolution
--------------------------------------------------------------------------------

-- | Pure selector: given the property text, the caller's hint, and
-- (optionally) the @:info@ output, pick which path to persist.
--
-- Wave-3 kept for unit-test compatibility; the Wave-3 'handle' uses
-- the caller hint verbatim — the @:info@ plumbing that sat on top of
-- the subprocess ghci isn't reintroduced here because the regression
-- store only uses the module to reload the right compile scope.
chooseStoreModule :: Text -> Maybe Text -> Maybe Text -> Maybe Text
chooseStoreModule _prop callerHint _mInfo = callerHint

-- | True iff @t@ parses as a single Haskell identifier (possibly
-- qualified with dots, e.g. @Spec.prop_x@).
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
