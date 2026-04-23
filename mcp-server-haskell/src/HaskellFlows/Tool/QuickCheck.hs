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
    -- * Shared runtime-execution helper (Regression, Determinism)
  , runQuickCheckViaCabalRepl
    -- * Pure helpers exposed for unit tests
  , chooseStoreModule
  , isSimpleIdent
  ) where

import Control.Applicative ((<|>))
import Control.Exception (SomeException, try)
import Data.Aeson
import Data.Aeson.Types (parseEither)
import Data.Char (isAlpha, isAlphaNum)
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.Lazy as TL
import qualified Data.Text.Lazy.Encoding as TLE
import System.Timeout (timeout)

import qualified System.Process as Proc
import System.Exit (ExitCode (..))

import HaskellFlows.Data.PropertyStore (Store, save)
import HaskellFlows.Ghc.ApiSession
  ( GhcSession
  , gsProject
  , withGhcSession
  )
import HaskellFlows.Types (ProjectDir, unProjectDir)

import qualified Data.List.NonEmpty as NE
import GHC (parseName)
import GHC.Data.FastString (unpackFS)
import GHC.Types.Name (nameSrcSpan)
import GHC.Types.SrcLoc (RealSrcSpan, SrcSpan (..), srcSpanFile)
import HaskellFlows.Ghc.Sanitize
  ( CommandError (..)
  , sanitizeExpression
  )
import HaskellFlows.Mcp.Protocol
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
      -- Resolve the property's defining module via the GHC API.
      -- If the property is a bare identifier we can look up
      -- 'parseName + nameSrcSpan' and the resulting file path
      -- becomes authoritative; the caller hint ('md') is treated
      -- as a fallback for lambda/expression properties where
      -- parseName would legitimately fail.
      --
      -- This restores the pre-Wave-5 behaviour where
      -- 'ghci_quickcheck prop_x module="src/Foo.hs"' — a common
      -- caller mistake when the property actually lives in the
      -- test suite — still persisted test/Spec.hs in the
      -- regression store, so replay loaded the right scope.
      resolved <- resolvePropertyModule ghcSess safe
      let loadHint = resolved <|> md
      mRes <- timeout quickCheckTimeoutMicros $
        try $ runQuickCheckViaCabalRepl (gsProject ghcSess) loadHint safe
      case mRes of
        Nothing ->
          pure (renderResult
            (QcException prop "timeout: property exceeded 30s budget"))
        Just (Left (ex :: SomeException)) ->
          pure (renderResult (QcException prop (T.pack (show ex))))
        Just (Right out) -> do
          let qr = parseQuickCheckOutput prop out
          case qr of
            QcPassed _ _ -> save store prop loadHint
            _            -> pure ()
          pure (renderResult qr)

-- | Resolve a property name to the source file it was defined in
-- by asking the GHC API. Returns 'Nothing' when the input isn't a
-- simple identifier, when parseName can't resolve it in the
-- current interactive scope, or when the resulting Name has no
-- RealSrcSpan (e.g. a name from a pre-built package). Callers then
-- fall back to the user-provided hint.
resolvePropertyModule :: GhcSession -> Text -> IO (Maybe Text)
resolvePropertyModule ghcSess nm
  | not (isSimpleIdent nm) = pure Nothing
  | otherwise = do
      eRes <- try @SomeException $ withGhcSession ghcSess $ do
        names <- parseName (T.unpack nm)
        case NE.toList names of
          []     -> pure Nothing
          (n:_)  -> pure (fileFromSpan (nameSrcSpan n))
      pure $ case eRes of
        Left _           -> Nothing
        Right (Just fp)  -> Just (T.pack fp)
        Right Nothing    -> Nothing
  where
    fileFromSpan :: SrcSpan -> Maybe FilePath
    fileFromSpan = \case
      RealSrcSpan s _ -> Just (unpackFS (srcSpanFile (s :: RealSrcSpan)))
      UnhelpfulSpan _ -> Nothing

-- | Run a QuickCheck property via @cabal v2-repl@ on the project's
-- test-suite target. Returns the raw @Result.output@ text so the
-- existing 'parseQuickCheckOutput' can consume it unchanged.
--
-- The statement we pipe into repl is the same shape as the
-- in-process Wave-3 one (show Result.output) — the only difference
-- is the execution vehicle. cabal invokes ghci with the correct
-- per-stanza flags, resolves deps (QuickCheck included) natively,
-- and returns clean.
runQuickCheckViaCabalRepl :: ProjectDir -> Maybe Text -> Text -> IO Text
runQuickCheckViaCabalRepl pd mModule safeProp = do
  let loadDirective = case mModule of
        Just modPath | not (T.null modPath) ->
          [":load " <> T.unpack modPath]
        _ -> []
      input = unlines $
        loadDirective <>
        [ "import Test.QuickCheck"
        -- Record-update with unqualified field names — GHC 9.12
        -- panics on the fully-qualified @Test.QuickCheck.chatty@
        -- variant inside a record update.
        , "let qcArgs = stdArgs { chatty = False }"
        , "r <- quickCheckWithResult qcArgs (" <> T.unpack safeProp <> ")"
        , "putStrLn \"__QC_OUTPUT_START__\""
        , "putStr (output r)"
        , "putStrLn \"__QC_OUTPUT_END__\""
        , ":q"
        ]
      -- Target @all@ by default. The repl loads the library +
      -- its exposed modules, so :load against a src/ file brings
      -- the user's definitions into scope without needing to
      -- know the module name.
      cp = (Proc.proc "cabal"
             [ "v2-repl", "all"
             , "--build-depends=QuickCheck"
             , "-v0"
             ])
             { Proc.cwd     = Just (unProjectDir pd)
             , Proc.std_in  = Proc.CreatePipe
             , Proc.std_out = Proc.CreatePipe
             , Proc.std_err = Proc.CreatePipe
             }
  (ec, outStr, _errStr) <- Proc.readCreateProcessWithExitCode cp input
  case ec of
    ExitSuccess    -> pure (extractQcOutput (T.pack outStr))
    ExitFailure _c -> pure (T.pack outStr)

-- | Slice the chatty output between our sentinel markers. The
-- rest of cabal's chatter (ghci prompt, module-load lines,
-- "Leaving GHCi", …) is discarded; only the QuickCheck formatter's
-- text reaches the parser.
extractQcOutput :: Text -> Text
extractQcOutput full =
  let (_, afterStart) = T.breakOn "__QC_OUTPUT_START__" full
      body            = T.drop (T.length "__QC_OUTPUT_START__") afterStart
      (captured, _)   = T.breakOn "__QC_OUTPUT_END__" body
  in T.strip captured


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
