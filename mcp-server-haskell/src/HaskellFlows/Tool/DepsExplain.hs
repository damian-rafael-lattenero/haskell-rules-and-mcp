-- | Internal handler for the @explain@ branch of @ghc_deps@ (#63 + #94).
--
-- Phase 1 scope (MVP): parse a cabal solver dump and extract
-- the root conflict (deepest @rejecting:@) so the agent gets a
-- structured @conflict@ field instead of 200 lines of free text.
-- Candidate generation (bump-our-constraint / allow-newer /
-- Stackage LTS) is deferred to Phase 2.
--
-- Two input modes:
--
--   * @cabal_output@ supplied → parse it directly. Lets the
--     agent feed any prior dry-run output into the explainer
--     without re-running cabal.
--   * @cabal_output@ omitted → run @cabal v2-build all
--     --dry-run --enable-tests --enable-benchmarks@ in the
--     project root and parse its stderr.
--
-- Issue #94 Phase C retired the @ghc_deps_explain@ wire surface;
-- 'HaskellFlows.Tool.Deps' is the single externally-advertised tool.
-- This module's 'handle' is the implementation 'Deps.handle'
-- forwards to when @action="explain"@.
module HaskellFlows.Tool.DepsExplain
  ( handle
  , DepsExplainArgs (..)
    -- * Pure helpers (exported for unit tests)
  , Conflict (..)
  , Rejection (..)
  , parseSolverOutput
  , identifyRootCause
  , extractPackages
  ) where

import Control.Exception (SomeException, try)
import Data.Aeson
import Data.Aeson.Types (parseEither)
import Data.Char (isAsciiLower, isDigit)
import Data.List (nub)
import Data.Text (Text)
import qualified Data.Text as T
import System.Exit (ExitCode)
import qualified System.Process as Proc

import qualified HaskellFlows.Mcp.Envelope as Env
import HaskellFlows.Mcp.Protocol
import HaskellFlows.Mcp.ToolName (ToolName (..), toolNameText)
import HaskellFlows.Types (ProjectDir, unProjectDir)


newtype DepsExplainArgs = DepsExplainArgs
  { daCabalOutput :: Maybe Text
  }
  deriving stock (Show)

instance FromJSON DepsExplainArgs where
  parseJSON = withObject "DepsExplainArgs" $ \o ->
    DepsExplainArgs <$> o .:? "cabal_output"

handle :: ProjectDir -> Value -> IO ToolResult
handle pd rawArgs = case parseEither parseJSON rawArgs of
  Left err ->
    pure (Env.toolResponseToResult (Env.mkFailed
      ((Env.mkErrorEnvelope Env.MissingArg
          (T.pack ("Invalid arguments: " <> err)))
            { Env.eeCause = Just (T.pack err) })))
  Right args -> do
    output <- case daCabalOutput args of
      Just t  -> pure t
      Nothing -> runCabalDryRun pd
    pure (renderReport output (parseSolverOutput output))

--------------------------------------------------------------------------------
-- subprocess
--------------------------------------------------------------------------------

-- | Run @cabal v2-build all --dry-run --enable-tests --enable-benchmarks@
-- in the project root. Returns the merged @stderr@ + @stdout@ — the
-- solver writes to stderr in modern cabal but older versions emit
-- to stdout, and the parser ignores noise either way.
runCabalDryRun :: ProjectDir -> IO Text
runCabalDryRun pd = do
  let cp = (Proc.proc "cabal"
             [ "v2-build", "all"
             , "--dry-run"
             , "--enable-tests"
             , "--enable-benchmarks"
             ])
             { Proc.cwd     = Just (unProjectDir pd)
             , Proc.std_in  = Proc.NoStream
             , Proc.std_out = Proc.CreatePipe
             , Proc.std_err = Proc.CreatePipe
             }
  res <- try (Proc.readCreateProcessWithExitCode cp "")
          :: IO (Either SomeException (ExitCode, String, String))
  case res of
    Left e -> pure (T.pack ("cabal subprocess failed: " <> show e))
    Right (_, out, err) -> pure (T.pack (out <> "\n" <> err))

--------------------------------------------------------------------------------
-- parsing
--------------------------------------------------------------------------------

-- | One @rejecting: pkg-version (conflict: …)@ entry from the solver.
data Rejection = Rejection
  { rDepth   :: !Int   -- ^ indent depth from the @[__N]@ marker
  , rPackage :: !Text  -- ^ "pkg-version", e.g. "aeson-2.2.3.0"
  , rReason  :: !Text  -- ^ what's after \"conflict:\", trimmed
  }
  deriving stock (Eq, Show)

-- | The structured report the tool returns when it finds a
-- conflict in the input.
data Conflict = Conflict
  { cRoot      :: !Rejection
  , cPackages  :: ![Text]    -- ^ unique package names involved
  , cBackjumps :: !(Maybe Int)  -- ^ backjump limit if reported
  , cAll       :: ![Rejection]  -- ^ every rejection seen, in order
  }
  deriving stock (Eq, Show)

-- | Parse a solver dump. Returns 'Nothing' when the input contains
-- no @rejecting:@ lines (i.e. the build resolved cleanly or the
-- output is from a different cabal subcommand).
parseSolverOutput :: Text -> Maybe Conflict
parseSolverOutput txt =
  let rejections = mapMaybe' parseRejection (T.lines txt)
  in case rejections of
       [] -> Nothing
       _  -> Just Conflict
               { cRoot      = identifyRootCause rejections
               , cPackages  = extractPackages rejections
               , cBackjumps = parseBackjumps txt
               , cAll       = rejections
               }

-- | One line → maybe a rejection. Recognises the cabal-install
-- format @[__N] rejecting: pkg-version (conflict: …)@. Tolerant
-- of leading whitespace and the trailing ANSI colour codes some
-- terminals inject when run interactively.
parseRejection :: Text -> Maybe Rejection
parseRejection raw =
  let stripped = T.stripStart raw
  in case T.stripPrefix "[__" stripped of
       Nothing -> Nothing
       Just afterMarker ->
         let (depthTxt, rest) = T.breakOn "]" afterMarker
         in case T.unpack (T.strip depthTxt) of
              ds | all isDigit ds, not (null ds) ->
                let depth   = read ds
                    afterRb = T.stripStart (T.drop 1 rest)
                in case T.stripPrefix "rejecting:" afterRb of
                     Nothing -> Nothing
                     Just rj ->
                       let trimmed = T.stripStart rj
                           (pkg, parenAndAfter) =
                             T.breakOn " (conflict:" trimmed
                       in if T.null pkg
                            then Nothing
                            else
                              let reason = case T.stripPrefix " (conflict:"
                                              parenAndAfter of
                                    Just inside -> stripCloseParen
                                                    (T.stripStart inside)
                                    Nothing     -> ""
                              in Just Rejection
                                   { rDepth   = depth
                                   , rPackage = T.strip pkg
                                   , rReason  = reason
                                   }
              _  -> Nothing

stripCloseParen :: Text -> Text
stripCloseParen t =
  case T.unsnoc (T.stripEnd t) of
    Just (rest, ')') -> T.stripEnd rest
    _                -> t

-- | The root cause is the rejection at the GREATEST depth — that's
-- the one whose constraint the solver tripped on after exploring
-- everything shallower. Ties are broken by reading order so we
-- return the first deepest rejection.
identifyRootCause :: [Rejection] -> Rejection
identifyRootCause [r] = r
identifyRootCause (r0 : rs) = foldr deepest r0 rs
  where
    deepest r acc = if rDepth r > rDepth acc then r else acc
identifyRootCause [] =
  -- Defensive — callers gate on @null rejections@ before invoking
  -- 'identifyRootCause', so this branch is unreachable in practice.
  Rejection { rDepth = 0, rPackage = "", rReason = "" }

-- | Best-effort extraction of package names from the rejection
-- list. Keeps the lowercase @hyphen-name@ tokens and drops
-- version numbers / operators. Order: insertion order, deduped.
extractPackages :: [Rejection] -> [Text]
extractPackages rs =
  nub $ concatMap (\r -> [stripVersion (rPackage r)] <> hostNames (rReason r))
                  rs
  where
    stripVersion pkg =
      -- "aeson-2.2.3.0" → "aeson"; "lens" → "lens".
      case reverse (T.splitOn "-" pkg) of
        (verLast : nameRev)
          | not (T.null verLast)
          , isVersionLike verLast
          -> T.intercalate "-" (reverse nameRev)
        _ -> pkg
    isVersionLike t = case T.uncons t of
      Just (c, _) -> isDigit c
      Nothing     -> False
    -- Pull the first 1–2 lower-case identifier-ish tokens out of
    -- the conflict reason — typically "text >= 2.0 needed" mentions
    -- the second package.
    hostNames reason =
      take 2 [ tok | tok <- T.words reason
                   , isPkgIdent tok
                   ]
    isPkgIdent t = case T.uncons t of
      Just (c, rest) -> isAsciiLower c
                     && T.all (\ch -> isAsciiLower ch
                                   || isDigit ch
                                   || ch == '-' || ch == '_') rest
                     && T.length t >= 2
      Nothing -> False

-- | If the dump contains @backjump limit reached (currently N, …@,
-- pull out N. Helps the agent decide whether the conflict is
-- a depth-budget exhaustion vs a true unresolvable.
parseBackjumps :: Text -> Maybe Int
parseBackjumps txt =
  case T.breakOn "backjump limit reached (currently " txt of
    (_, after) | not (T.null after) ->
      let payload = T.drop (T.length "backjump limit reached (currently ") after
          numTxt  = T.takeWhile isDigit payload
      in if T.null numTxt then Nothing
         else case reads (T.unpack numTxt) of
                ((n, _) : _) -> Just n
                _            -> Nothing
    _ -> Nothing

-- | Total replacement for 'Data.Maybe.mapMaybe' to keep the import
-- list narrow.
mapMaybe' :: (a -> Maybe b) -> [a] -> [b]
mapMaybe' _ [] = []
mapMaybe' f (x : xs) = case f x of
  Just y  -> y : mapMaybe' f xs
  Nothing -> mapMaybe' f xs

--------------------------------------------------------------------------------
-- response shaping
--------------------------------------------------------------------------------

-- | Issue #90: zero-conflict case maps to status='no_match' (the
-- explainer asked \"is there a conflict?\" and the answer is no);
-- conflict found maps to status='ok' with the structured report.
renderReport :: Text -> Maybe Conflict -> ToolResult
renderReport _ Nothing =
  let payload = object
        [ "conflict" .= Null
        , "hint"     .= ( "No solver conflict detected in the supplied \
                          \cabal output. Either the build resolves \
                          \cleanly or the output came from a different \
                          \subcommand. Run 'cabal v2-build --dry-run' \
                          \to capture solver-shaped stderr and retry."
                          :: Text )
        ]
  in Env.toolResponseToResult (Env.mkNoMatch payload)
renderReport _ (Just c) =
  let root = cRoot c
      payload = object
        [ "conflict"  .= object
            [ "root_cause" .= object
                [ "package" .= rPackage root
                , "depth"   .= rDepth   root
                , "reason"  .= rReason  root
                ]
            , "involved_packages" .= cPackages c
            , "backjump_limit"    .= cBackjumps c
            , "rejection_count"   .= length (cAll c)
            ]
        , "candidates"  .= ([] :: [Value])
        , "hint"        .=
            ( "Phase 1 returns the structured conflict only. Use ghc_deps \
              \to bump the constraint that names the root_cause package, \
              \or add an 'allow-newer' line to cabal.project. Phase 2 will \
              \generate verified fix candidates automatically." :: Text )
        ]
  in Env.toolResponseToResult (Env.mkOk payload)
