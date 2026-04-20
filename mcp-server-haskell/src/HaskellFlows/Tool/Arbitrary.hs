-- | @ghci_arbitrary@ — generate a template 'Arbitrary' instance for a
-- user-defined data type.
--
-- Mirrors @mcp-server/src/tools/arbitrary.ts@ at its essential level.
-- Given a type name, the tool:
--
-- 1. queries GHCi for @:i \<type\>@ to get the declaration,
-- 2. parses out the constructors and their argument counts,
-- 3. emits an @instance Arbitrary T where arbitrary = oneof [...]@
--    template that the agent can paste into the source file.
--
-- Deliberately does NOT write to disk. Arbitrary generation is
-- best-effort (see the caveats in 'renderTemplate'); letting the agent
-- review + paste preserves the auditing loop and keeps the tool
-- idempotent.
--
-- Security: the type-name argument is routed through
-- 'sanitizeExpression' (the same boundary used for @:t@/@:i@/@:eval@).
module HaskellFlows.Tool.Arbitrary
  ( descriptor
  , handle
  , ArbitraryArgs (..)
  , renderTemplate
  , parseConstructors
  , Constructor (..)
  ) where

import Data.Aeson
import Data.Aeson.Types (parseEither)
import Data.Maybe (mapMaybe)
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.Lazy as TL
import qualified Data.Text.Lazy.Encoding as TLE

import HaskellFlows.Ghci.Session
import HaskellFlows.Mcp.Protocol
import HaskellFlows.Parser.Type (isOutOfScope)

descriptor :: ToolDescriptor
descriptor =
  ToolDescriptor
    { tdName        = "ghci_arbitrary"
    , tdDescription =
        "Generate a QuickCheck Arbitrary instance template for a user-defined "
          <> "data type. Returns the instance text for the agent to paste — does "
          <> "not modify files. Complex types (GADTs, existentials, constrained "
          <> "constructors) may need hand-editing after paste."
    , tdInputSchema =
        object
          [ "type"       .= ("object" :: Text)
          , "properties" .= object
              [ "type_name" .= object
                  [ "type"        .= ("string" :: Text)
                  , "description" .=
                      ("Name of the data/newtype to derive Arbitrary for. \
                       \Example: \"Expr\", \"Command\", \"Status\"." :: Text)
                  ]
              ]
          , "required"             .= ["type_name" :: Text]
          , "additionalProperties" .= False
          ]
    }

newtype ArbitraryArgs = ArbitraryArgs
  { aaTypeName :: Text
  }
  deriving stock (Show)

instance FromJSON ArbitraryArgs where
  parseJSON = withObject "ArbitraryArgs" $ \o ->
    ArbitraryArgs <$> o .: "type_name"

handle :: Session -> Value -> IO ToolResult
handle sess rawArgs = case parseEither parseJSON rawArgs of
  Left parseError ->
    pure (errorResult (T.pack ("Invalid arguments: " <> parseError)))
  Right (ArbitraryArgs tname) -> do
    res <- infoOf sess tname
    case res of
      Left cmdErr -> pure (errorResult (formatCommandError cmdErr))
      Right gr
        | not (grSuccess gr)         -> pure (errorResult (grOutput gr))
        | isOutOfScope (grOutput gr) -> pure (errorResult (grOutput gr))
        | otherwise ->
            case parseConstructors (grOutput gr) of
              []    -> pure (errorResult
                        ( "No constructors parsed for '" <> tname
                        <> "'. It may be a GADT, typeclass, or type synonym — "
                        <> "those need a hand-written Arbitrary." ))
              ctors -> pure (successResult tname ctors (renderTemplate tname ctors))

--------------------------------------------------------------------------------
-- constructor parsing
--------------------------------------------------------------------------------

-- | A data constructor extracted from @:i@ output.
--
-- Argument types are kept opaque as free-form 'Text' — we don't need
-- parsed types to emit the template (we only count them for <$>/<*>).
data Constructor = Constructor
  { cName :: !Text
  , cArgs :: ![Text]
  }
  deriving stock (Eq, Show)

-- | Parse @:i@ output for a data/newtype and return its constructors.
--
-- Handles both the inline form (@data T = A | B Int@) and the multi-line
-- form that GHCi prints for wider declarations (each @|@ on its own
-- line). Everything after the first @-- Defined@ comment is ignored.
--
-- Only fires on declarations starting with @data@ or @newtype@ — a
-- @type Alias = Int@ synonym also contains @=@ but has no constructor
-- list, so we return @[]@ and let the tool layer explain to the agent.
parseConstructors :: Text -> [Constructor]
parseConstructors out =
  let allLns   = T.lines out
      -- GHC 9.x prepends a kind signature line (e.g. @type Run :: * -> *@)
      -- before the @data@ declaration. Drop everything up to the first
      -- line that actually opens with @data @ / @newtype @ — the
      -- constructor list is there.
      declLns  = dropWhile (not . isDataDeclLine) allLns
      trimmed  = T.unlines (takeWhile (not . isDefinedComment) declLns)
      -- Collapse newline+indent runs so inline and multiline forms
      -- converge on a single "= A | B ..." string.
      one      = T.strip (T.replace "\n" " " trimmed)
  in if hasCtorHeader one
       then mapMaybe parseCtorText (splitOnPipe (dropDataHeader one))
       else []

-- | Is @ln@ the @data Foo@ or @newtype Bar@ declaration line (after
-- optional leading whitespace)?
isDataDeclLine :: Text -> Bool
isDataDeclLine ln =
  let s = T.stripStart ln
  in "data "    `T.isPrefixOf` s
  || "newtype " `T.isPrefixOf` s

-- | Does the declaration open with @data @ or @newtype @?
-- The trailing space guards against accidentally matching @dataX@.
hasCtorHeader :: Text -> Bool
hasCtorHeader t = "data "    `T.isPrefixOf` t
               || "newtype " `T.isPrefixOf` t

isDefinedComment :: Text -> Bool
isDefinedComment l = "-- Defined" `T.isInfixOf` l || "\t-- Defined" `T.isInfixOf` l

-- | Drop everything up to and including the @=@ that opens the
-- constructor list. If no @=@ is present this was a type synonym or
-- class and we return an empty string so the caller reports no ctors.
dropDataHeader :: Text -> Text
dropDataHeader t =
  case T.breakOn "=" t of
    (_, rest) | T.null rest -> ""
    (_, rest)               -> T.strip (T.drop 1 rest)

-- | Split on @|@ but only at the top level. Nested @(|)@ inside
-- parameter types would break a naive split; constructors rarely carry
-- those, but we at least skip @|@ inside parens to be conservative.
splitOnPipe :: Text -> [Text]
splitOnPipe = go 0 []
  where
    go :: Int -> String -> Text -> [Text]
    go depth acc t = case T.uncons t of
      Nothing      -> [T.pack (reverse acc)]
      Just ('(', rest) -> go (depth + 1) ('(':acc) rest
      Just (')', rest) -> go (max 0 (depth - 1)) (')':acc) rest
      Just ('|', rest)
        | depth == 0 -> T.pack (reverse acc) : go 0 [] rest
      Just (c, rest) -> go depth (c:acc) rest

-- | Parse one @Ctor arg1 arg2@ fragment into a 'Constructor'.
--
-- Argument tokenisation is space-separated with parentheses AND
-- braces preserved as single groups. Records, strict fields
-- (@!Int@), and kind annotations survive intact — we then expand a
-- record block's internal fields so the <$>/<*> template stays
-- arity-correct.
parseCtorText :: Text -> Maybe Constructor
parseCtorText raw =
  case groupTokens (T.strip raw) of
    []     -> Nothing
    (n:xs)
      | T.null n  -> Nothing
      | otherwise ->
          Just Constructor
            { cName = n
            , cArgs = normaliseArgs xs
            }
  where
    -- Record form: @Ctor {f1 :: T1, f2 :: T2}@ groups to
    -- @[\"Ctor\", \"{f1 :: T1, f2 :: T2}\"]@. Expand to one synthetic
    -- \"arbitrary\"-slot per record field so the template emits the
    -- right number of @<*>@s. Content of the slot is a literal; the
    -- template only ever counts the list length.
    normaliseArgs :: [Text] -> [Text]
    normaliseArgs [single]
      | Just n' <- recordFieldCount single
          = replicate n' "arbitrary"
    normaliseArgs xs = xs

-- | If @tok@ is a record block @{f1 :: T1, f2 :: T2, ...}@, return the
-- field count. Commas inside nested parens or braces do not count.
recordFieldCount :: Text -> Maybe Int
recordFieldCount t
  | "{" `T.isPrefixOf` t && "}" `T.isSuffixOf` t =
      let inner = T.init (T.tail t)
      in if T.null (T.strip inner)
           then Just 0
           else Just (length (splitTopLevelCommas inner))
  | otherwise = Nothing

-- | Whitespace-split with parenthesised AND brace groups preserved
-- as one token.
groupTokens :: Text -> [Text]
groupTokens = go 0 [] []
  where
    go :: Int -> String -> [Text] -> Text -> [Text]
    go depth curr acc t = case T.uncons t of
      Nothing -> reverse (flush curr acc)
      Just (c, rest)
        | c == '(' || c == '{' -> go (depth + 1) (c:curr) acc rest
        | c == ')' || c == '}' -> go (max 0 (depth - 1)) (c:curr) acc rest
        | c == ' ' && depth == 0 ->
            go depth [] (flush curr acc) rest
        | otherwise -> go depth (c:curr) acc rest

    flush [] acc = acc
    flush xs acc = T.pack (reverse xs) : acc

-- | Split on commas at depth 0, ignoring those inside nested parens
-- or braces. Used to count record fields.
splitTopLevelCommas :: Text -> [Text]
splitTopLevelCommas = go 0 []
  where
    go :: Int -> String -> Text -> [Text]
    go depth acc t = case T.uncons t of
      Nothing -> [T.pack (reverse acc)]
      Just (c, rest)
        | c == '(' || c == '{' -> go (depth + 1) (c:acc) rest
        | c == ')' || c == '}' -> go (max 0 (depth - 1)) (c:acc) rest
        | c == ',' && depth == 0 ->
            T.pack (reverse acc) : go 0 [] rest
        | otherwise -> go depth (c:acc) rest

--------------------------------------------------------------------------------
-- template rendering
--------------------------------------------------------------------------------

-- | Produce a compilable @instance Arbitrary T where arbitrary = oneof [...]@
-- snippet. Uses 'pure' for nullary constructors and an @<$> … <*> …@
-- chain for the rest. Does not emit @shrink@ — the default is fine and
-- the agent can extend by hand if needed.
--
-- Known limitations (surfaced in the tool description so the agent sees
-- them upfront):
--
-- * Does not look at argument types — everything is @arbitrary@,
--   leaning on type inference to pick the right instance.
-- * Does not handle polymorphic types @T a b@; the generated instance
--   will need an explicit @instance (Arbitrary a, Arbitrary b) => @
--   context that the agent adds.
renderTemplate :: Text -> [Constructor] -> Text
renderTemplate typeName ctors =
  T.unlines $
    [ "instance Arbitrary " <> typeName <> " where"
    , "  arbitrary = oneof"
    ]
    <> zipWith renderBranch [0 :: Int ..] ctors
    <> [ "    ]" ]
  where
    renderBranch i c =
      let prefix = if i == 0 then "    [ " else "    , "
          nArgs  = length (cArgs c)
      in  prefix <> case nArgs of
            0 -> "pure " <> cName c
            1 -> cName c <> " <$> arbitrary"
            n -> cName c
                 <> " <$> arbitrary"
                 <> T.concat (replicate (n - 1) " <*> arbitrary")

--------------------------------------------------------------------------------
-- response shaping
--------------------------------------------------------------------------------

successResult :: Text -> [Constructor] -> Text -> ToolResult
successResult typeName ctors tmpl =
  let payload =
        object
          [ "success"      .= True
          , "type_name"    .= typeName
          , "constructors" .= map renderCtor ctors
          , "template"     .= tmpl
          , "hint"         .= ( "Paste the template into the module that \
                               \defines '" <> typeName <> "'. If the type is \
                               \polymorphic, add an Arbitrary constraint on \
                               \each type variable." :: Text )
          ]
  in ToolResult
       { trContent = [ TextContent (encodeUtf8Text payload) ]
       , trIsError = False
       }

renderCtor :: Constructor -> Value
renderCtor c =
  object
    [ "name"  .= cName c
    , "arity" .= length (cArgs c)
    , "args"  .= cArgs c
    ]

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
  ContainsNewline  -> "type_name must be a single line (no newline characters)"
  ContainsSentinel -> "type_name contains the internal framing sentinel and was rejected"
  EmptyInput       -> "type_name is empty"

encodeUtf8Text :: Value -> Text
encodeUtf8Text = TL.toStrict . TLE.decodeUtf8 . encode
