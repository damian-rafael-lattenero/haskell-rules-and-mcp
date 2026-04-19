-- | Parsers for the output of GHCi's @:t@ (type) and @:i@ (info) commands.
--
-- Parallel to @mcp-server/src/parsers/type-parser.ts@. Kept deliberately
-- small; we only extract the shape the agent actually acts on. Richer
-- structure (class hierarchies, instance context) can come later once a
-- consumer needs it.
--
-- Out-of-scope detection is exposed as a small helper so the tool layer
-- can treat deferred-type-error output as failure even when GHCi succeeds
-- textually.
module HaskellFlows.Parser.Type
  ( ParsedType (..)
  , ParsedInfo (..)
  , InfoKind (..)
  , parseTypeOutput
  , parseInfoOutput
  , isOutOfScope
  ) where

import Data.Maybe (mapMaybe)
import Data.Text (Text)
import qualified Data.Text as T

-- | A parsed @expr :: ty@ pair.
data ParsedType = ParsedType
  { ptExpression :: !Text
  , ptType       :: !Text
  }
  deriving stock (Eq, Show)

-- | Best-effort parse of a @:t@ response.
--
-- GHCi usually emits one of:
--
-- > map (+1) :: Num b => [b] -> [b]
--
-- or, for multi-line types:
--
-- > foldr
-- >   :: Foldable t => (a -> b -> b) -> b -> t a -> b
--
-- We split on the first @::@, trim, and collapse internal whitespace in
-- the type half so the agent gets a single line.
parseTypeOutput :: Text -> Maybe ParsedType
parseTypeOutput raw =
  let trimmed = T.strip raw
  in case T.breakOn "::" trimmed of
       (_, rest) | T.null rest -> Nothing
       (lhs, rest) ->
         let exprPart = T.strip lhs
             tyPart   = T.strip (T.drop 2 rest)
         in if T.null exprPart || T.null tyPart
              then Nothing
              else Just ParsedType
                     { ptExpression = exprPart
                     , ptType       = collapseWhitespace tyPart
                     }

-- | GHCi with @-fdefer-type-errors@ resolves unknown names to a fresh type
-- variable instead of failing outright. We surface that as an error to the
-- tool layer — otherwise the agent sees a bogus polymorphic type.
isOutOfScope :: Text -> Bool
isOutOfScope out =
  any (`T.isInfixOf` out)
    [ "deferred-out-of-scope-variables"
    , "Variable not in scope"
    , "Not in scope"
    ]

-- | Coarse classification of what @:i@ told us about a name.
data InfoKind
  = IkClass
  | IkData
  | IkNewtype
  | IkTypeSynonym
  | IkFunction
  | IkUnknown
  deriving stock (Eq, Show)

-- | Structured view of the @:i@ output.
data ParsedInfo = ParsedInfo
  { piKind       :: !InfoKind
  , piName       :: !Text
  , piDefinition :: !Text
  , piInstances  :: ![Text]
  }
  deriving stock (Eq, Show)

-- | Best-effort parse of @:i@ output. Mirrors the TS
-- @parseInfoOutput@ shape but returns a proper enum instead of a string.
parseInfoOutput :: Text -> ParsedInfo
parseInfoOutput raw =
  let definition = T.strip raw
      lns        = T.lines definition
      firstLine  = case lns of { (l:_) -> l; _ -> "" }
      kind       = classify firstLine lns
      name       = extractName firstLine
      insts      = mapMaybe instanceLine lns
  in ParsedInfo
       { piKind       = kind
       , piName       = name
       , piDefinition = definition
       , piInstances  = insts
       }

--------------------------------------------------------------------------------
-- internals
--------------------------------------------------------------------------------

classify :: Text -> [Text] -> InfoKind
classify firstLine lns
  | "class "     `T.isPrefixOf` firstLine = IkClass
  | "data "      `T.isPrefixOf` firstLine = IkData
  | "newtype "   `T.isPrefixOf` firstLine = IkNewtype
  | "type role " `T.isPrefixOf` firstLine = refine lns
  | "type "      `T.isPrefixOf` firstLine =
      -- Disambiguate "type X :: Kind" kind-annotation from genuine
      -- "type X = ..." synonyms by scanning subsequent lines.
      if " :: " `T.isInfixOf` firstLine then refine lns else IkTypeSynonym
  | " :: "       `T.isInfixOf` firstLine  = IkFunction
  | otherwise                             = IkUnknown

-- | Scan the tail of the output to decide what the actual definition kind
-- is when the first line was just a kind/role annotation.
refine :: [Text] -> InfoKind
refine []       = IkUnknown
refine (_:rest) = go rest
  where
    go [] = IkUnknown
    go (l:ls)
      | "data "    `T.isPrefixOf` stripped = IkData
      | "newtype " `T.isPrefixOf` stripped = IkNewtype
      | "class "   `T.isPrefixOf` stripped = IkClass
      | "type "    `T.isPrefixOf` stripped && " = " `T.isInfixOf` stripped
          = IkTypeSynonym
      | otherwise = go ls
      where
        stripped = T.stripStart l

-- | Pull the head identifier of the first line for display — good enough
-- for classes, data/newtype, type synonyms and functions.
extractName :: Text -> Text
extractName firstLine =
  case T.words firstLine of
    ("type" : "role" : n : _) -> n
    ("class" : n : _)         -> dropContext n
    ("data" : n : _)          -> dropContext n
    ("newtype" : n : _)       -> dropContext n
    ("type" : n : _)          -> n
    (n : "::" : _)            -> n
    (n : _)                   -> n
    []                        -> ""
  where
    -- Drop a trailing context parenthesis like "(Eq a) =>".
    dropContext n
      | "(" `T.isPrefixOf` n = n
      | otherwise            = n

instanceLine :: Text -> Maybe Text
instanceLine l =
  let stripped = T.stripStart l
  in if "instance " `T.isPrefixOf` stripped
       then Just (T.strip l)
       else Nothing

-- | Replace runs of whitespace (including newlines) with a single space.
collapseWhitespace :: Text -> Text
collapseWhitespace = T.unwords . T.words
