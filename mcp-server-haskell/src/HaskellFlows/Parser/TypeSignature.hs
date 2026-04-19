-- | Structural parser for GHCi @:t@ / @:i@ type signatures.
--
-- Not a full Haskell type parser — we need only the subset needed by
-- 'HaskellFlows.Suggest.Rules' to pattern-match on shape: argument
-- arity, \"is the result the same type as the first argument?\",
-- \"is every argument the same type?\", type-class context, list/
-- tuple structure.
--
-- The trade-off: we hand-roll a recursive-descent parser (~120 LOC)
-- instead of importing @haskell-src-exts@ (~5 MB transitive closure).
-- The subset is well-defined and the compiler is the ultimate
-- correctness oracle — if we miss-parse a signature, the worst case
-- is that no rule fires, never a false-positive property.
module HaskellFlows.Parser.TypeSignature
  ( ParsedSig (..)
  , SigType (..)
  , parseSignature
  , isSameTypeThroughout
  , argCount
  , returnType
  ) where

import Data.Char (isAlphaNum, isUpper, isLower)
import Data.Text (Text)
import qualified Data.Text as T

-- | Parsed type signature: constraints before @=>@ plus a flat
-- argument list and a return type. The standalone @ParsedSig@ is
-- deliberately flat (arrow chain → list) — rules read it by arity,
-- not by tree shape.
data ParsedSig = ParsedSig
  { psConstraints :: ![Text]    -- ^ e.g. @[\"Eq a\", \"Show a\"]@
  , psArgs        :: ![SigType]
  , psReturn      :: !SigType
  }
  deriving stock (Eq, Show)

-- | Subset of Haskell types we care about for rule matching.
data SigType
  = TyVar  !Text                -- ^ @a@, @b@
  | TyCon  !Text                -- ^ @Int@, @String@
  | TyApp  !SigType ![SigType]  -- ^ @Maybe a@, @Map k v@
  | TyList !SigType             -- ^ @[a]@
  | TyTuple ![SigType]          -- ^ @(a, b)@
  | TyArrow !SigType !SigType   -- ^ higher-order arg @(a -> b)@
  deriving stock (Eq, Show)

-- | Parse @"Eq a => a -> a -> Bool"@ into a 'ParsedSig'. Returns
-- 'Nothing' on malformed input — callers treat that as \"no rules
-- match\" rather than propagating an error.
parseSignature :: Text -> Maybe ParsedSig
parseSignature raw = do
  let trimmed = collapseWs (T.strip raw)
  (constraints, body) <- splitConstraints trimmed
  let chain = splitTopArrow body
  case chain of
    []  -> Nothing
    [r] -> do
      rt <- parseType r
      pure ParsedSig
        { psConstraints = constraints
        , psArgs        = []
        , psReturn      = rt
        }
    xs  -> do
      args <- mapM parseType (init xs)
      rt   <- parseType (last xs)
      pure ParsedSig
        { psConstraints = constraints
        , psArgs        = args
        , psReturn      = rt
        }

--------------------------------------------------------------------------------
-- shape predicates (used by the rule catalog)
--------------------------------------------------------------------------------

-- | True iff the return type is identical to every argument type. A
-- fast structural equality check, used by rules like
-- \"Associative: @a -> a -> a@\".
isSameTypeThroughout :: ParsedSig -> Bool
isSameTypeThroughout ParsedSig{psArgs = [], psReturn = _} = False
isSameTypeThroughout ParsedSig{psArgs = args, psReturn = r} =
  all (sigTypeEq r) args

sigTypeEq :: SigType -> SigType -> Bool
sigTypeEq = (==)

argCount :: ParsedSig -> Int
argCount = length . psArgs

returnType :: ParsedSig -> SigType
returnType = psReturn

--------------------------------------------------------------------------------
-- internals
--------------------------------------------------------------------------------

collapseWs :: Text -> Text
collapseWs = T.unwords . T.words

-- | Split off a leading @(C1 a, C2 b) =>@ or @C a =>@ context.
-- Returns @(constraintList, rest)@ with the constraint list empty
-- if no @=>@ is present.
splitConstraints :: Text -> Maybe ([Text], Text)
splitConstraints t = case T.breakOn "=>" t of
  (_, rest) | T.null rest ->
    Just ([], t)
  (lhs, rest) ->
    let cs    = parseConstraints (T.strip lhs)
        body  = T.strip (T.drop 2 rest)
    in Just (cs, body)

-- | Parse @(Eq a, Show a)@ or @Eq a@ into the flat list of
-- constraint texts.
parseConstraints :: Text -> [Text]
parseConstraints raw
  | T.null raw = []
  | "(" `T.isPrefixOf` raw && ")" `T.isSuffixOf` raw =
      map T.strip (splitTopComma (T.init (T.tail raw)))
  | otherwise = [T.strip raw]

-- | Split on top-level @->@ (arrows inside parens stay grouped). The
-- arrow is right-associative in Haskell but from the rule engine's
-- perspective a left-to-right flat list of argument types is more
-- convenient.
splitTopArrow :: Text -> [Text]
splitTopArrow = splitTopOn "->"

splitTopComma :: Text -> [Text]
splitTopComma = splitTopOn ","

-- | Generic top-level-only split by literal @delim@. Depth is tracked
-- across @(@, @[@, @{@.
splitTopOn :: Text -> Text -> [Text]
splitTopOn delim t = go (0 :: Int) [] (T.unpack t) []
  where
    dlen = T.length delim

    go _ acc []     pending = reverse (flush acc pending)
    go d acc (c:cs) pending
      | c == '(' || c == '[' || c == '{' = go (d + 1) (c:acc) cs pending
      | c == ')' || c == ']' || c == '}' = go (max 0 (d - 1)) (c:acc) cs pending
      | d == 0
      , matchesDelim delim (c:cs) =
          go 0 [] (drop dlen (c:cs)) (flush acc pending)
      | otherwise = go d (c:acc) cs pending

    flush [] rest = rest
    flush acc rest = T.pack (reverse acc) : rest

    matchesDelim d s = take (T.length d) s == T.unpack d

--------------------------------------------------------------------------------
-- type parser
--------------------------------------------------------------------------------

-- | Parse a single type expression (no arrows at the top — splitting
-- on @->@ happens at the signature level).
parseType :: Text -> Maybe SigType
parseType raw
  | T.null stripped = Nothing
  -- Parenthesised: could be @(a)@ (grouping), @(a,b)@ (tuple), or
  -- @(a -> b)@ (arrow as higher-order arg).
  | "(" `T.isPrefixOf` stripped && ")" `T.isSuffixOf` stripped =
      let inner = T.init (T.tail stripped)
      in case splitTopComma inner of
           [one] -> case splitTopArrow one of
             [_] -> parseType one
             xs  -> do
               parts <- mapM parseType xs
               pure (foldr1 TyArrow parts)
           xs    -> do
             parts <- mapM parseType xs
             pure (TyTuple parts)
  -- List: @[T]@
  | "[" `T.isPrefixOf` stripped && "]" `T.isSuffixOf` stripped = do
      inner <- parseType (T.init (T.tail stripped))
      pure (TyList inner)
  | otherwise =
      parseAppOrAtom stripped
  where
    stripped = T.strip raw

-- | Parse an application head like @Maybe a@ or @Map k v@, or a bare
-- atom like @Int@ / @a@.
parseAppOrAtom :: Text -> Maybe SigType
parseAppOrAtom t =
  case splitTopSpace t of
    []       -> Nothing
    [one]    -> Just (toAtom one)
    (h:rest) -> do
      args <- mapM parseType rest
      case toAtom h of
        TyCon nm -> Just (TyApp (TyCon nm) args)
        TyVar nm -> Just (TyApp (TyVar nm) args)
        other    -> Just (TyApp other args)

splitTopSpace :: Text -> [Text]
splitTopSpace raw = filter (not . T.null) (splitTopOnChar ' ' raw)

splitTopOnChar :: Char -> Text -> [Text]
splitTopOnChar ch t = go (0 :: Int) [] (T.unpack t) []
  where
    go _ acc []     pending = reverse (flush acc pending)
    go d acc (c:cs) pending
      | c == '(' || c == '[' = go (d + 1) (c:acc) cs pending
      | c == ')' || c == ']' = go (max 0 (d - 1)) (c:acc) cs pending
      | d == 0 && c == ch    = go 0 [] cs (flush acc pending)
      | otherwise            = go d (c:acc) cs pending

    flush [] rest = rest
    flush acc rest = T.pack (reverse acc) : rest

toAtom :: Text -> SigType
toAtom t
  | T.null t                = TyVar "_"
  | isUpper (T.head t)      = TyCon (identifier t)
  | isLower (T.head t)
    || T.head t == '_'      = TyVar (identifier t)
  | otherwise               = TyCon t

identifier :: Text -> Text
identifier = T.takeWhile (\c -> isAlphaNum c || c == '_' || c == '\'' || c == '.')
