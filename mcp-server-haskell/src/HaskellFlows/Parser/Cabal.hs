-- | Pure parsing of @.cabal@ content into the pieces the engine
-- needs: stanza structure, scalar fields and comma-list fields.
--
-- Everything here is total and IO-free — the seam where the mutable
-- ghcide session meets cabal files should be a one-line read of
-- 'T.IO.readFile' feeding these parsers, nothing else. The grammar
-- implemented is the subset the Cabal user-guide guarantees for
-- stanza layout (top-level stanza headers, @field: value@ lines,
-- indented continuation lines), not the full PackageDescription.
module HaskellFlows.Parser.Cabal
  ( -- * Stanzas
    Stanza (..)
  , splitStanzas
  , stanzaHeaderOf
    -- * Fields
  , fieldSplit
  , packageNameOf
  , sourceDirOf
  , listFieldOf
    -- * Derived inventory
  , projectModuleFilesFromCabal
  ) where

import Data.Char (isSpace)
import Data.Maybe (isNothing, mapMaybe)
import Data.Text (Text)
import qualified Data.Text as T

-- | A cabal stanza we can map to a hie.yaml component.
data Stanza = LibStanza | TestStanza !Text
  deriving stock (Eq, Show)

-- | Split significant lines into (stanza header, body) pairs. Lines
-- before the first stanza header are dropped. The body spans until
-- the next column-0 stanza header.
splitStanzas :: [Text] -> [(Stanza, [Text])]
splitStanzas [] = []
splitStanzas (t : rest) = case stanzaHeaderOf t of
  Just st ->
    let (body, rest') = span (isNothing . stanzaHeaderOf) rest
    in (st, body) : splitStanzas rest'
  Nothing -> splitStanzas rest

-- | Top-level (column-0) stanza headers only.
stanzaHeaderOf :: Text -> Maybe Stanza
stanzaHeaderOf t
  | not (T.null t)
  , not (isSpace (T.head t)) =
      if t == "library"
        then Just LibStanza
        else case T.stripPrefix "test-suite " t of
          Just rest
            | not (T.null (T.strip rest)) ->
                Just (TestStanza (T.takeWhile (/= ' ') (T.strip rest)))
          _ -> Nothing
  | otherwise = Nothing

-- | Break a @field: value@ line, dropping the separator colon and
-- trimming both sides.
fieldSplit :: Text -> (Text, Text)
fieldSplit t =
  let (k, v) = T.break (== ':') t
  in (T.strip k, T.strip (T.drop 1 v))

packageNameOf :: [Text] -> Either String Text
packageNameOf ls =
  case mapMaybe pick ls of
    (n : _) -> Right n
    [] -> Left "no 'name:' field in .cabal"
  where
    pick t =
      let (k, v) = fieldSplit t
      in if T.toLower (T.strip k) == "name" && not (T.null v)
           then Just v
           else Nothing

-- | First comma-separated value of the stanza's hs-source-dirs, or the
-- conventional default when the stanza omits the field.
sourceDirOf :: Text -> [Text] -> Text
sourceDirOf def body =
  case mapMaybe pick body of
    (d : _) -> d
    [] -> def
  where
    pick t =
      let (k, v) = fieldSplit t
      in if T.toLower k == "hs-source-dirs"
           then let first = T.takeWhile (/= ',') v
                in if T.null first then Nothing else Just first
           else Nothing

-- | All values of a comma-separated list field, joining
-- continuation lines (@\  , Foo@) to the header line.
listFieldOf :: Text -> [Text] -> [Text]
listFieldOf name body =
  case dropWhile (not . isField name) body of
    [] -> []
    (h : rest) ->
      -- cabal continuation grammar: a field value spans its first
      -- line plus following lines that are neither fields (contain
      -- ':') nor stanza headers — covers both the comma style and
      -- the one-module-per-line style ghc_module add writes.
      let first = splitList (snd (fieldSplit h))
          conts = concatMap splitList (takeWhile isItem rest)
          items = filter (not . T.null) (first <> conts)
      in items
  where
    isField n t = T.toLower (T.strip (fst (fieldSplit t))) == n
    isItem t =
      not (T.null t)
        && not (T.any (== ':') t)
        && not (isStanzaHeader t)
    isStanzaHeader t =
      let w = T.takeWhile (/= ' ') t
      in w `elem`
           [ "library", "test-suite", "executable", "benchmark"
           , "flag", "source-repository", "foreign-library", "common"
           , "custom-setup", "setup" ]
    splitList v = [ T.strip x | x <- T.splitOn "," v ]

-- | F3: relative module paths (library + test stanzas) parsed from
-- .cabal content. Module names map to @dir/Mod/Sub.hs@ under each
-- stanza's hs-source-dirs.
projectModuleFilesFromCabal :: Text -> [FilePath]
projectModuleFilesFromCabal cabal =
  concatMap stanzaModules (splitStanzas sigLines)
  where
    sigLines =
      [ T.strip t
      | t <- T.lines cabal
      , not (T.null (T.strip t))
      , not ("--" `T.isPrefixOf` T.strip t)
      ]
    stanzaModules (LibStanza, body)    = mk "src" body
    stanzaModules (TestStanza _, body) = mk "test" body
    mk def body =
      let dir = T.unpack (sourceDirOf (T.pack def) body)
      in map (moduleToPath dir) (listFieldOf "exposed-modules" body
                              <> listFieldOf "other-modules" body)
    moduleToPath dir name =
      dir <> "/" <> map (\c -> if c == '.' then '/' else c) (T.unpack name) <> ".hs"
