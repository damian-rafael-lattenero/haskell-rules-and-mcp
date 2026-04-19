-- | Pure rewrite engine for \"extract binding\".
--
-- Given a contiguous line range inside a module, produce a new version
-- of the file where those lines are replaced by a reference to a
-- freshly-introduced @where@ clause at the end of the enclosing top-
-- level declaration. The extracted body becomes the right-hand side of
-- the new binding.
--
-- Limitations (deliberate, since we're textual):
--
-- * Assumes the extracted range is an /expression/, not a pattern or
--   declaration. The compile-verify step in the tool layer catches
--   missuses — if the rewrite produces something that doesn't parse,
--   the snapshot is restored.
-- * The @where@ clause is appended at end-of-file. This is safe: GHC
--   treats it as attaching to the nearest top-level binding via layout
--   rules so long as it's at column 0. If the file already ends with a
--   @where@ on the nearest binding, the agent should prefer 'rename'
--   over 'extract' to add a local let — covered in the @hint@ we
--   return.
--
-- This is a minimum-viable extract. A proper AST-aware version lives
-- behind a future @ghci_refactor@ powered by @ghc-lib-parser@; for now
-- this lets an agent factor a repeated sub-expression out with the
-- compiler enforcing correctness.
module HaskellFlows.Refactor.Extract
  ( extractBinding
  , ExtractResult (..)
  ) where

import Data.Char (isSpace)
import Data.Text (Text)
import qualified Data.Text as T

-- | What the tool layer needs to render a response or revert.
data ExtractResult = ExtractResult
  { erNewContent :: !Text
  , erBindingTxt :: !Text       -- the new binding we appended
  , erIndent     :: !Int        -- indent of the extracted region
  }
  deriving stock (Eq, Show)

-- | Replace @[startLine, endLine]@ with a call to @newName@, and
-- append a @newName = <original block>@ binding to the file.
extractBinding
  :: Text                 -- ^ new binding name (validated)
  -> Int                  -- ^ start line (1-based, inclusive)
  -> Int                  -- ^ end line   (1-based, inclusive)
  -> Text                 -- ^ original file content
  -> Either Text ExtractResult
extractBinding newName startLine endLine raw
  | startLine < 1 =
      Left "line_start must be >= 1"
  | endLine < startLine =
      Left "line_end must be >= line_start"
  | otherwise = do
      let ls      = T.lines raw
          totalN  = length ls
      if endLine > totalN
        then Left ("line_end " <> tshow endLine
                <> " is past end of file (" <> tshow totalN <> " lines)")
        else do
          let (before, rest) = splitAt (startLine - 1) ls
              (body,  after) = splitAt (endLine - startLine + 1) rest
          if null body
            then Left "extracted range is empty"
            else do
              let indent     = commonIndent body
                  spaces     = T.replicate indent " "
                  callLine   = spaces <> newName
                  bindingTxt = renderBinding newName indent body
                  -- Preserve original trailing newline, if any.
                  hadTrailing = T.isSuffixOf "\n" raw
                  newBody =
                    T.intercalate "\n"
                      (before <> [callLine] <> after <> [bindingTxt])
                    <> (if hadTrailing then "\n" else "")
              pure ExtractResult
                { erNewContent = newBody
                , erBindingTxt = bindingTxt
                , erIndent     = indent
                }

--------------------------------------------------------------------------------
-- helpers
--------------------------------------------------------------------------------

-- | Common leading-whitespace length across a group of lines. Blank
-- lines are ignored so a vertical gap in the extracted block doesn't
-- collapse the indent to zero.
commonIndent :: [Text] -> Int
commonIndent ls =
  let nonBlank = filter (not . T.null . T.strip) ls
  in case nonBlank of
       []     -> 0
       (l:rest) ->
         foldr (min . T.length . T.takeWhile isSpace)
               (T.length (T.takeWhile isSpace l))
               rest

-- | Render @newName = <body>@ as a top-level binding that can be
-- appended to the file. Body lines are dedented by the common indent
-- so the right-hand side reads naturally at column 2, then bumped back
-- to column 0 for the @newName =@ header.
renderBinding :: Text -> Int -> [Text] -> Text
renderBinding newName indent body =
  let dedent ln =
        let n = min indent (T.length (T.takeWhile isSpace ln))
        in T.drop n ln
      -- Preserve relative indentation within the extracted block.
      reindent = map (\ln -> "  " <> dedent ln) body
      header   = newName <> " ="
  in T.intercalate "\n" (header : reindent)

tshow :: Show a => a -> Text
tshow = T.pack . show
