-- | Flow: 'ghc_info' for a typeclass returns a class header +
-- methods, not a malformed string (#70).
--
-- Pre-#70, querying any class produced a 'definition' field
-- like 'class Functor Class `Functor''. The agent had no way
-- to recover the method API from the response — it knew the
-- kind and the instances, but not what fmap/(<$)/etc. looked
-- like. The fix:
--
--   * 'definition' is now a real Haskell header — at least
--     'class Functor f where' followed by indented method
--     signatures.
--   * a structured 'class_methods' array carries one
--     {name, type} object per method, mirroring the existing
--     'constructors' field for data types (#54).
--
-- This scenario asserts the new shape on the canonical
-- examples (Functor + Eq) and confirms the data-type path
-- (#54) still works in the same response shape.
module Scenarios.FlowInfoClass
  ( runFlow
  ) where

import Data.Aeson (Value (..), object, (.=))
import qualified Data.Aeson.Key as Key
import qualified Data.Aeson.KeyMap as KeyMap
import Data.Maybe (fromMaybe)
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Vector as V

import E2E.Assert
  ( Check (..)
  , checkPure
  , liveCheck
  , stepFooter
  , stepHeader
  )
import qualified E2E.Client as Client
import E2E.Envelope (fieldText, lookupField)
import HaskellFlows.Mcp.ToolName (ToolName (..))

runFlow :: Client.McpClient -> FilePath -> IO [Check]
runFlow c _projectDir = do
  -- Step 1 — bootstrap a project so GHCi has somewhere to live.
  -- The class info we need (Functor / Eq) lives in base, so the
  -- project doesn't need any custom modules; we only need a
  -- live session.
  _ <- Client.callTool c GhcCreateProject
         (object [ "name" .= ("info-class-demo" :: Text) ])
  _ <- Client.callTool c GhcLoad
         (object [ "module_path" .= ("src/InfoClassDemo.hs" :: Text) ])

  -- Step 2 — Functor: must report class header + 'fmap'.
  t0 <- stepHeader 1 "ghc_info(Functor) returns class header + fmap (#70)"
  rFun <- Client.callTool c GhcInfo
            (object [ "name" .= ("Functor" :: Text) ])
  let funDef    = fieldText "definition" rFun
      funMethods = methodNames rFun
      funOk =
           fieldText "kind" rFun == Just "class"
        && maybe False ("class Functor" `T.isInfixOf`) funDef
        && "fmap" `elem` funMethods
        -- Critical: pre-#70 'definition' was the malformed
        -- "class Functor Class `Functor'". The fix removes the
        -- backtick-wrapped artefact entirely.
        && maybe False (not . T.isInfixOf "Class `Functor'") funDef
  cFun <- liveCheck $ checkPure
    "Functor reports header + fmap; no 'Class `Functor'' artefact"
    funOk
    ("Got: definition=" <> fromMaybe "<missing>" funDef
       <> " methods=" <> T.intercalate ", " funMethods)
  stepFooter 1 t0

  -- Step 3 — Eq: must report class header + at least one of
  -- (==) / (/=). MINIMAL pragma exposes only one, but at least
  -- one operator must surface.
  t1 <- stepHeader 2 "ghc_info(Eq) returns class header + (==)/(/=) (#70)"
  rEq <- Client.callTool c GhcInfo
           (object [ "name" .= ("Eq" :: Text) ])
  let eqMethods = methodNames rEq
      hasOp     = "(==)" `elem` eqMethods || "(/=)" `elem` eqMethods
      eqOk      =
           fieldText "kind" rEq == Just "class"
        && hasOp
        && maybe False (not . T.isInfixOf "Class `Eq'")
                       (fieldText "definition" rEq)
  cEq <- liveCheck $ checkPure
    "Eq reports at least one of (==)/(/=); no 'Class `Eq'' artefact"
    eqOk
    ("Got methods: " <> T.intercalate ", " eqMethods)
  stepFooter 2 t1

  -- Step 4 — sanity: a data type still works (#54 contract).
  -- Maybe must report kind=data + constructors[]. The
  -- class_methods field must be ABSENT.
  t2 <- stepHeader 3 "ghc_info(Maybe) data path unaffected (#70 + #54)"
  rMaybe <- Client.callTool c GhcInfo
             (object [ "name" .= ("Maybe" :: Text) ])
  let maybeOk =
           fieldText "kind" rMaybe == Just "data"
        && hasArrayField "constructors" rMaybe
        -- class_methods must NOT be present on a data type.
        && not (hasArrayField "class_methods" rMaybe)
  cMaybe <- liveCheck $ checkPure
    "Maybe still reports kind=data + constructors[] without class_methods"
    maybeOk
    ("Got: " <> truncRender rMaybe)
  stepFooter 3 t2

  pure [cFun, cEq, cMaybe]

--------------------------------------------------------------------------------
-- helpers
--------------------------------------------------------------------------------

methodNames :: Value -> [Text]
methodNames v = case lookupField "class_methods" v of
  Just (Array a) ->
    [ n | Object o <- V.toList a
        , Just (String n) <- [KeyMap.lookup (Key.fromText "name") o] ]
  _ -> []

hasArrayField :: Text -> Value -> Bool
hasArrayField k v = case lookupField k v of
  Just (Array _) -> True
  _              -> False

truncRender :: Value -> Text
truncRender v =
  let raw = T.pack (show v)
      cap = 600
  in if T.length raw > cap then T.take cap raw <> "…(truncated)" else raw
