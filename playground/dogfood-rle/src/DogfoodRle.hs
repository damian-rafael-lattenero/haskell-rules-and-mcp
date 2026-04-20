-- | Run-length encoding — scratch project for Phase 11b dogfood.
--
-- Laws the agent is expected to discover via ghci_suggest + verify
-- via ghci_quickcheck:
--
-- * roundtrip:          @decode (encode xs) == xs@
-- * length preserved:   @length (decode (encode xs)) == length xs@
-- * non-empty image:    @not (null xs) ==> not (null (encode xs))@
-- * runs are non-zero:  @all ((> 0) . runLen) (encode xs)@
module DogfoodRle
  ( Run (..)
  , encode
  , decode
  ) where

import Data.List (group)

-- | A single run in the encoded stream: how many consecutive copies
-- of a given value appeared.
data Run a = Run
  { runLen :: !Int
  , runVal :: !a
  }
  deriving stock (Eq, Show)

-- | Collapse consecutive equal elements into 'Run's.
--
-- @encode "aaabbc" = [Run 3 'a', Run 2 'b', Run 1 'c']@
encode :: Eq a => [a] -> [Run a]
encode = map mkRun . group
  where
    mkRun g = Run (length g) (head g)

-- | Inverse of 'encode'. Replays each run as a flat list.
decode :: [Run a] -> [a]
decode = concatMap expandRun
  where
    expandRun (Run n v) = replicate n v
