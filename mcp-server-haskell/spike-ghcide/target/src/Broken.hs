-- | Not registered in the .cabal: ghcide resolves it via a fallback cradle
-- (base-only). Used by spike A to force real type-error diagnostics.
module Broken where

oops :: Int
oops = 'x'
