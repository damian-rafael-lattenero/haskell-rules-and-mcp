---
paths:
  - "src/**/*.hs"
  - "app/**/*.hs"
  - "*.cabal"
  - "cabal.project"
---

# Project: haskell-rules-and-mcp

## Toolchain
- GHC 9.12.2, GHC2024, Cabal 3.12, macOS aarch64
- `-Wall` enabled for both library and executable
- `.ghci` has `-fdefer-type-errors -ferror-spans -fprint-explicit-foralls`
- Dependencies: base, containers, array, mtl, QuickCheck

## Module Architecture
- `HM.Syntax` — AST types: Expr (11 constructors), Type (TVar/TCon/TArr/TProd), Scheme, Lit
- `HM.Subst` — Substitutable typeclass, apply/ftv for Type, Scheme, lists
- `HM.Unify` — Robinson unification + occurs check, TypeError ADT
- `HM.Infer` — Algorithm W inference, ExceptT TypeError (State Int) monad, defaultEnv with operator types
- `HM.Pretty` — Pretty-printing with infix operator detection, lambda collapsing
- `Parser.Core` — Parser monad with furthest-failure tracking, ParseError type
- `Parser.Combinators` — sepBy, between, chainl1/chainr1, option, notFollowedBy
- `Parser.Char` — Lexing: identifier, reserved, operator, natural, symbol
- `Parser.HM` — Full expression parser with operator precedence, multi-arg lambda, multi-binding let, typo hints

## Conventions
- All Map/Set imports are `qualified` (Map.Map, Set.Set)
- Operators desugar to `EApp (EApp (EVar op) e1) e2` — new operators need defaultEnv entry in HM.Infer
- New Expr constructors require changes in: HM.Syntax + HM.Infer + HM.Pretty + Parser.HM
- Testing: examples in app/Main.hs (54+ parsed + manual). No Hspec/Tasty framework.
- ELetRec is separate from ELet (no mutual recursion)
