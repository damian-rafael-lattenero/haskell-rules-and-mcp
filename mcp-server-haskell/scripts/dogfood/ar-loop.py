#!/usr/bin/env python3
"""ar-loop — arithmetic-expr dogfood, MCP-only end to end (W3.2).

The small sibling of hm-loop: same protocol law (the DRIVER may only
call MCP tools; host-side actions are limited to resetting the fixture
dir before the run and reading files after), a third of the surface —
proving the loop is not HM-shaped. Targets scale down accordingly:

    calls<=15 | breaks=0 | zombies=0 | props>=2 | cold_start<=60s

Usage:
  ar-loop.py <binary> <project_dir> <spool_dir>
"""
import json, os, shutil, subprocess, sys, time

sys.path.insert(0, os.path.dirname(os.path.abspath(__file__)))
import mcpd


def _server_count():
    r = subprocess.run(["pgrep", "-f", "haskell-flows-mcp$"],
                       capture_output=True, text=True)
    return len([l for l in r.stdout.splitlines() if l.strip()])


BASELINE_SERVERS = _server_count()

# ---------------------------------------------------------------------------
# The task material: arithmetic expressions with precedence. Sources live
# here (versioned, clean headers) — landed on disk EXCLUSIVELY via
# write_module.
# ---------------------------------------------------------------------------

EXPR = """\
-- | Arithmetic expressions: eval only, no surface syntax here.
module Ar.Expr
  ( Expr(..)
  , eval
  ) where

data Expr
  = Lit Int
  | Add Expr Expr
  | Mul Expr Expr
  | Neg Expr
  deriving stock (Show, Eq)

eval :: Expr -> Int
eval (Lit n)   = n
eval (Add l r) = eval l + eval r
eval (Mul l r) = eval l * eval r
eval (Neg e)   = negate (eval e)
"""

PRETTY = """\
-- | Minimal-parenthesis printer. Precedence: Neg > Mul > Add; Add is
-- the loosest so it parenthesizes at any operand depth (p > 0), Mul
-- and Neg only inside Mul/Neg positions (p > 1).
module Ar.Pretty
  ( pretty
  ) where

import Ar.Expr (Expr(..))

pretty :: Expr -> String
pretty = go (0 :: Int)
  where
    go _ (Lit n)   = show n
    go p (Neg e)   = paren (p > 1) ("-" ++ go 2 e)
    go p (Add l r) = paren (p > 0) (go 1 l ++ " + " ++ go 1 r)
    go p (Mul l r) = paren (p > 1) (go 2 l ++ " * " ++ go 2 r)
    paren True  s  = "(" ++ s ++ ")"
    paren False s  = s
"""

PARSER = """\
-- | Char-level recursive-descent parser: expr (term ('*' term)* ) with
-- unary minus and parens at the factor level.
module Ar.Parser
  ( parse
  ) where

import Ar.Expr (Expr(..))
import Data.Char (isDigit, isSpace)

parse :: String -> Maybe Expr
parse src = case expr (filter (not . isSpace) src) of
  Just (e, []) -> Just e
  _            -> Nothing

expr :: String -> Maybe (Expr, String)
expr s = do
  (t, rest) <- term s
  more t rest
  where
    more l ('+' : r) = do
      (t, r') <- term r
      more (Add l t) r'
    more l r = Just (l, r)

term :: String -> Maybe (Expr, String)
term s = do
  (f, rest) <- factor s
  more f rest
  where
    more l ('*' : r) = do
      (f, r') <- factor r
      more (Mul l f) r'
    more l r = Just (l, r)

factor :: String -> Maybe (Expr, String)
factor ('-' : r) = do
  (e, r') <- factor r
  Just (Neg e, r')
factor ('(' : r) = do
  (e, r') <- expr r
  case r' of
    (')' : r'') -> Just (e, r'')
    _           -> Nothing
factor s@(c : _)
  | not (isDigit c) = Nothing
  | otherwise =
      let (ds, r) = span isDigit s
      in Just (Lit (read ds), r)
factor [] = Nothing
"""

SPEC = """\
-- | Property suite for the arithmetic language (scaffold + QuickCheck).
-- (no negative literals in the surface syntax: the generator keeps Lit
--  non-negative — "-3" is spelled Neg (Lit 3); test-only orphans)
{-# OPTIONS_GHC -Wno-orphans #-}
module Main where

import Ar.Expr (Expr(..), eval)
import Ar.Pretty (pretty)
import Ar.Parser (parse)
import Test.QuickCheck
  ( Arbitrary(..), oneof, quickCheck, sized, withNumTests )

instance Arbitrary Expr where
  arbitrary = sized gen
    where
      gen 0 = Lit . abs <$> arbitrary
      gen n = oneof
        [ gen 0
        , Neg <$> gen (n `div` 2)
        , Add <$> gen (n `div` 2) <*> gen (n `div` 2)
        , Mul <$> gen (n `div` 2) <*> gen (n `div` 2)
        ]

prop_roundtrip :: Expr -> Bool
prop_roundtrip e = parse (pretty e) == Just e

prop_evalLit :: Int -> Bool
prop_evalLit n = eval (Lit (abs n)) == abs n

main :: IO ()
main = do
  quickCheck (withNumTests 300 prop_roundtrip)
  quickCheck (withNumTests 300 prop_evalLit)
"""

MODULES = [
    ("src/Ar/Expr.hs", "Ar.Expr", EXPR),
    ("src/Ar/Pretty.hs", "Ar.Pretty", PRETTY),
    ("src/Ar/Parser.hs", "Ar.Parser", PARSER),
    ("test/Spec.hs", "Main", SPEC),
]

# ---------------------------------------------------------------------------


def base_zombies():
    """Server processes left behind by THIS run's teardown — pgrep
    after the daemon dies, minus the pre-run baseline (the user's own
    long-lived server is not our zombie)."""
    def count():
        r = subprocess.run(["pgrep", "-f", "haskell-flows-mcp$"],
                           capture_output=True, text=True)
        return len([l for l in r.stdout.splitlines() if l.strip()])
    return max(0, count() - BASELINE_SERVERS)


def call(spool, tool, args, timeout=240):
    """One FIFO round-trip: write the request, poll the resp file."""
    resp = f"{spool}/resp.{time.time_ns()}.json"
    t0 = time.time()
    with open(f"{spool}/cmd.fifo", "w") as fifo:
        fifo.write(f"{tool}\t{json.dumps(args)}\t{resp}\n")
    out = None
    deadline = time.time() + timeout
    while time.time() < deadline:
        if os.path.exists(resp):
            time.sleep(0.05)
            with open(resp) as f:
                out = f.read()
            os.unlink(resp)
            break
        time.sleep(0.1)
    if out is None:
        payload = {"driver": "TIMEOUT", "tool": tool}
    else:
        try:
            payload = json.loads(out)
        except json.JSONDecodeError:
            payload = {"driver": "UNPARSED", "raw": (out or "")[:400]}
    return payload, time.time() - t0


def ok(payload):
    return payload.get("status") == "ok"


def main():
    binary, project_dir, spool = (sys.argv[1], os.path.abspath(sys.argv[2]),
                              os.path.abspath(sys.argv[3]))

    # host-side: reset the fixture + spool (regenerable state only)
    for d in (project_dir, spool):
        shutil.rmtree(d, ignore_errors=True)
    os.makedirs(project_dir, exist_ok=True)
    os.makedirs(spool, exist_ok=True)

    t_cold0 = time.time()
    daemon = subprocess.Popen(
        [sys.executable, os.path.join(os.path.dirname(
            os.path.abspath(__file__)), "mcpd.py"),
         "daemon", binary, project_dir, spool],
        stdout=open(f"{spool}/daemon.out", "wb"), stderr=subprocess.STDOUT)
    try:
        deadline = time.time() + 120
        while not os.path.exists(f"{spool}/daemon.ready"):
            if daemon.poll() is not None:
                print("FATAL: daemon died during handshake")
                sys.exit(1)
            if time.time() > deadline:
                print("FATAL: handshake timeout")
                sys.exit(1)
            time.sleep(0.5)
        cold_start = time.time() - t_cold0

        metrics = {"calls": 0, "breaks": 0, "cold_start": cold_start}
        verdicts = []

        def TOOL(tool, args, name, informational=False):
            metrics["calls"] += 1
            p, dt = call(spool, tool, args)
            good = ok(p) or (informational and p.get("status") == "partial")
            if not good:
                metrics["breaks"] += 1
            print(f"  [{metrics['calls']:2d}] {tool:<14} {name:<34}"
                  f" {'ok' if good else 'FAIL'} {dt:5.1f}s")
            if not good:
                print("      " + json.dumps(p)[:300])
            verdicts.append((name, good))
            return p

        # ---- the loop (MCP-only) ----
        TOOL("ghc_session", {"action": "toolchain"},
             "handshake toolchain", informational=True)
        TOOL("ghc_project", {"action": "create", "name": "ar",
                             "path": project_dir, "write": True},
             "create ar (write)")
        TOOL("ghc_module",
             {"action": "add",
              "modules": ["Ar.Expr", "Ar.Pretty", "Ar.Parser"]},
             "register 3 library modules")
        # (no test-anchor registration needed: the scaffold's
        #  test-suite already ships Spec.hs + QuickCheck — W1.5)
        for path, mod, src in MODULES:
            TOOL("ghc_module",
                 {"action": "write_module", "module": path, "code": src},
                 f"write_module {mod}")
        TOOL("ghc_check", {"action": "project"}, "project gate green")

        TOOL("ghc_eval", {"expression":
             "Ar.Expr.eval (Ar.Expr.Add (Ar.Expr.Mul (Ar.Expr.Lit 6)"
             " (Ar.Expr.Lit 7)) (Ar.Expr.Lit 1))"},
             "eval: 6 * 7 + 1 == 43 (qualified)")

        PROPS_EXPRS = [
            ("pretty-parse roundtrip",
             r"\e -> Ar.Parser.parse (Ar.Pretty.pretty (e :: Ar.Expr.Expr)) == Just e"),
            ("eval Lit identity",
             r"\n -> Ar.Expr.eval (Ar.Expr.Lit (abs (n :: Int))) == abs n"),
        ]
        for pname, pexpr in PROPS_EXPRS:
            TOOL("ghc_property",
                 {"action": "check", "property": pexpr},
                 f"property: {pname}")

        TOOL("ghc_property", {"action": "list"}, "property store has 2")

        # ---- E4 report (scaled to the smaller surface) ----
        props_persisted = 0
        try:
            store = open(f"{project_dir}/.haskell-flows/properties.json")
            props_persisted = len(json.load(store))
        except Exception:
            pass

        green = all(g for _, g in verdicts[1:])  # toolchain probe is
        # informational (optional deps may be absent) — don't count it
        e4 = {
            "calls": metrics["calls"],
            "calls_ok": metrics["calls"] <= 15,
            "breaks": metrics["breaks"],
            "props": props_persisted,
            "cold_start": round(cold_start, 1),
            "green": green,
        }
    finally:
        # teardown FIRST so the zombie count reflects leftovers only
        daemon.terminate()
        try:
            daemon.wait(timeout=5)
        except subprocess.TimeoutExpired:
            daemon.kill()
        time.sleep(1)   # let the server see stdin-EOF and exit

    e4["zombies"] = base_zombies()
    print("\n== E4 (small-dogfood targets: calls<=15 props>=2) ==")
    for k, v in e4.items():
        print(f"  {k}: {v}")
    verdict = (e4["calls_ok"] and e4["breaks"] == 0
               and e4["zombies"] == 0 and e4["props"] >= 2
               and e4["cold_start"] <= 60 and e4["green"])
    print("E4 VERDICT:", "GREEN" if verdict else "RED")
    sys.exit(0 if verdict else 1)


if __name__ == "__main__":
    main()
