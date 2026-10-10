#!/usr/bin/env python3
"""hm-loop — HM dogfood, MCP-only end to end (W3.1).

One haskell-flows-mcp server, driven ONLY through MCP tool calls:
scaffold a project, author 6 modules via write_module, eval (flat and
nested), persist >=3 properties via ghc_property, land the project
gate green. Prints the E4 metric table at the end:

    calls<=25 | breaks=0 | zombies=0 | props>=3 | cold_start<=60s

Protocol law: the DRIVER may only call MCP tools. Host-side actions
are limited to resetting the fixture dir before the run and reading
files after (metrics/debug).

Usage:
  hm-loop.py <binary> <project_dir> <spool_dir>
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
# The task material: a small Hindley–Milner-flavoured expression
# language. Sources live here (versioned, clean headers) — they are
# the "agent's design", landed on disk EXCLUSIVELY via write_module.
# ---------------------------------------------------------------------------

SYNTAX = """\
-- | Surface syntax and types of the HM expression language.
module Hm.Syntax
  ( Expr(..)
  , Op(..)
  , Type(..)
  ) where

data Expr
  = Var String
  | Lit Int
  | BoolLit Bool
  | Lam String Expr
  | App Expr Expr
  | Let String Expr Expr
  | If Expr Expr Expr
  | Binop Op Expr Expr
  deriving stock (Show, Eq)

data Op = Add | Sub | Mul | Eq | Lt
  deriving stock (Show, Eq)

data Type = TInt | TBool | TFun Type Type
  deriving stock (Show, Eq)
"""

LEXER = """\
-- | Tokens of the HM expression language.
module Hm.Lexer
  ( Token(..)
  , lexExpr
  ) where

import Hm.Syntax (Op(..))
import Data.Char (isAlpha, isDigit, isSpace)

data Token
  = TIdent String | TIntLit Int | TBoolLit Bool
  | TLam | TDot | TLet | TIn | TIf | TThen | TElse
  | TOParen | TCParen | TEqual | TBackslash
  | TOp Op
  deriving stock (Show, Eq)

lexExpr :: String -> Either String [Token]
lexExpr = go
  where
    go [] = Right []
    go (c : cs)
      | isSpace c = go cs
      | c == '\\\\' = (TBackslash :) <$> go cs
      | c == '.' = (TDot :) <$> go cs
      | c == '(' = (TOParen :) <$> go cs
      | c == ')' = (TCParen :) <$> go cs
      | c == '=' = case cs of
          ('=' : r) -> (TOp Eq :) <$> go r
          _         -> (TEqual :) <$> go cs
      | isAlpha c =
          let (w, rest) = span (\\k -> isAlpha k || k == '_') (c : cs)
          in case w of
               "let"  -> (TLet :) <$> go rest
               "in"   -> (TIn :) <$> go rest
               "if"   -> (TIf :) <$> go rest
               "then" -> (TThen :) <$> go rest
               "else" -> (TElse :) <$> go rest
               "True"  -> (TBoolLit True :) <$> go rest
               "False" -> (TBoolLit False :) <$> go rest
               _       -> (TIdent w :) <$> go rest
      | isDigit c =
          let (ds, rest) = span isDigit (c : cs)
          in (TIntLit (read ds) :) <$> go rest
      | c == '+' = (TOp Add :) <$> go cs
      | c == '-', (d : _) <- cs, isDigit d =
          let (ds, rest) = span isDigit cs
          in (TIntLit (negate (read ds)) :) <$> go rest
      | c == '-' = (TOp Sub :) <$> go cs
      | c == '*' = (TOp Mul :) <$> go cs
      | c == '<' = (TOp Lt :) <$> go cs
      | otherwise = Left ("unexpected character: " ++ [c])
"""

PARSER = """\
-- | Recursive-descent parser for the HM expression language.
module Hm.Parser
  ( parseExpr
  ) where

import Hm.Syntax (Expr(..))
import Hm.Lexer (Token(..), lexExpr)

parseExpr :: String -> Either String Expr
parseExpr src = do
  toks <- lexExpr src
  case expr toks of
    Left err    -> Left err
    Right (e, []) -> Right e
    Right (_, t)  -> Left ("trailing tokens: " ++ show t)

expr :: [Token] -> Either String (Expr, [Token])
expr ts = app ts

app :: [Token] -> Either String (Expr, [Token])
app ts = do
  (f, ts1) <- atom ts
  more f ts1
  where
    more f rest = case atom rest of
      Right (a, rest') -> more (App f a) rest'
      Left _           -> binop f rest

binop :: Expr -> [Token] -> Either String (Expr, [Token])
binop l (TOp op : ts) = do
  (r, ts') <- atom ts
  binop (Binop op l r) ts'
binop l ts = Right (l, ts)

atom :: [Token] -> Either String (Expr, [Token])
atom (TIdent n : ts)    = Right (Var n, ts)
atom (TIntLit i : ts)   = Right (Lit i, ts)
atom (TBoolLit b : ts)  = Right (BoolLit b, ts)
atom (TLet : TIdent n : TEqual : ts) = do
  (bound, ts1) <- expr ts
  case ts1 of
    (TIn : ts2) -> do
      (body, ts3) <- expr ts2
      Right (Let n bound body, ts3)
    _ -> Left "let: expected 'in'"
atom (TIf : ts) = do
  (c, ts1) <- expr ts
  case ts1 of
    (TThen : ts2) -> do
      (t, ts3) <- expr ts2
      case ts3 of
        (TElse : ts4) -> do
          (e, ts5) <- expr ts4
          Right (If c t e, ts5)
        _ -> Left "if: expected 'else'"
    _ -> Left "if: expected 'then'"
atom (TOParen : ts) = do
  (e, ts1) <- expr ts
  case ts1 of
    (TCParen : ts2) -> Right (e, ts2)
    _ -> Left "paren: expected ')'"
atom (TBackslash : TIdent n : TDot : ts) = do
  (body, ts1) <- expr ts
  Right (Lam n body, ts1)
atom ts = Left ("unexpected token: " ++ show (take 1 ts))
"""

PRETTY = """\
-- | Minimal-parenthesis pretty-printer for HM expressions.
module Hm.Pretty
  ( pretty
  ) where

import Hm.Syntax (Expr(..), Op(..))

pretty :: Expr -> String
pretty = go (0 :: Int)
  where
    go _ (Var n)      = n
    go _ (Lit i)      = show i
    go _ (BoolLit b)  = if b then "True" else "False"
    go p (Lam n b)    = paren (p > 0) ("\\\\" ++ n ++ ". " ++ go 0 b)
    go p (App f x)    = paren (p > 1) (go 1 f ++ " " ++ go 2 x)
    go p (Let n b e)  = paren (p > 0)
      ("let " ++ n ++ " = " ++ go 0 b ++ " in " ++ go 0 e)
    go p (If c t e)   = paren (p > 0) ("if " ++ go 0 c ++ " then " ++ go 0 t
                                       ++ " else " ++ go 0 e)
    go p (Binop op l r) =
      paren (p > 0) (go 2 l ++ opStr op ++ " " ++ go 2 r)
    opStr Add = "+"; opStr Sub = "-"; opStr Mul = "*"
    opStr Eq  = "=="; opStr Lt = "<"
    paren True s  = "(" ++ s ++ ")"
    paren False s = s
"""

TYPECHECK = """\
-- | Monomorphic type inference (Algorithm W core) for HM exprs.
-- (assoc-list envs: the scaffold library exposes only `base`.)
module Hm.Typecheck
  ( infer
  ) where

import Hm.Syntax (Expr(..), Op(..), Type(..))

type Env = [(String, Type)]

infer :: Expr -> Either String Type
infer = go []
  where
    lookupVar :: Env -> String -> Maybe Type
    lookupVar env n = case [t | (k, t) <- env, k == n] of
      (t : _) -> Just t
      []      -> Nothing
    go :: Env -> Expr -> Either String Type
    go _ (Lit _)     = Right TInt
    go _ (BoolLit _) = Right TBool
    go env (Var n)   = case lookupVar env n of
      Just t  -> Right t
      Nothing -> Left ("unbound variable: " ++ n)
    go env (Lam n b) = do
      tb <- go ((n, TInt) : env) b
      Right (TFun TInt tb)
    go env (App f x) = do
      tf <- go env f
      tx <- go env x
      case tf of
        TFun a r | a == tx -> Right r
        _                  -> Left "application argument mismatch"
    go env (Let n b e) = do
      tb <- go env b
      go ((n, tb) : env) e
    go env (If c t e) = do
      tc <- go env c
      tt <- go env t
      te <- go env e
      if tc /= TBool then Left "if: condition must be Bool"
      else if tt /= te then Left "if: branches disagree"
      else Right tt
    go env (Binop op l r) = do
      tl <- go env l
      tr <- go env r
      if tl == TInt && tr == TInt
        then Right (case op of { Eq -> TBool; Lt -> TBool; _ -> TInt })
        else Left ("operator type mismatch: " ++ show (op, tl, tr))
"""

EVAL = """\
-- | Evaluator for the HM expression language.
-- (assoc-list envs: the scaffold library exposes only `base`.)
module Hm.Eval
  ( Value(..)
  , eval
  ) where

import Hm.Syntax (Expr(..), Op(..))

data Value = VInt Int | VBool Bool | VClosure String Expr Env
  deriving stock (Show, Eq)

type Env = [(String, Value)]

eval :: Expr -> Either String Value
eval = go []
  where
    lookupVar env n = case [v | (k, v) <- env, k == n] of
      (v : _) -> Just v
      []      -> Nothing
    go _ (Lit i)     = Right (VInt i)
    go _ (BoolLit b) = Right (VBool b)
    go env (Var n)   = case lookupVar env n of
      Just v  -> Right v
      Nothing -> Left ("unbound variable: " ++ n)
    go env (Lam n b) = Right (VClosure n b env)
    go env (App f x) = do
      vf <- go env f
      vx <- go env x
      case vf of
        VClosure n b cenv -> go ((n, vx) : cenv) b
        _                 -> Left "applying a non-function"
    go env (Let n b e) = do
      vb <- go env b
      go ((n, vb) : env) e
    go env (If c t e) = do
      vc <- go env c
      case vc of
        VBool True  -> go env t
        VBool False -> go env e
        _           -> Left "if: condition must be Bool"
    go env (Binop op l r) = do
      vl <- go env l
      vr <- go env r
      case (vl, vr) of
        (VInt a, VInt b) -> Right $ case op of
          Add -> VInt (a + b); Sub -> VInt (a - b); Mul -> VInt (a * b)
          Eq  -> VBool (a == b); Lt -> VBool (a < b)
        _ -> Left "operator on non-Int"
"""

SPEC = """\
-- | Property suite for the HM language (scaffold + QuickCheck).
-- (test-only orphans: Arbitrary for the syntax types lives here,
--  next to the generator, not in the base-only library)
{-# OPTIONS_GHC -Wno-orphans #-}
module Main where

import Hm.Syntax (Expr(..), Op(..), Type(..))
import Hm.Parser (parseExpr)
import Hm.Pretty (pretty)
import Hm.Eval (Value(..), eval)
import Hm.Typecheck (infer)
import Test.QuickCheck
  ( Arbitrary(..), elements, oneof, quickCheck, sized, withNumTests )

instance Arbitrary Op where
  arbitrary = elements [Add, Sub, Mul, Eq, Lt]

instance Arbitrary Expr where
  arbitrary = sized gen
    where
      gen 0 = oneof [Var <$> elements ["x", "y", "f"],
                     Lit <$> arbitrary, BoolLit <$> arbitrary]
      gen n = oneof
        [ gen 0
        , Lam <$> elements ["x", "y"] <*> gen (n `div` 2)
        , App <$> gen (n `div` 2) <*> gen (n `div` 2)
        , Binop <$> arbitrary <*> gen (n `div` 2) <*> gen (n `div` 2)
        , If <$> gen (n `div` 3) <*> gen (n `div` 3) <*> gen (n `div` 3)
        ]

prop_roundtrip :: Expr -> Bool
prop_roundtrip e = parseExpr (pretty e) == Right e

prop_evalLit :: Int -> Bool
prop_evalLit n = eval (Lit (abs n)) == Right (VInt (abs n))

prop_inferLit :: Int -> Bool
prop_inferLit n = infer (Lit (abs n)) == Right TInt

main :: IO ()
main = do
  quickCheck (withNumTests 300 prop_roundtrip)
  quickCheck (withNumTests 300 prop_evalLit)
  quickCheck (withNumTests 300 prop_inferLit)
"""

MODULES = [
    ("src/Hm/Syntax.hs", "Hm.Syntax", SYNTAX),
    ("src/Hm/Lexer.hs", "Hm.Lexer", LEXER),
    ("src/Hm/Parser.hs", "Hm.Parser", PARSER),
    ("src/Hm/Pretty.hs", "Hm.Pretty", PRETTY),
    ("src/Hm/Typecheck.hs", "Hm.Typecheck", TYPECHECK),
    ("src/Hm/Eval.hs", "Hm.Eval", EVAL),
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
        # wait for the handshake to complete
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
        TOOL("ghc_project", {"action": "create", "name": "hm",
                             "path": project_dir, "write": True},
             "create hm (write)")
        TOOL("ghc_module",
             {"action": "add",
              "modules": ["Hm.Syntax", "Hm.Lexer", "Hm.Parser",
                          "Hm.Pretty", "Hm.Typecheck", "Hm.Eval"]},
             "register 6 library modules")
        # (no test-anchor registration needed: the scaffold's
        #  test-suite already ships Spec.hs + QuickCheck — W1.5)
        for path, mod, src in MODULES:
            TOOL("ghc_module",
                 {"action": "write_module", "module": path, "code": src},
                 f"write_module {mod}")
        TOOL("ghc_check", {"action": "project"}, "project gate green")

        TOOL("ghc_eval", {"expression":
             "Hm.Parser.parseExpr (Hm.Pretty.pretty"
             " (Hm.Syntax.Binop Hm.Syntax.Add (Hm.Syntax.Lit 1)"
             " (Hm.Syntax.Lit 2)))"},
             "eval flat: pretty∘parse (qualified)")
        TOOL("ghc_eval", {"expression":
             r'eval (Let "x" (Lit 20) (Binop Add (Var "x") (Lit 22)))'},
             "eval nested: let x = 20 in x + 22")

        PROPS_EXPRS = [
            ("pretty-parse roundtrip",
             r"\e -> Hm.Parser.parseExpr (Hm.Pretty.pretty (e :: Hm.Syntax.Expr)) == Right e"),
            ("eval Lit identity",
             r"\n -> Hm.Eval.eval (Hm.Syntax.Lit (abs (n :: Int))) == Right (Hm.Eval.VInt (abs n))"),
            ("infer Lit is TInt",
             r"\n -> Hm.Typecheck.infer (Hm.Syntax.Lit (abs (n :: Int))) == Right Hm.Syntax.TInt"),
        ]
        for pname, pexpr in PROPS_EXPRS:
            TOOL("ghc_property",
                 {"action": "check", "property": pexpr},
                 f"property: {pname}")

        TOOL("ghc_property", {"action": "list"}, "property store has 3")

        # ---- E4 report ----
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
            "calls_ok": metrics["calls"] <= 25,
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
    print("\n== E4 ==")
    for k, v in e4.items():
        print(f"  {k}: {v}")
    verdict = (e4["calls_ok"] and e4["breaks"] == 0
               and e4["zombies"] == 0 and e4["props"] >= 3
               and e4["cold_start"] <= 60 and e4["green"])
    print("E4 VERDICT:", "GREEN" if verdict else "RED")
    sys.exit(0 if verdict else 1)


if __name__ == "__main__":
    main()
