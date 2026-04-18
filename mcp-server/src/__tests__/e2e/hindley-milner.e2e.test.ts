/**
 * E2E Extreme Test: Generate Hindley-Milner Type Inference from scratch,
 * compile via MCP, and verify correctness with eval + QuickCheck.
 *
 * This test creates a complete HM type inference system as a single Haskell
 * module, then uses MCP tools to compile it, evaluate inference results,
 * and verify algebraic properties — all without human interaction.
 */
import { describe, it, expect, beforeAll, afterAll } from "vitest";
import { Client } from "@modelcontextprotocol/sdk/client/index.js";
import { StdioClientTransport } from "@modelcontextprotocol/sdk/client/stdio.js";
import { execSync } from "node:child_process";
import { writeFile, unlink, mkdir } from "node:fs/promises";
import path from "node:path";
import { setupIsolatedFixture, type IsolatedFixture } from "../helpers/isolated-fixture.js";

const SERVER_SCRIPT = path.resolve(
  import.meta.dirname,
  "../../../dist/index.js"
);
const GHCUP_BIN = path.join(process.env.HOME ?? "", ".ghcup", "bin");
const TEST_PATH = `${GHCUP_BIN}:${process.env.PATH}`;

const GHC_AVAILABLE = (() => {
  try {
    execSync("ghc --version", {
      stdio: "pipe",
      env: { ...process.env, PATH: TEST_PATH },
    });
    return true;
  } catch {
    return false;
  }
})();

function callTool(client: Client, name: string, args: Record<string, unknown> = {}) {
  return client.callTool({ name, arguments: args });
}

function parseResult(result: Awaited<ReturnType<Client["callTool"]>>): any {
  const text = (result.content as Array<{ type: string; text: string }>)[0]!.text;
  return JSON.parse(text);
}

// ============================================================================
// The complete Hindley-Milner implementation as a single module.
// Written by the test, compiled by the MCP, verified by eval + QuickCheck.
// ============================================================================
const HM_SOURCE = `module HM where

import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import Data.Set (Set)
import qualified Data.Set as Set
import Control.Monad.State
import Control.Monad.Except
import Test.QuickCheck (Arbitrary(..), oneof, sized, resize, elements)

-- Types
type Name = String
type TVar = String

data Lit = LInt Integer | LBool Bool
  deriving (Show, Eq, Ord)

data Expr
  = EVar  Name
  | ELit  Lit
  | EApp  Expr Expr
  | ELam  Name Expr
  | ELet  Name Expr Expr
  | EIf   Expr Expr Expr
  deriving (Show, Eq)

data Type = TVar TVar | TCon String | TArr Type Type
  deriving (Show, Eq, Ord)

data Scheme = Forall [TVar] Type
  deriving (Show, Eq)

data InferError
  = UnificationFail Type Type
  | InfiniteType TVar Type
  | UnboundVariable Name
  deriving (Show, Eq)

-- Substitution
newtype Subst = Subst (Map TVar Type)
  deriving (Show, Eq)

emptySubst :: Subst
emptySubst = Subst Map.empty

compose :: Subst -> Subst -> Subst
compose s2@(Subst m2) (Subst m1) =
  Subst (Map.map (apply s2) m1 \`Map.union\` m2)

class Substitutable a where
  apply :: Subst -> a -> a
  ftv   :: a -> Set TVar

instance Substitutable Type where
  apply (Subst s) (TVar a)   = Map.findWithDefault (TVar a) a s
  apply _         (TCon c)   = TCon c
  apply s         (TArr l r) = TArr (apply s l) (apply s r)
  ftv (TVar a)   = Set.singleton a
  ftv (TCon _)   = Set.empty
  ftv (TArr l r) = ftv l \`Set.union\` ftv r

instance Substitutable Scheme where
  apply (Subst s) (Forall vars t) =
    Forall vars (apply (Subst (foldr Map.delete s vars)) t)
  ftv (Forall vars t) = ftv t \`Set.difference\` Set.fromList vars

instance Substitutable a => Substitutable [a] where
  apply s = map (apply s)
  ftv     = foldr (Set.union . ftv) Set.empty

-- Type Environment
newtype TypeEnv = TypeEnv (Map Name Scheme) deriving (Show)

emptyEnv :: TypeEnv
emptyEnv = TypeEnv Map.empty

extend :: TypeEnv -> (Name, Scheme) -> TypeEnv
extend (TypeEnv env) (x, s) = TypeEnv (Map.insert x s env)

lookupEnv :: Name -> TypeEnv -> Maybe Scheme
lookupEnv x (TypeEnv env) = Map.lookup x env

instance Substitutable TypeEnv where
  apply s (TypeEnv env) = TypeEnv (Map.map (apply s) env)
  ftv (TypeEnv env) = ftv (Map.elems env)

-- Unification
unify :: Type -> Type -> Either InferError Subst
unify (TArr l1 r1) (TArr l2 r2) = do
  s1 <- unify l1 l2
  s2 <- unify (apply s1 r1) (apply s1 r2)
  pure (s2 \`compose\` s1)
unify (TVar a) t = bind a t
unify t (TVar a) = bind a t
unify (TCon a) (TCon b)
  | a == b    = Right emptySubst
  | otherwise = Left (UnificationFail (TCon a) (TCon b))
unify t1 t2 = Left (UnificationFail t1 t2)

bind :: TVar -> Type -> Either InferError Subst
bind a t
  | t == TVar a         = Right emptySubst
  | a \`Set.member\` ftv t = Left (InfiniteType a t)
  | otherwise           = Right (Subst (Map.singleton a t))

-- Inference Monad
type Infer a = ExceptT InferError (State Int) a

runInfer :: Infer a -> Either InferError a
runInfer m = evalState (runExceptT m) 0

fresh :: Infer Type
fresh = do
  n <- get
  put (n + 1)
  pure (TVar (letters !! n))
  where
    letters = [c : s | s <- "" : map show [1 :: Int ..], c <- ['a'..'z']]

instantiate :: Scheme -> Infer Type
instantiate (Forall vars t) = do
  freshVars <- mapM (const fresh) vars
  let s = Subst (Map.fromList (zip vars freshVars))
  pure (apply s t)

generalize :: TypeEnv -> Type -> Scheme
generalize env t = Forall vars t
  where vars = Set.toList (ftv t \`Set.difference\` ftv env)

liftUnify :: Type -> Type -> Infer Subst
liftUnify t1 t2 = case unify t1 t2 of
  Left err -> throwError err
  Right s  -> pure s

-- Algorithm W
inferExpr :: TypeEnv -> Expr -> Infer (Subst, Type)
inferExpr _   (ELit (LInt _))  = pure (emptySubst, TCon "Int")
inferExpr _   (ELit (LBool _)) = pure (emptySubst, TCon "Bool")
inferExpr env (EVar x) = case lookupEnv x env of
  Nothing -> throwError (UnboundVariable x)
  Just s  -> do { t <- instantiate s; pure (emptySubst, t) }
inferExpr env (ELam x body) = do
  tv <- fresh
  let env' = extend env (x, Forall [] tv)
  (s1, t1) <- inferExpr env' body
  pure (s1, TArr (apply s1 tv) t1)
inferExpr env (EApp fun arg) = do
  tv <- fresh
  (s1, t1) <- inferExpr env fun
  (s2, t2) <- inferExpr (apply s1 env) arg
  s3 <- liftUnify (apply s2 t1) (TArr t2 tv)
  pure (s3 \`compose\` s2 \`compose\` s1, apply s3 tv)
inferExpr env (ELet x e1 e2) = do
  (s1, t1) <- inferExpr env e1
  let env'   = apply s1 env
      scheme = generalize env' t1
      env''  = extend env' (x, scheme)
  (s2, t2) <- inferExpr env'' e2
  pure (s2 \`compose\` s1, t2)
inferExpr env (EIf cond thenE elseE) = do
  (s1, t1) <- inferExpr env cond
  (s2, t2) <- inferExpr (apply s1 env) thenE
  (s3, t3) <- inferExpr (apply (s2 \`compose\` s1) env) elseE
  s4 <- liftUnify (apply (s3 \`compose\` s2) t1) (TCon "Bool")
  s5 <- liftUnify (apply (s4 \`compose\` s3) t2) (apply s4 t3)
  pure (s5 \`compose\` s4 \`compose\` s3 \`compose\` s2 \`compose\` s1,
        apply (s5 \`compose\` s4 \`compose\` s3) t2)

infer :: Expr -> Either InferError Scheme
infer expr = runInfer $ do
  (s, t) <- inferExpr emptyEnv expr
  pure (generalize emptyEnv (apply s t))

-- Arbitrary instances for QuickCheck
instance Arbitrary Type where
  arbitrary = sized go where
    go 0 = oneof [TVar <$> elements ["a","b","c"], TCon <$> elements ["Int","Bool"]]
    go n = oneof
      [ TVar <$> elements ["a","b","c"]
      , TCon <$> elements ["Int","Bool"]
      , TArr <$> resize (n \`div\` 2) arbitrary <*> resize (n \`div\` 2) arbitrary
      ]

instance Arbitrary Subst where
  arbitrary = do
    keys <- elements [[], ["a"], ["b"], ["a","b"], ["a","b","c"]]
    vals <- mapM (const arbitrary) keys
    pure (Subst (Map.fromList (zip keys vals)))
`;

describe.runIf(GHC_AVAILABLE)(
  "E2E Extreme: Hindley-Milner Type Inference from scratch",
  () => {
    let client: Client;
    let transport: StdioClientTransport;
    let fixture: IsolatedFixture;
    let FIXTURE_DIR: string;
    let HM_MODULE: string;

    beforeAll(async () => {
      fixture = await setupIsolatedFixture("hm-project", "hm-e2e");
      FIXTURE_DIR = fixture.dir;
      HM_MODULE = path.join(FIXTURE_DIR, "src", "HM.hs");

      // Ensure src directory exists
      await mkdir(path.join(FIXTURE_DIR, "src"), { recursive: true });

      // Write the HM module from scratch
      await writeFile(HM_MODULE, HM_SOURCE, "utf-8");

      // Start MCP server pointing to the HM project
      transport = new StdioClientTransport({
        command: "node",
        args: [SERVER_SCRIPT],
        env: {
          ...process.env,
          PATH: TEST_PATH,
          HASKELL_PROJECT_DIR: FIXTURE_DIR,
          HASKELL_LIBRARY_TARGET: "lib:hm-project",
        },
      });
      client = new Client(
        { name: "hm-e2e-test", version: "0.1.0" },
        { capabilities: {} }
      );
      await client.connect(transport);
    }, 60_000);

    afterAll(async () => {
      try { await unlink(HM_MODULE); } catch { /* ignore */ }
      try { await client.close(); } catch { /* ignore */ }
      await fixture.cleanup();
    });

    // =================================================================
    // PHASE 1: Compile the generated code
    // =================================================================

    it("compiles the HM module without errors", async () => {
      const result = parseResult(
        await callTool(client, "ghci_load", {
          module_path: "src/HM.hs",
          diagnostics: true,
        })
      );
      expect(result.success).toBe(true);
      expect(result.errors).toHaveLength(0);
    });

    // =================================================================
    // PHASE 2: Verify type inference correctness with eval
    // =================================================================

    it("infers Int for integer literal", async () => {
      const r = parseResult(
        await callTool(client, "ghci_eval", {
          expression: "infer (ELit (LInt 42))",
        })
      );
      expect(r.output).toContain("TCon \"Int\"");
    });

    it("infers Bool for boolean literal", async () => {
      const r = parseResult(
        await callTool(client, "ghci_eval", {
          expression: "infer (ELit (LBool True))",
        })
      );
      expect(r.output).toContain("TCon \"Bool\"");
    });

    it("infers forall a. a -> a for identity function", async () => {
      const r = parseResult(
        await callTool(client, "ghci_eval", {
          expression: 'infer (ELam "x" (EVar "x"))',
        })
      );
      expect(r.success).toBe(true);
      expect(r.output).toContain("Forall");
      expect(r.output).toContain("TArr");
      // The type should be a -> a (same variable on both sides)
      const forallMatch = r.output.match(/Forall \["(\w+)"\] \(TArr \(TVar "\1"\) \(TVar "\1"\)\)/);
      expect(forallMatch).not.toBeNull();
    });

    it("infers Int for let-polymorphic application: let id = \\x -> x in id 42", async () => {
      const r = parseResult(
        await callTool(client, "ghci_eval", {
          expression:
            'infer (ELet "id" (ELam "x" (EVar "x")) (EApp (EVar "id") (ELit (LInt 42))))',
        })
      );
      expect(r.output).toContain("TCon \"Int\"");
    });

    it("rejects omega combinator (\\x -> x x) with InfiniteType", async () => {
      const r = parseResult(
        await callTool(client, "ghci_eval", {
          expression: 'infer (ELam "x" (EApp (EVar "x") (EVar "x")))',
        })
      );
      expect(r.output).toContain("InfiniteType");
    });

    it("rejects unbound variable", async () => {
      const r = parseResult(
        await callTool(client, "ghci_eval", {
          expression: 'infer (EVar "nope")',
        })
      );
      expect(r.output).toContain("UnboundVariable");
    });

    it("let-polymorphism: id used at both Int and Bool in same expression", async () => {
      const r = parseResult(
        await callTool(client, "ghci_eval", {
          expression:
            'infer (ELet "id" (ELam "x" (EVar "x")) ' +
            '(EIf (EApp (EVar "id") (ELit (LBool True))) ' +
            '(EApp (EVar "id") (ELit (LInt 1))) ' +
            '(ELit (LInt 2))))',
        })
      );
      expect(r.output).toContain("Right");
      expect(r.output).toContain("TCon \"Int\"");
    });

    // =================================================================
    // PHASE 3: Verify algebraic properties with QuickCheck
    // =================================================================

    it("substitution identity: apply emptySubst t == t", async () => {
      const r = parseResult(
        await callTool(client, "ghci_quickcheck", {
          property: "\\(t :: Type) -> apply emptySubst t == t",
          tests: 200,
        })
      );
      expect(r.success).toBe(true);
      expect(r.passed).toBeGreaterThanOrEqual(200);
    });

    it("substitution composition: apply (compose s1 s2) t == apply s1 (apply s2 t)", async () => {
      const r = parseResult(
        await callTool(client, "ghci_quickcheck", {
          property:
            "\\s1 s2 (t :: Type) -> apply (compose s1 s2) t == apply s1 (apply s2 t)",
          tests: 200,
        })
      );
      expect(r.success).toBe(true);
      expect(r.passed).toBeGreaterThanOrEqual(200);
    });

    it("unification correctness: unify t1 t2 = Right s => apply s t1 == apply s t2", async () => {
      const r = parseResult(
        await callTool(client, "ghci_quickcheck", {
          property:
            "\\t1 t2 -> case unify t1 t2 of { Right s -> apply s t1 == apply s (t2 :: Type); Left _ -> True }",
          tests: 200,
        })
      );
      expect(r.success).toBe(true);
      expect(r.passed).toBeGreaterThanOrEqual(200);
    });

    it("unification reflexivity: unify t t == Right emptySubst", async () => {
      const r = parseResult(
        await callTool(client, "ghci_quickcheck", {
          property: "\\(t :: Type) -> unify t t == Right emptySubst",
          tests: 200,
        })
      );
      expect(r.success).toBe(true);
      expect(r.passed).toBeGreaterThanOrEqual(200);
    });

    // =================================================================
    // PHASE 4: Verify MCP tools work correctly on HM code
    // =================================================================

    it("ghci_type returns correct type for infer", async () => {
      const r = parseResult(
        await callTool(client, "ghci_type", { expression: "infer" })
      );
      expect(r.success).toBe(true);
      expect(r.type).toContain("Expr");
      expect(r.type).toContain("Either");
      expect(r.type).toContain("Scheme");
    });

    it("ghci_type returns correct type for unify", async () => {
      const r = parseResult(
        await callTool(client, "ghci_type", { expression: "unify" })
      );
      expect(r.success).toBe(true);
      expect(r.type).toContain("Type");
      expect(r.type).toContain("Either");
    });
  }
);
