/**
 * Integration test: when a real GHCi session + a cabal project expose a
 * function whose type shape is recognized by NONE of the registered law
 * engines, `handleAnalyze` must now surface the generic fallback
 * suggestions + guidance rather than returning an empty result.
 *
 * We pick `lookup :: Env -> String -> Either Error Int` — three levels
 * (context → input → Either result) — because that shape is deliberately
 * outside every engine's pattern. If the engines grow in the future to
 * recognize this shape, this test will still pass (engine suggestions
 * are additive), and `suggest-fallback.test.ts` will start skipping the
 * "does NOT emit fallback when engine matched" branch for it.
 */
import { describe, it, expect, beforeAll, afterAll } from "vitest";
import { execSync } from "node:child_process";
import { mkdtemp, writeFile, mkdir, rm } from "node:fs/promises";
import { tmpdir } from "node:os";
import path from "node:path";

import { GhciSession } from "../../ghci-session.js";
import { handleAnalyze } from "../../tools/suggest.js";

const GHCUP_BIN = path.join(process.env.HOME ?? "", ".ghcup", "bin");
const TEST_PATH = `${GHCUP_BIN}:${process.env.PATH ?? ""}`;

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

describe.skipIf(!GHC_AVAILABLE)("handleAnalyze fallback — real GHCi integration", () => {
  let projectDir: string;
  let session: GhciSession;

  beforeAll(async () => {
    projectDir = await mkdtemp(path.join(tmpdir(), "suggest-fallback-int-"));
    await mkdir(path.join(projectDir, "src"), { recursive: true });

    await writeFile(
      path.join(projectDir, "pkg.cabal"),
      [
        "cabal-version:      2.4",
        "name:               pkg",
        "version:            0.1",
        "library",
        "  exposed-modules:  Lookup",
        "  hs-source-dirs:   src",
        "  build-depends:    base >= 4.20",
        "  default-language: GHC2024",
      ].join("\n"),
      "utf-8"
    );
    // Three-arrow signature `Env -> String -> Either String Int` — no
    // engine recognizes this shape, so the pure engine layer returns [].
    // The fallback path is what we are exercising end-to-end.
    await writeFile(
      path.join(projectDir, "src", "Lookup.hs"),
      [
        "module Lookup where",
        "",
        "type Env = [(String, Int)]",
        "",
        "lookupVar :: Env -> String -> Either String Int",
        "lookupVar env k = case lookup k env of",
        "  Just v  -> Right v",
        "  Nothing -> Left (\"unbound: \" ++ k)",
        "",
      ].join("\n"),
      "utf-8"
    );

    session = new GhciSession(projectDir);
    await session.start();
  }, 180_000);

  afterAll(async () => {
    try { await session?.kill(); } catch { /* ignore */ }
    await rm(projectDir, { recursive: true, force: true });
  });

  it("returns _fallbackSuggestions + _guidance when no engine matches", async () => {
    const raw = await handleAnalyze(session, "src/Lookup.hs", projectDir);
    const result = JSON.parse(raw);

    expect(result.success).toBe(true);

    // The registered engines don't match `Env -> String -> Either String Int`,
    // so the per-function suggestedProperties array stays empty.
    const lookupFn = result.functions.find(
      (f: { name: string }) => f.name === "lookupVar"
    );
    expect(lookupFn).toBeDefined();
    expect(lookupFn.suggestedProperties).toEqual([]);

    // Fallback fires: determinism is always emitted; error-propagation is
    // emitted here because the result type contains `Either`.
    expect(result._fallbackSuggestions).toBeDefined();
    const fallbacks = result._fallbackSuggestions as Array<{
      function: string;
      law: string;
      property: string;
    }>;
    expect(
      fallbacks.some((f) => f.function === "lookupVar" && f.law === "determinism")
    ).toBe(true);
    expect(
      fallbacks.some(
        (f) =>
          f.function === "lookupVar" && f.law.includes("error propagation")
      )
    ).toBe(true);

    // Guidance explicitly mentions the zero-match situation so an agent
    // branches on text if it doesn't parse structure.
    const guidance = (result._guidance ?? []) as string[];
    expect(guidance.some((g) => /Zero engine matches/.test(g))).toBe(true);
  }, 60_000);
});
