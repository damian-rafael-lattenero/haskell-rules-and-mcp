/**
 * NEW-1 coverage (Phase 5.1): `handleAnalyze` MUST load the whole project
 * (not just the target module) BEFORE the cross-module sibling probe.
 *
 * Regression guard for the runtime bug discovered in session 3: the Phase 4
 * cross-module fix (unioning defs from `:show modules`) was defeated by an
 * earlier `session.loadModule(target)` in `replace` mode, which dropped every
 * other module from scope. The probe then saw only the target, and
 * cross-module engines (evaluator-preservation, constant-folding-soundness,
 * functor-laws) never fired in production flows — their unit tests passed
 * but they were dark in real usage.
 *
 * The fix: call `session.loadModules(paths, names)` (load_all) with the
 * paths parsed from `.cabal`, so the probe sees every library module.
 */
import { describe, it, expect, vi, beforeEach, afterEach } from "vitest";
import { mkdtemp, writeFile, rm, mkdir } from "node:fs/promises";
import { tmpdir } from "node:os";
import path from "node:path";

import { handleAnalyze } from "../tools/suggest.js";
import { createMockSession } from "./helpers/mock-session.js";
import type { GhciSession } from "../ghci-session.js";

// Minimal cabal scaffold so parseCabalModules returns a non-empty library.
async function makeCabalProject(): Promise<string> {
  const root = await mkdtemp(path.join(tmpdir(), "cross-module-"));
  await mkdir(path.join(root, "src", "Expr"), { recursive: true });
  await writeFile(
    path.join(root, "minimal.cabal"),
    [
      "cabal-version:      2.4",
      "name:               minimal",
      "version:            0.1",
      "library",
      "  exposed-modules:  Expr.Syntax Expr.Eval Expr.Simplify",
      "  hs-source-dirs:   src",
      "  build-depends:    base >= 4.20",
      "  default-language: GHC2024",
    ].join("\n"),
    "utf-8"
  );
  // Dummy source files so moduleToFilePath has real targets to reference.
  for (const m of ["Syntax", "Eval", "Simplify"]) {
    await writeFile(
      path.join(root, "src", "Expr", `${m}.hs`),
      `module Expr.${m} where\n`,
      "utf-8"
    );
  }
  return root;
}

describe("handleAnalyze cross-module load (NEW-1)", () => {
  let projectDir: string;

  beforeEach(async () => {
    projectDir = await makeCabalProject();
  });
  afterEach(async () => {
    await rm(projectDir, { recursive: true, force: true });
  });

  it("calls session.loadModules with every cabal library module before probing siblings", async () => {
    const session = createMockSession({
      // `:browse Expr.Simplify` → one function
      execute: async (cmd: unknown) => {
        const c = String(cmd);
        if (c.startsWith(":browse")) {
          return {
            success: true,
            output: "simplify :: Expr -> Expr\n",
          };
        }
        if (c === ":show modules") {
          return {
            success: true,
            output:
              "Expr.Syntax    ( src/Expr/Syntax.hs, interpreted )\n" +
              "Expr.Eval      ( src/Expr/Eval.hs, interpreted )\n" +
              "Expr.Simplify  ( src/Expr/Simplify.hs, interpreted )\n",
          };
        }
        return { success: true, output: "" };
      },
    });

    await handleAnalyze(session, "src/Expr/Simplify.hs", projectDir);

    const lm = session.loadModules as unknown as ReturnType<typeof vi.fn>;
    expect(lm).toHaveBeenCalledTimes(1);
    const [paths, modules] = lm.mock.calls[0]!;
    expect(paths).toEqual(
      expect.arrayContaining([
        expect.stringContaining("Expr/Syntax.hs"),
        expect.stringContaining("Expr/Eval.hs"),
        expect.stringContaining("Expr/Simplify.hs"),
      ])
    );
    expect(modules).toEqual(["Expr.Syntax", "Expr.Eval", "Expr.Simplify"]);
  });

  it("falls back to single-module load when no cabal exists", async () => {
    const rootNoCabal = await mkdtemp(path.join(tmpdir(), "no-cabal-"));
    try {
      const session = createMockSession({
        execute: async (cmd: unknown) => {
          const c = String(cmd);
          if (c.startsWith(":browse")) {
            return { success: true, output: "f :: Int -> Int\n" };
          }
          return { success: true, output: "" };
        },
      });

      await handleAnalyze(session, "src/Foo.hs", rootNoCabal);

      // No cabal → loadModules MUST NOT be called; loadModule is the fallback.
      expect((session.loadModules as unknown as ReturnType<typeof vi.fn>).mock.calls.length).toBe(0);
      expect(
        (session.loadModule as unknown as ReturnType<typeof vi.fn>).mock.calls.length
      ).toBeGreaterThanOrEqual(1);
    } finally {
      await rm(rootNoCabal, { recursive: true, force: true });
    }
  });

  it("surfaces a clean error when loadModules fails", async () => {
    const session: GhciSession = createMockSession({
      loadModules: {
        success: false,
        output: "Module `Expr.Eval' has a syntax error",
      },
    });

    const raw = await handleAnalyze(session, "src/Expr/Simplify.hs", projectDir);
    const parsed = JSON.parse(raw);
    expect(parsed.success).toBe(false);
    expect(parsed.mode).toBe("analyze");
    expect(parsed.error).toContain("Could not load project modules");
  });

  it("still returns success when :browse on the target finds functions after load_all", async () => {
    const session = createMockSession({
      execute: async (cmd: unknown) => {
        const c = String(cmd);
        if (c === ":browse Expr.Simplify") {
          return { success: true, output: "simplify :: Expr -> Expr\n" };
        }
        if (c === ":show modules") {
          return { success: true, output: "Expr.Simplify ( src/Expr/Simplify.hs, interpreted )\n" };
        }
        return { success: true, output: "" };
      },
    });

    const raw = await handleAnalyze(session, "src/Expr/Simplify.hs", projectDir);
    const parsed = JSON.parse(raw);
    expect(parsed.success).toBe(true);
    expect(parsed.mode).toBe("analyze");
    expect(parsed.functions).toHaveLength(1);
    expect(parsed.functions[0].name).toBe("simplify");
  });
});
