/**
 * Unit tests for the "no engine matched" fallback on
 * `ghci_suggest(mode=analyze)`. The goal: when zero law engines
 * recognize a function's type shape (common for `Env -> Input ->
 * Either Error Output` style signatures), the tool should still return
 * ACTIONABLE guidance — not an empty `suggestedProperties` array with
 * a generic "write your own" message.
 */
import { describe, it, expect } from "vitest";
import { mkdtemp, writeFile, rm, mkdir } from "node:fs/promises";
import { tmpdir } from "node:os";
import path from "node:path";
import { handleAnalyze } from "../tools/suggest.js";
import { createMockSession } from "./helpers/mock-session.js";

/**
 * Build a minimal cabal project whose browse output returns a single
 * function whose type doesn't match any engine.
 */
async function projectWith(
  moduleName: string,
  functions: Array<{ name: string; type: string }>
): Promise<string> {
  const root = await mkdtemp(path.join(tmpdir(), "suggest-fallback-"));
  await mkdir(path.join(root, "src"), { recursive: true });
  await writeFile(
    path.join(root, "pkg.cabal"),
    [
      "cabal-version:      2.4",
      "name:               pkg",
      "version:            0.1",
      "library",
      `  exposed-modules:  ${moduleName}`,
      "  hs-source-dirs:   src",
      "  build-depends:    base >= 4.20",
      "  default-language: GHC2024",
    ].join("\n"),
    "utf-8"
  );
  await writeFile(
    path.join(root, "src", `${moduleName}.hs`),
    `module ${moduleName} where\n`,
    "utf-8"
  );
  void functions; // Arbitrary is not referenced here; browse output synth'd in mock.
  return root;
}

function mockSessionBrowse(browseOutput: string): ReturnType<typeof createMockSession> {
  return createMockSession({
    execute: async (cmd: unknown) => {
      const c = String(cmd);
      if (c.startsWith(":browse")) {
        return { success: true, output: browseOutput };
      }
      if (c === ":show modules") {
        return { success: true, output: "" };
      }
      return { success: true, output: "" };
    },
    loadModules: async () => ({ success: true, output: "" }),
    loadModule: async () => ({ success: true, output: "" }),
  });
}

describe("handleAnalyze fallback — zero-engine-match case", () => {
  it("emits determinism + error-propagation suggestions when no engine matches `Env -> Expr -> Either Error Int`", async () => {
    const projectDir = await projectWith("Eval", [
      { name: "eval", type: "Env -> Expr -> Either Error Int" },
    ]);
    const session = mockSessionBrowse(
      "eval :: Env -> Expr -> Either Error Int\n"
    );

    const raw = await handleAnalyze(session, "src/Eval.hs", projectDir);
    const result = JSON.parse(raw);

    // Engine side returns nothing — that's the premise of this test.
    expect(result.functions[0].suggestedProperties).toHaveLength(0);

    // But the fallback kicks in.
    expect(result._fallbackSuggestions).toBeDefined();
    const fallbacks = result._fallbackSuggestions as Array<{
      function: string;
      law: string;
      property: string;
    }>;
    expect(fallbacks.length).toBeGreaterThanOrEqual(2);

    const determinism = fallbacks.find((f) => f.law === "determinism");
    expect(determinism).toBeDefined();
    expect(determinism!.function).toBe("eval");
    // 2-arg function ⇒ lambda binds two args
    expect(determinism!.property).toMatch(/\\a1 a2 -> eval a1 a2 == eval a1 a2/);

    const errProp = fallbacks.find((f) =>
      f.law.includes("error propagation")
    );
    expect(errProp).toBeDefined();
    // Result type is Either → the shape gets picked up.
    expect(errProp!.property).toMatch(/case eval a1 a2 of/);

    // Guidance explains why — the LLM should not parrot "just write it".
    expect(result._guidance).toBeDefined();
    const guidance = result._guidance as string[];
    expect(guidance.some((g) => g.includes("Zero engine matches"))).toBe(true);
    expect(guidance.some((g) => g.includes("determinism"))).toBe(true);

    await rm(projectDir, { recursive: true, force: true });
  });

  it("does NOT emit a fallback when an engine matched at least one function", async () => {
    // `simplify :: Expr -> Expr` matches the endomorphism engine, so
    // `suggestFunctionProperties` will return at least one law. The
    // fallback pathway must stay quiet in that case.
    const projectDir = await projectWith("Simplify", [
      { name: "simplify", type: "Expr -> Expr" },
    ]);
    const session = mockSessionBrowse("simplify :: Expr -> Expr\n");

    const raw = await handleAnalyze(session, "src/Simplify.hs", projectDir);
    const result = JSON.parse(raw);

    // Endomorphism engine fires → at least one suggestion.
    expect(result.functions[0].suggestedProperties.length).toBeGreaterThan(0);

    expect(result._fallbackSuggestions).toBeUndefined();
    expect(result._guidance).toBeUndefined();

    await rm(projectDir, { recursive: true, force: true });
  });

  it("fallback omits error-propagation for non-Either/Maybe return types", async () => {
    const projectDir = await projectWith("Render", [
      { name: "render", type: "Env -> Doc -> String" },
    ]);
    const session = mockSessionBrowse("render :: Env -> Doc -> String\n");

    const raw = await handleAnalyze(session, "src/Render.hs", projectDir);
    const result = JSON.parse(raw);

    const fallbacks = (result._fallbackSuggestions ?? []) as Array<{
      law: string;
    }>;
    expect(fallbacks.some((f) => f.law === "determinism")).toBe(true);
    expect(
      fallbacks.some((f) => f.law.includes("error propagation"))
    ).toBe(false);

    await rm(projectDir, { recursive: true, force: true });
  });

  it("fallback is silent when module has zero functions (nothing to suggest)", async () => {
    const projectDir = await projectWith("Empty", []);
    const session = mockSessionBrowse(""); // no browse output

    const raw = await handleAnalyze(session, "src/Empty.hs", projectDir);
    const result = JSON.parse(raw);

    expect(result._fallbackSuggestions).toBeUndefined();

    await rm(projectDir, { recursive: true, force: true });
  });

  it("determinism lambda arity matches the function's top-level arrows", async () => {
    // Higher-order function — `(a -> b) -> [a] -> [b]` has arity 2 at the
    // top level, so the fallback should generate a 2-arg lambda.
    const projectDir = await projectWith("HO", [
      { name: "mapish", type: "(a -> b) -> [a] -> [b]" },
    ]);
    const session = mockSessionBrowse("mapish :: (a -> b) -> [a] -> [b]\n");

    const raw = await handleAnalyze(session, "src/HO.hs", projectDir);
    const result = JSON.parse(raw);
    const fallbacks = result._fallbackSuggestions as Array<{ property: string }>;
    const det = fallbacks.find((f) => f.property.includes("mapish"));
    expect(det!.property).toMatch(/\\a1 a2 -> mapish a1 a2 == mapish a1 a2/);

    await rm(projectDir, { recursive: true, force: true });
  });
});
