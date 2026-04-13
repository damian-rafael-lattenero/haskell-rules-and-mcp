import { describe, it, expect, afterEach } from "vitest";
import { handleSuggest } from "../tools/suggest.js";
import { createMockSession } from "./helpers/mock-session.js";
import { writeFile, readFile, mkdtemp, rm, mkdir } from "node:fs/promises";
import path from "node:path";
import os from "node:os";
import type { GhciResult } from "../ghci-session.js";

describe("handleSuggest", () => {
  const tmpDirs: string[] = [];

  async function makeTmpDir(): Promise<string> {
    const dir = await mkdtemp(path.join(os.tmpdir(), "suggest-test-"));
    tmpDirs.push(dir);
    return dir;
  }

  afterEach(async () => {
    for (const dir of tmpDirs) {
      try {
        await rm(dir, { recursive: true });
      } catch {
        /* ignore */
      }
    }
    tmpDirs.length = 0;
  });

  it("switches to analyze mode for module without undefined stubs", async () => {
    const dir = await makeTmpDir();
    const srcDir = path.join(dir, "src");
    await mkdir(srcDir, { recursive: true });
    await writeFile(
      path.join(srcDir, "Clean.hs"),
      "module Clean where\n\nfoo :: Int\nfoo = 42\n",
      "utf-8"
    );

    const session = createMockSession({
      execute: async (cmd: string): Promise<GhciResult> => {
        if (typeof cmd === "string" && cmd.startsWith(":browse")) {
          return {
            output: "foo :: Int",
            success: true,
          };
        }
        return { output: "", success: true };
      },
      loadModule: { output: "Ok, one module loaded.", success: true },
    });

    const result = JSON.parse(
      await handleSuggest(session, { module_path: "src/Clean.hs" }, dir)
    );
    expect(result.success).toBe(true);
    expect(result.mode).toBe("analyze");
    expect(result.functions).toBeDefined();
    expect(result.functions.length).toBeGreaterThan(0);
    expect(result.functions[0].name).toBe("foo");
  });

  it("finds undefined functions", async () => {
    const dir = await makeTmpDir();
    const srcDir = path.join(dir, "src");
    await mkdir(srcDir, { recursive: true });
    await writeFile(
      path.join(srcDir, "Foo.hs"),
      [
        "module Foo where",
        "",
        "foo :: Int -> Int",
        "foo = undefined",
        "",
        "bar :: String -> Bool",
        "bar = undefined",
        "",
      ].join("\n"),
      "utf-8"
    );

    const holeOutput =
      "src/Foo.hs:4:7: warning: [GHC-88464] [-Wtyped-holes]\n" +
      "    \u2022 Found hole: _ :: Int -> Int\n" +
      "    \u2022 In an equation for 'foo': foo = _\n" +
      "    \u2022 Relevant bindings include\n" +
      "        foo :: Int -> Int (bound at src/Foo.hs:4:1)\n" +
      "      Valid hole fits include\n" +
      "        id :: forall a. a -> a\n" +
      "          with id @Int\n" +
      "          (imported from 'Prelude' at src/Foo.hs:1:8-16)\n" +
      "   |\n" +
      "4 | foo = _\n" +
      "   |       ^\n" +
      "src/Foo.hs:7:7: warning: [GHC-88464] [-Wtyped-holes]\n" +
      "    \u2022 Found hole: _ :: String -> Bool\n" +
      "    \u2022 In an equation for 'bar': bar = _\n" +
      "    \u2022 Relevant bindings include\n" +
      "        bar :: String -> Bool (bound at src/Foo.hs:7:1)\n" +
      "      Valid hole fits include\n" +
      "        null :: forall a. [a] -> Bool\n" +
      "          with null @[] @Char\n" +
      "          (imported from 'Prelude' at src/Foo.hs:1:8-16)\n" +
      "   |\n" +
      "7 | bar = _\n" +
      "   |       ^\n" +
      "Ok, one module loaded.";

    let loadCount = 0;
    const session = createMockSession({
      execute: async (): Promise<GhciResult> => ({ output: "", success: true }),
      loadModule: async (): Promise<GhciResult> => {
        loadCount++;
        if (loadCount === 1) {
          // Pre-check: module compiles fine with = undefined
          return { output: "Ok, one module loaded.", success: true };
        }
        if (loadCount === 2) {
          // Second load: holes output
          return { output: holeOutput, success: true };
        }
        // Final reload after restore
        return { output: "Ok, one module loaded.", success: true };
      },
    });

    const result = JSON.parse(
      await handleSuggest(session, { module_path: "src/Foo.hs" }, dir)
    );
    expect(result.success).toBe(true);
    expect(result.suggestions).toHaveLength(2);

    expect(result.suggestions[0].function).toBe("foo");
    expect(result.suggestions[0].line).toBe(4);
    expect(result.suggestions[0].expectedType).toBe("Int -> Int");
    expect(result.suggestions[0].validFits.length).toBeGreaterThan(0);

    expect(result.suggestions[1].function).toBe("bar");
    expect(result.suggestions[1].line).toBe(7);
    expect(result.suggestions[1].expectedType).toBe("String -> Bool");
  });

  it("returns error for non-existent file", async () => {
    const dir = await makeTmpDir();

    const session = createMockSession({
      execute: async (): Promise<GhciResult> => ({ output: "", success: true }),
      loadModule: { output: "Ok, one module loaded.", success: true },
    });

    const result = JSON.parse(
      await handleSuggest(session, { module_path: "src/NoExiste.hs" }, dir)
    );
    expect(result.success).toBe(false);
    expect(result.error).toContain("File not found");
  });

  it("returns error for module with compile errors", async () => {
    const dir = await makeTmpDir();
    const srcDir = path.join(dir, "src");
    await mkdir(srcDir, { recursive: true });
    await writeFile(
      path.join(srcDir, "Bad.hs"),
      [
        "module Bad where",
        "",
        "foo :: Int",
        "foo = undefined",
        "",
        "bar :: Int",
        'bar = "not an int"',
        "",
      ].join("\n"),
      "utf-8"
    );

    const errorOutput =
      "src/Bad.hs:7:7: error: [GHC-83865]\n" +
      "    \u2022 Couldn\u2019t match type \u2018[Char]\u2019 with \u2018Int\u2019\n" +
      "    \u2022 In the expression: \"not an int\"\n" +
      "Failed, no modules loaded.";

    const session = createMockSession({
      execute: async (): Promise<GhciResult> => ({ output: "", success: true }),
      loadModule: { output: errorOutput, success: false },
    });

    const result = JSON.parse(
      await handleSuggest(session, { module_path: "src/Bad.hs" }, dir)
    );
    expect(result.success).toBe(false);
    expect(result.error).toContain("compile errors");
    expect(result.errors.length).toBeGreaterThan(0);
  });

  it("restores original file after suggest", async () => {
    const dir = await makeTmpDir();
    const srcDir = path.join(dir, "src");
    await mkdir(srcDir, { recursive: true });
    const originalContent = [
      "module Restore where",
      "",
      "foo :: Int",
      "foo = undefined",
      "",
    ].join("\n");
    const filePath = path.join(srcDir, "Restore.hs");
    await writeFile(filePath, originalContent, "utf-8");

    let loadCount = 0;
    const session = createMockSession({
      execute: async (): Promise<GhciResult> => ({ output: "", success: true }),
      loadModule: async (): Promise<GhciResult> => {
        loadCount++;
        if (loadCount === 1) {
          // Pre-check succeeds
          return { output: "Ok, one module loaded.", success: true };
        }
        if (loadCount === 2) {
          // Hole load — return minimal hole output
          return {
            output:
              "src/Restore.hs:4:7: warning: [GHC-88464] [-Wtyped-holes]\n" +
              "    \u2022 Found hole: _ :: Int\n" +
              "    \u2022 In an equation for 'foo': foo = _\n" +
              "      Valid hole fits include\n" +
              "        maxBound :: forall a. Bounded a => a\n" +
              "   |\n" +
              "4 | foo = _\n" +
              "   |       ^\n" +
              "Ok, one module loaded.",
            success: true,
          };
        }
        // Final reload after restore
        return { output: "Ok, one module loaded.", success: true };
      },
    });

    await handleSuggest(session, { module_path: "src/Restore.hs" }, dir);

    const afterContent = await readFile(filePath, "utf-8");
    expect(afterContent).toBe(originalContent);
  });
});
