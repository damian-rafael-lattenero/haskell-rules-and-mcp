/**
 * Unit tests for `detectFormatterConfig` — reports whether a project
 * pins a fourmolu/ormolu config or is using the tool's defaults.
 *
 * Why this exists: during the expr-evaluator dogfood session the
 * formatter reflowed export lists and rewrote `-- |` haddock comments
 * into `{- | -}` block form because we had no project config to pin a
 * style. That was surprising from the agent's perspective — the MCP
 * appeared to "decide" style without asking. Surfacing `configSource`
 * lets agents/users know when the defaults are in play and act before
 * committing a formatting PR.
 */
import { describe, it, expect, beforeEach, afterEach } from "vitest";
import { mkdtemp, writeFile, rm } from "node:fs/promises";
import os from "node:os";
import path from "node:path";
import { detectFormatterConfig } from "../tools/format.js";

describe("detectFormatterConfig", () => {
  let projectDir: string;

  beforeEach(async () => {
    projectDir = await mkdtemp(path.join(os.tmpdir(), "format-config-"));
  });

  afterEach(async () => {
    await rm(projectDir, { recursive: true, force: true });
  });

  it("reports 'defaults' + actionable hint when no config file is present", async () => {
    const info = await detectFormatterConfig(projectDir, "/usr/bin/fourmolu");
    expect(info.source).toBe("defaults");
    expect(info.configPath).toBeUndefined();
    expect(info.hint).toBeDefined();
    // The hint must include the one-liner to write a config, so an agent
    // or user reading the JSON can act on it without web-searching.
    expect(info.hint).toMatch(/fourmolu --print-default-config > fourmolu\.yaml/);
  });

  it("reports 'project' + path when fourmolu.yaml exists at root", async () => {
    const cfgPath = path.join(projectDir, "fourmolu.yaml");
    await writeFile(cfgPath, "indentation: 4\n", "utf-8");

    const info = await detectFormatterConfig(projectDir, "/usr/bin/fourmolu");
    expect(info.source).toBe("project");
    expect(info.configPath).toBe(cfgPath);
    // No hint on the project path — nothing to prompt about.
    expect(info.hint).toBeUndefined();
  });

  it("accepts the dotfile variant `.fourmolu.yaml`", async () => {
    const cfgPath = path.join(projectDir, ".fourmolu.yaml");
    await writeFile(cfgPath, "indentation: 2\n", "utf-8");

    const info = await detectFormatterConfig(projectDir, "/usr/bin/fourmolu");
    expect(info.source).toBe("project");
    expect(info.configPath).toBe(cfgPath);
  });

  it("prefers the non-dotfile when both exist (alphabetical stability)", async () => {
    const visible = path.join(projectDir, "fourmolu.yaml");
    const hidden = path.join(projectDir, ".fourmolu.yaml");
    await writeFile(visible, "indentation: 4\n", "utf-8");
    await writeFile(hidden, "indentation: 2\n", "utf-8");

    const info = await detectFormatterConfig(projectDir, "/usr/bin/fourmolu");
    expect(info.configPath).toBe(visible);
  });

  it("uses the formatter binary name in the hint (fourmolu vs ormolu)", async () => {
    const fourmolu = await detectFormatterConfig(
      projectDir,
      "/opt/tools/fourmolu"
    );
    expect(fourmolu.hint).toContain("fourmolu --print-default-config");

    const ormolu = await detectFormatterConfig(projectDir, "/opt/tools/ormolu");
    expect(ormolu.hint).toContain("ormolu --print-default-config");
  });

  it("handles bundled binary paths (basename extraction works)", async () => {
    const info = await detectFormatterConfig(
      projectDir,
      "/Users/x/vendor-tools/fourmolu/darwin-arm64/fourmolu"
    );
    expect(info.source).toBe("defaults");
    expect(info.hint).toMatch(/fourmolu --print-default-config/);
  });
});
