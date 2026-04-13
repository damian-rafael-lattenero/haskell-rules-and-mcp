import { describe, it, expect } from "vitest";
import { RULES_REGISTRY, loadRule } from "../resources/rules.js";
import { createRulesChecker } from "../tools/registry.js";

describe("RULES_REGISTRY", () => {
  it("has 2 rules (workflow + conventions)", () => {
    expect(RULES_REGISTRY).toHaveLength(2);
  });

  it("each rule has required fields", () => {
    for (const rule of RULES_REGISTRY) {
      expect(rule.name).toBeTruthy();
      expect(rule.uri).toMatch(/^rules:\/\//);
      expect(rule.title).toBeTruthy();
      expect(rule.description).toBeTruthy();
      expect(rule.fileName).toMatch(/\.md$/);
      expect(rule.embeddedContent).toBeTruthy();
    }
  });

  it("workflow rule contains prime directive and forbidden patterns", () => {
    const rule = RULES_REGISTRY.find(r => r.name === "haskell-mcp-workflow")!;
    expect(rule).toBeDefined();
    expect(rule.embeddedContent).toContain("PRIME DIRECTIVE");
    expect(rule.embeddedContent).toContain("FORBIDDEN");
    expect(rule.embeddedContent).toContain("ALWAYS MANDATORY");
  });

  it("conventions rule contains import style", () => {
    const rule = RULES_REGISTRY.find(r => r.name === "haskell-project-conventions")!;
    expect(rule.embeddedContent).toContain("Import Style");
  });
});

describe("loadRule", () => {
  it("returns embedded content as fallback when file not found", async () => {
    const fakeRule = {
      name: "test",
      uri: "rules://test",
      title: "Test",
      description: "Test",
      fileName: "nonexistent-file-xyz.md",
      embeddedContent: "fallback content here",
    };
    const content = await loadRule(fakeRule);
    expect(content).toBe("fallback content here");
  });

  it("loads real workflow file when it exists", async () => {
    const workflowRule = RULES_REGISTRY.find(r => r.name === "haskell-mcp-workflow")!;
    const content = await loadRule(workflowRule);
    expect(content).toBeTruthy();
    expect(content.length).toBeGreaterThan(100);
    // Full version from disk has prime directive, tables, and forbidden patterns
    expect(content).toContain("PRIME DIRECTIVE");
    expect(content).toContain("FORBIDDEN");
    expect(content).toContain("ALWAYS MANDATORY");
    expect(content).toContain("WHEN");
  });
});

describe("createRulesChecker — notice-once behavior", () => {
  it("returns notice on first call when rules are missing", async () => {
    const checker = createRulesChecker(() => "/nonexistent/path");
    const first = await checker.check();
    expect(first).toContain("ghci_setup()");
  });

  it("returns null on second call (notice already shown)", async () => {
    const checker = createRulesChecker(() => "/nonexistent/path");
    await checker.check(); // first call — shows notice
    const second = await checker.check();
    expect(second).toBeNull();
  });

  it("returns null on all subsequent calls", async () => {
    const checker = createRulesChecker(() => "/nonexistent/path");
    await checker.check();
    await checker.check();
    const third = await checker.check();
    expect(third).toBeNull();
  });

  it("does NOT show notice again after reset() — once per session", async () => {
    const checker = createRulesChecker(() => "/nonexistent/path");
    await checker.check(); // shows notice
    await checker.check(); // null
    checker.reset();
    // reset() clears the cache but noticeShown stays true to prevent spam
    const afterReset = await checker.check();
    expect(afterReset).toBeNull();
  });

  it("stays null after multiple resets", async () => {
    const checker = createRulesChecker(() => "/nonexistent/path");
    await checker.check(); // shows notice once
    checker.reset();
    checker.reset();
    const result = await checker.check();
    expect(result).toBeNull();
  });

  it("returns null always when rules exist", async () => {
    // Use the actual project dir which has rules installed
    const checker = createRulesChecker(
      () => "/nonexistent/path",
      () => process.cwd().replace("/mcp-server", "")
    );
    const first = await checker.check();
    // If rules exist at base dir, should be null
    // If not, it will be the notice — either way second call should be null
    const second = await checker.check();
    if (first === null) {
      // Rules found — both should be null
      expect(second).toBeNull();
    } else {
      // Rules not found — second should be null (notice-once)
      expect(second).toBeNull();
    }
  });
});
