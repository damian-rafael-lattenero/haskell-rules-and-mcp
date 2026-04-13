import { describe, it, expect } from "vitest";
import { RULES_REGISTRY, loadRule } from "../resources/rules.js";

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

  it("workflow rule contains tool tiers and core loop", () => {
    const rule = RULES_REGISTRY.find(r => r.name === "haskell-mcp-workflow")!;
    expect(rule).toBeDefined();
    expect(rule.embeddedContent).toContain("TOOL TIERS");
    expect(rule.embeddedContent).toContain("PRIME DIRECTIVE");
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
    // Full version from disk has flows, tiers, etc.
    expect(content).toContain("PRIME DIRECTIVE");
    expect(content).toContain("FLOW 4");
    expect(content).toContain("Tier 1");
  });
});
